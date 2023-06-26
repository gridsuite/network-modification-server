/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.impacts.BaseImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact.CollectionImpactType;
import org.springframework.context.ApplicationContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class GeneratorScaling extends AbstractScaling {

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        super(generatorScalableInfos);
    }

    @Override
    public Set<BaseImpact> apply(Network network, Reporter subReporter, ApplicationContext context) {
        super.apply(network, subReporter, context);
        // must return a Collection Impact here
        Set<BaseImpact> impacts = new HashSet<>();
        impacts.add(CollectionElementImpact.builder()
            .impactType(CollectionImpactType.COLLECTION)
            .elementType(IdentifiableType.GENERATOR)
            .build()
        );
        return impacts;
    }

    @Override
    protected void applyStackingUpVariation(Network network,
                                         Reporter subReporter,
                                         List<IdentifiableAttributes> identifiableAttributes,
                                         ScalingVariationInfos generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Scalable stackingUpScalable = Scalable.stack(identifiableAttributes.stream()
                .map(equipment -> {
                    sum.set(network.getGenerator(equipment.getId()).getTargetP() + sum.get());
                    return getScalable(equipment.getId());
                }).toArray(Scalable[]::new));
        scale(network, subReporter, generatorScalingVariation, sum, stackingUpScalable);
    }

    @Override
    protected void applyVentilationVariation(Network network,
                                          Reporter subReporter,
                                          List<IdentifiableAttributes> identifiableAttributes,
                                          ScalingVariationInfos generatorScalingVariation,
                                          Double distributionKeys) {
        if (distributionKeys != null) {
            AtomicReference<Double> sum = new AtomicReference<>(0D);
            List<Float> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            identifiableAttributes.forEach(equipment -> {
                sum.set(network.getGenerator(equipment.getId()).getTargetP() + sum.get());
                scalables.add(getScalable(equipment.getId()));
                percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            scale(network, subReporter, generatorScalingVariation, sum, ventilationScalable);
        }
    }

    @Override
    protected void applyRegularDistributionVariation(Network network,
                                                  Reporter subReporter,
                                                  List<IdentifiableAttributes> identifiableAttributes,
                                                  ScalingVariationInfos generatorScalingVariation) {
        List<Generator> generators = identifiableAttributes
                .stream()
                .map(attribute -> network.getGenerator(attribute.getId()))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        AtomicReference<Double> sum = new AtomicReference<>(0D);

        List<Scalable> scalables = generators.stream()
                .map(generator -> {
                    sum.set(sum.get() + generator.getTargetP());
                    return getScalable(generator.getId());
                }).collect(Collectors.toList());

        List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), (float) (100.0 / scalables.size())));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReporter, generatorScalingVariation, sum, regularDistributionScalable);
    }

    @Override
    protected void applyProportionalToPmaxVariation(Network network,
                                                 Reporter subReporter,
                                                 List<IdentifiableAttributes> identifiableAttributes,
                                                 ScalingVariationInfos generatorScalingVariation) {
        AtomicReference<Double> maxPSum = new AtomicReference<>(0D);
        AtomicReference<Double> targetPSum = new AtomicReference<>(0D);
        List<Generator> generators = identifiableAttributes
                .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
        Map<String, Double> maxPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();

        // we retrieve max P and the sum of max P of each generator to calculate the percentage.
        // we calculate the sum of target P to calculate variation value if variation type is Target_P
        generators.forEach(generator -> {
            maxPMap.put(generator.getId(), generator.getMaxP());
            maxPSum.set(maxPSum.get() + generator.getMaxP());
            targetPSum.set(targetPSum.get() + generator.getTargetP());
        });

        setScalablePercentage(maxPSum, maxPMap, percentages, scalables);
        Scalable proportionalToPmaxScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReporter, generatorScalingVariation, targetPSum, proportionalToPmaxScalable);
    }

    @Override
    protected void applyProportionalVariation(Network network,
                                           Reporter subReporter,
                                           List<IdentifiableAttributes> identifiableAttributes,
                                           ScalingVariationInfos generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Generator> generators = identifiableAttributes
                .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
        List<Float> percentages = new ArrayList<>();
        Map<String, Double> targetPMap = new HashMap<>();
        List<Scalable> scalables = new ArrayList<>();

        // we retrieve the target P for every generator and calculate their sum
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getTargetP());
            sum.set(sum.get() + generator.getTargetP());
        });

        // we calculate percentage of each target P value relative to the sum of target P
        setScalablePercentage(sum, targetPMap, percentages, scalables);
        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReporter, generatorScalingVariation, sum, proportionalScalable);
    }

    private void setScalablePercentage(AtomicReference<Double> sum,
                                       Map<String, Double> targetPMap,
                                       List<Float> percentages,
                                       List<Scalable> scalables) {
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });
    }

    private void scale(Network network, Reporter subReporter, ScalingVariationInfos scalingVariationInfos, AtomicReference<Double> sum, Scalable scalable) {
        double asked = getAsked(scalingVariationInfos, sum);
        double done = scalable.scale(network, asked);
        createReport(subReporter,
                "scalingApplied",
                String.format("successfully scaled for mode %s with variation value asked is %s. variation done is  %s", scalingVariationInfos.getVariationMode(), asked, done),
                TypedValue.INFO_SEVERITY);
    }

    @Override
    protected double getAsked(ScalingVariationInfos generatorScalingVariation, AtomicReference<Double> sum) {
        return scalingInfos.getVariationType() == VariationType.DELTA_P
                ? generatorScalingVariation.getVariationValue()
                : generatorScalingVariation.getVariationValue() - sum.get();
    }

    @Override
    protected Scalable getScalable(String id) {
        return Scalable.onGenerator(id);
    }
}
