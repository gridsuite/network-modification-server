/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.powsybl.iidm.modification.scalable.ScalingParameters.Priority.RESPECT_OF_VOLUME_ASKED;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class GeneratorScaling extends AbstractScaling {

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        super(generatorScalableInfos);
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
        scale(network, subReporter, generatorScalingVariation, sum, stackingUpScalable, new ScalingParameters());
    }

    @Override
    protected void applyVentilationVariation(Network network,
                                          Reporter subReporter,
                                          List<IdentifiableAttributes> identifiableAttributes,
                                          ScalingVariationInfos generatorScalingVariation,
                                          Double distributionKeys) {
        if (distributionKeys != null) {
            AtomicReference<Double> sum = new AtomicReference<>(0D);
            List<Double> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            identifiableAttributes.forEach(equipment -> {
                sum.set(network.getGenerator(equipment.getId()).getTargetP() + sum.get());
                scalables.add(getScalable(equipment.getId()));
                percentages.add((equipment.getDistributionKey() / distributionKeys) * 100);
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            scale(network, subReporter, generatorScalingVariation, sum, ventilationScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));
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

        List<Double> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), 100.0 / scalables.size()));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        scale(network, subReporter, generatorScalingVariation, sum, regularDistributionScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));
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
        List<Double> percentages = new ArrayList<>();
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
        scale(network, subReporter, generatorScalingVariation, targetPSum, proportionalToPmaxScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));
    }

    @Override
    protected void applyProportionalVariation(Network network,
                                           Reporter subReporter,
                                           List<IdentifiableAttributes> identifiableAttributes,
                                           ScalingVariationInfos generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Generator> generators = identifiableAttributes
                .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
        List<Double> percentages = new ArrayList<>();
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
        scale(network, subReporter, generatorScalingVariation, sum, proportionalScalable, new ScalingParameters().setPriority(RESPECT_OF_VOLUME_ASKED));
    }

    private void setScalablePercentage(AtomicReference<Double> sum,
                                       Map<String, Double> targetPMap,
                                       List<Double> percentages,
                                       List<Scalable> scalables) {
        targetPMap.forEach((id, p) -> {
            percentages.add((p / sum.get()) * 100);
            scalables.add(getScalable(id));
        });
    }

    private void scale(Network network, Reporter subReporter, ScalingVariationInfos scalingVariationInfos, AtomicReference<Double> sum, Scalable scalable, ScalingParameters scalingParameters) {
        double asked = getAsked(scalingVariationInfos, sum);
        double done = scalable.scale(network, asked, scalingParameters);
        subReporter.report(Report.builder()
                .withKey("scalingApplied")
                .withDefaultMessage("successfully scaled for mode ${scalingMode} with variation value asked is ${askedValue}. variation done is  ${actualValue}")
                .withValue("scalingMode", scalingVariationInfos.getVariationMode().name())
                .withValue("askedValue", asked)
                .withValue("actualValue", done)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
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
