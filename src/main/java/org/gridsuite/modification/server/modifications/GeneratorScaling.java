/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class GeneratorScaling extends AbstractScaling {
    private final boolean isIterative;

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        super(generatorScalableInfos);
        this.isIterative = generatorScalableInfos.isIterative();
    }

    @Override
    public void applyStackingUpVariation(Network network,
                                         List<IdentifiableAttributes> identifiableAttributes,
                                         ScalingVariationInfos generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Scalable stackingUpScalable = Scalable.stack(identifiableAttributes.stream()
                .map(equipment -> getScalable(equipment.getId())).toArray(Scalable[]::new));
        stackingUpScalable.scale(network, getAsked(generatorScalingVariation, sum));
    }

    @Override
    public void applyVentilationVariation(Network network,
                                          List<IdentifiableAttributes> identifiableAttributes,
                                          ScalingVariationInfos generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        var distributionKeys = identifiableAttributes.stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(IdentifiableAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "This mode is available only for equipment with distribution key");
        }

        identifiableAttributes.forEach(equipment -> {
            scalables.add(getScalable(equipment.getId()));
            percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
        });
        Scalable ventilationScalable = Scalable.proportional(percentages, scalables, isIterative);
        ventilationScalable.scale(network, getAsked(generatorScalingVariation, sum));
    }

    @Override
    public void applyRegularDistributionVariation(Network network,
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

        if (!scalables.isEmpty()) {
            List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), (float) (100.0 / scalables.size())));

            Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
            regularDistributionScalable.scale(network,
                    getAsked(generatorScalingVariation, sum));
        } else {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "equipments cannot be found");
        }
    }

    @Override
    public void applyProportionalToPmaxVariation(Network network,
                                                  List<IdentifiableAttributes> identifiableAttributes,
                                                  ScalingVariationInfos generatorScalingVariation) {
        List<Generator> generators = identifiableAttributes
                .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
        AtomicReference<Double> maxPSum = new AtomicReference<>(0D);
        AtomicReference<Double> targetPSum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getMaxP());
            maxPSum.set(maxPSum.get() + generator.getMaxP());
            targetPSum.set(targetPSum.get() + generator.getTargetP());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / maxPSum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalToPmaxScalable = Scalable.proportional(percentages, scalables, isIterative);
        proportionalToPmaxScalable.scale(network,
                getAsked(generatorScalingVariation, targetPSum),
                Scalable.ScalingConvention.GENERATOR);
    }

    @Override
    public void applyProportionalVariation(Network network,
                                            List<IdentifiableAttributes> identifiableAttributes,
                                            ScalingVariationInfos generatorScalingVariation) {
        List<Generator> generators = identifiableAttributes
                .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getTargetP());
            sum.set(sum.get() + generator.getTargetP());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables, isIterative);
        proportionalScalable.scale(network,
                getAsked(generatorScalingVariation, sum));
    }

    @Override
    public double getAsked(ScalingVariationInfos generatorScalingVariation, AtomicReference<Double> sum) {
        return scalingInfos.getVariationType() == VariationType.DELTA_P
                ? generatorScalingVariation.getVariationValue()
                : generatorScalingVariation.getVariationValue() - sum.get();
    }

    @Override
    public Scalable getScalable(String id) {
        return Scalable.onGenerator(id);
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return NetworkModificationException.Type.GENERATOR_SCALING_ERROR;
    }

    @Override
    public ModificationType getModificationType() {
        return ModificationType.GENERATOR_SCALING;
    }
}
