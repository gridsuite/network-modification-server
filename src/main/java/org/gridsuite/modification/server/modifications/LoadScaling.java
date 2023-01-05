/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScaling extends AbstractScaling {

    @Autowired
    public LoadScaling(LoadScalingInfos loadScalableInfos) {
        super(loadScalableInfos);
    }

    @Override
    public void applyVentilationVariation(Network network,
                                          List<IdentifiableAttributes> identifiableAttributes,
                                          ScalingVariationInfos scalingVariationInfos) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        var distributionKeys = identifiableAttributes.stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(IdentifiableAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "This mode is available only for equipment with distribution key");
        }

        identifiableAttributes.forEach(equipment -> {
            scalables.add(getScalable(equipment.getId()));
            percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
        });
        Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
        scale(network, scalingVariationInfos, sum, ventilationScalable);
    }

    @Override
    public void applyRegularDistributionVariation(Network network,
                                                  List<IdentifiableAttributes> identifiableAttributes,
                                                  ScalingVariationInfos scalingVariationInfos) {
        List<Load> generators = identifiableAttributes
                .stream()
                .map(attribute -> network.getLoad(attribute.getId()))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Scalable> scalables = generators.stream()
                .map(generator -> {
                    sum.set(sum.get() + generator.getP0());
                    return getScalable(generator.getId());
                }).collect(Collectors.toList());

        List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), 100 / (float) scalables.size()));

        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        scale(network, scalingVariationInfos, sum, regularDistributionScalable);
    }

    @Override
    public void applyProportionalVariation(Network network,
                                           List<IdentifiableAttributes> identifiableAttributes,
                                           ScalingVariationInfos scalingVariationInfos) {
        List<Load> generators = identifiableAttributes
                .stream().map(attribute -> network.getLoad(attribute.getId())).collect(Collectors.toList());
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getP0());
            sum.set(sum.get() + generator.getP0());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        scale(network, scalingVariationInfos, sum, proportionalScalable);
    }

    private void scale(Network network, ScalingVariationInfos scalingVariationInfos, AtomicReference<Double> sum, Scalable proportionalScalable) {
        switch (scalingVariationInfos.getReactiveVariationMode()) {
            case CONSTANT_Q:
                proportionalScalable.scale(network,
                        getAsked(scalingVariationInfos, sum),
                        Scalable.ScalingConvention.LOAD);
                break;
            case TAN_FIXED:
                proportionalScalable.scaleWithConstantPowerFactor(network,
                        getAsked(scalingVariationInfos, sum),
                        Scalable.ScalingConvention.LOAD);
                break;
            default:
                throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "Reactive Variation mode not recognised");
        }
    }

    @Override
    public double getAsked(ScalingVariationInfos generatorScalingVariation, AtomicReference<Double> sum) {
        return scalingInfos.getVariationType() == VariationType.DELTA_P
                ? generatorScalingVariation.getVariationValue()
                : generatorScalingVariation.getVariationValue() - sum.get();
    }

    public Scalable getScalable(String id) {
        return Scalable.onLoad(id);
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return NetworkModificationException.Type.LOAD_SCALING_ERROR;
    }

    @Override
    public ModificationType getModificationType() {
        return ModificationType.LOAD_SCALING;
    }
}
