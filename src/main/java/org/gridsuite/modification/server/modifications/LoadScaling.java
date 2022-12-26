/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.FilterEquipmentAttributes;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.LoadScalingVariation;
import org.gridsuite.modification.server.service.FilterService;

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
public class LoadScaling extends AbstractModification {
    private final LoadScalingInfos loadScalableInfos;

    public LoadScaling(LoadScalingInfos loadScalableInfos) {
        this.loadScalableInfos = loadScalableInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        List<LoadScalingVariation> loadScalingVariations = loadScalableInfos.getLoadScalingVariations();
        List<String> filterIds = loadScalingVariations.stream()
                .map(LoadScalingVariation::getFilterId)
                .collect(Collectors.toList());
        List<FilterAttributes> filters = new FilterService(null, null).getFiltersMetadata(filterIds);

        for (LoadScalingVariation generatorScalingVariation : loadScalingVariations) {
            var filter = filters.stream()
                    .filter(f -> Objects.equals(generatorScalingVariation.getFilterId(), f.getId()))
                    .findFirst();
            filter.ifPresent(filterAttributes -> applyVariation(network, filterAttributes, generatorScalingVariation));
        }
    }

    private void applyVariation(Network network,
                                FilterAttributes filter,
                                LoadScalingVariation loadScalingVariation) {

        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        switch (loadScalingVariation.getActiveVariationMode()) {
            case PROPORTIONAL:
                filter.getFilterEquipmentsAttributes()
                        .forEach(equipment -> {
                            Load load = network.getLoad(equipment.getEquipmentID());
                            if (load != null) {
                                targetPMap.put(load.getId(), load.getP0());
                                sum.set(sum.get() + load.getP0());
                            }
                        });
                targetPMap.forEach((id, p) -> {
                    percentages.add((float) ((p / sum.get()) * 100));
                    scalables.add(getScalable(id));
                });
                Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
                scale(network, loadScalingVariation, sum, proportionalScalable);
                break;
            case REGULAR_DISTRIBUTION:
                scalables.addAll(filter.getFilterEquipmentsAttributes()
                        .stream()
                        .map(equipment -> {
                            Load load = network.getLoad(equipment.getEquipmentID());
                            if (load != null) {
                                sum.set(sum.get() + load.getP0());
                                return getScalable(equipment.getEquipmentID());
                            }
                            return null;
                        })
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList()));
                if (!scalables.isEmpty()) {
                    percentages.addAll(Collections.nCopies(scalables.size(), 100 / (float) scalables.size()));
                    Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
                    scale(network, loadScalingVariation, sum, regularDistributionScalable);
                }
                break;
            case VENTILATION:
                var distributionKeys = filter.getFilterEquipmentsAttributes().stream()
                        .mapToDouble(FilterEquipmentAttributes::getDistributionKey)
                        .sum();
                if (distributionKeys != 0) {
                    filter.getFilterEquipmentsAttributes().forEach(equipment -> {
                        scalables.add(getScalable(equipment.getEquipmentID()));
                        percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
                    });
                    Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
                    scale(network, loadScalingVariation, sum, ventilationScalable);
                }
                break;
            default:
                throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "Active Variation mode not recognised");
        }
    }

    private void scale(Network network, LoadScalingVariation loadScalingVariation, AtomicReference<Double> sum, Scalable proportionalScalable) {
        switch (loadScalingVariation.getReactiveVariationMode()) {
            case CONSTANT_Q:
                proportionalScalable.scaleWithConstantPowerFactor(network,
                        getAsked(loadScalingVariation, sum),
                        Scalable.ScalingConvention.LOAD);
                break;
            case TAN_FIXED:
                proportionalScalable.scale(network,
                        getAsked(loadScalingVariation, sum),
                        Scalable.ScalingConvention.LOAD);
                break;
            default:
                throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "Reactive Variation mode not recognised");
        }
    }

    private double getAsked(LoadScalingVariation loadScalingVariation, AtomicReference<Double> sum) {
        if (loadScalableInfos.getVariationType() == VariationType.DELTA_P) {
            return loadScalingVariation.getVariationValue();
        } else if (loadScalableInfos.getVariationType() == VariationType.TARGET_P) {
            return loadScalingVariation.getVariationValue() - sum.get();
        } else {
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "variation type not found");
        }
    }

    private Scalable getScalable(String id) {
        return Scalable.onLoad(id);
    }
}
