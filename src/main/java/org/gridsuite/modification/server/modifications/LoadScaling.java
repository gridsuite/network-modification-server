/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.FilterEquipmentAttributes;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.LoadScalingVariation;
import org.gridsuite.modification.server.service.FilterService;
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
public class LoadScaling extends AbstractModification {

    private final LoadScalingInfos loadScalingInfos;

    @Autowired
    public LoadScaling(LoadScalingInfos loadScalableInfos) {
        this.loadScalingInfos = loadScalableInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        List<LoadScalingVariation> loadScalingVariations = loadScalingInfos.getLoadScalingVariations();
        List<String> filterIds = new ArrayList<>();
        loadScalingVariations.forEach(l -> filterIds.addAll(l.getFilters().stream().filter(Objects::nonNull).map(FilterInfos::getId).collect(Collectors.toList())));
        List<FilterAttributes> filters = new FilterService("http://localhost:5027").getFilters(filterIds);

        loadScalingVariations.forEach(loadScalingVariation -> {
            var filtersList = filters.stream()
                    .filter(f -> loadScalingVariation.getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList()).contains(f.getId()))
                    .collect(Collectors.toList());

            if (!CollectionUtils.isEmpty(filtersList)) {
                filtersList.forEach(filter -> applyVariation(network, filter, loadScalingVariation, subReporter));
            } else {
                createReport(subReporter,NetworkModificationException.Type.LOAD_SCALING_ERROR.name(),"Filters list is empty",TypedValue.ERROR_SEVERITY);
                throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "Filters list is empty");
            }
        });
        createReport(subReporter,"loadScalingCreated","new load scaling created",TypedValue.INFO_SEVERITY);
    }

    private void applyVariation(Network network,
                                FilterAttributes filter,
                                LoadScalingVariation loadScalingVariation, Reporter subReporter) {

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
                            } else {
                                createReport(subReporter,NetworkModificationException.Type.LOAD_SCALING_ERROR.name(),"load " + equipment.getEquipmentID() + " not found",TypedValue.ERROR_SEVERITY);
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
                } else {
                    createReport(subReporter,NetworkModificationException.Type.LOAD_SCALING_ERROR.name(),"load scalable list is empty",TypedValue.WARN_SEVERITY);

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
        if (loadScalingInfos.getVariationType() == VariationType.DELTA_P) {
            return loadScalingVariation.getVariationValue();
        } else if (loadScalingInfos.getVariationType() == VariationType.TARGET_P) {
            return loadScalingVariation.getVariationValue() - sum.get();
        } else {
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "variation type not found");
        }
    }

    static void createReport(Reporter reporter, String reporterKey, String message, TypedValue errorSeverity) {
        reporter.report(Report.builder()
                .withKey(reporterKey)
                .withDefaultMessage(message)
                .withSeverity(errorSeverity)
                .build());
    }

    private Scalable getScalable(String id) {
        return Scalable.onLoad(id);
    }
}
