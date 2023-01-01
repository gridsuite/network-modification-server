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
import org.gridsuite.modification.server.service.SpringContext;
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
        loadScalingVariations.forEach(l -> filterIds.addAll(l.getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList())));
        List<String> distinctFilterIds = filterIds.stream().distinct().collect(Collectors.toList());
        List<FilterAttributes> filters = SpringContext.getBean(FilterService.class).getFilters(distinctFilterIds);

        if (!CollectionUtils.isEmpty(filters)) {
            loadScalingVariations.forEach(loadScalingVariation -> {
                var filterIdsList = loadScalingVariation.getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList());
                var filterList = filters.stream()
                        .filter(f -> filterIdsList.contains(f.getId()))
                        .collect(Collectors.toList());
                if (!filterList.isEmpty()) {
                    filterList.forEach(filter -> applyVariation(network, filter, loadScalingVariation, subReporter));
                } else {
                    throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "One of the variations does not have a correct filters");
                }
            });
        } else {
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "Filters list is Empty");
        }
        createReport(subReporter, "loadScalingCreated", "new load scaling created", TypedValue.INFO_SEVERITY);
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
                scaleWithProportionalMode(network, filter, loadScalingVariation, subReporter, sum, targetPMap, percentages, scalables);
                break;
            case REGULAR_DISTRIBUTION:
                scaleWithRegularMode(network, filter, loadScalingVariation, sum, percentages, scalables);
                break;
            case VENTILATION:
                scaleWithVentilationMode(network, filter, loadScalingVariation, sum, percentages, scalables);
                break;
            default:
                throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "Active Variation mode not recognised");
        }
    }

    private void scaleWithVentilationMode(Network network, FilterAttributes filter, LoadScalingVariation loadScalingVariation, AtomicReference<Double> sum, List<Float> percentages, List<Scalable> scalables) {
        var distributionKeys = filter.getFilterEquipmentsAttributes()
                .stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(FilterEquipmentAttributes::getDistributionKey)
                .sum();

        if (distributionKeys == 0) {
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "This mode is available only for equipment with distribution key");
        }

        filter.getFilterEquipmentsAttributes().forEach(equipment -> {
            scalables.add(getScalable(equipment.getEquipmentID()));
            percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
        });

        Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
        scale(network, loadScalingVariation, sum, ventilationScalable);
    }

    private void scaleWithRegularMode(Network network, FilterAttributes filter, LoadScalingVariation loadScalingVariation, AtomicReference<Double> sum, List<Float> percentages, List<Scalable> scalables) {
        List<String> notFoundEquipments = new ArrayList<>();
        scalables.addAll(filter.getFilterEquipmentsAttributes()
                .stream()
                .map(equipment -> {
                    Load load = network.getLoad(equipment.getEquipmentID());
                    if (load != null) {
                        sum.set(sum.get() + load.getP0());
                        return getScalable(equipment.getEquipmentID());
                    } else {
                        notFoundEquipments.add(equipment.getEquipmentID());
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList()));
        if (scalables.isEmpty()) {
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR,
                    "Error while creating load scaling, All generators of filter " + filter.getId() +  " not found: " + String.join(", ", notFoundEquipments));
        }

        percentages.addAll(Collections.nCopies(scalables.size(), 100 / (float) scalables.size()));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        scale(network, loadScalingVariation, sum, regularDistributionScalable);
    }

    private void scaleWithProportionalMode(Network network, FilterAttributes filter, LoadScalingVariation loadScalingVariation, Reporter subReporter, AtomicReference<Double> sum, Map<String, Double> targetPMap, List<Float> percentages, List<Scalable> scalables) {
        List<String> notFoundEquipments = new ArrayList<>();
        filter.getFilterEquipmentsAttributes()
                .forEach(equipment -> {
                    Load load = network.getLoad(equipment.getEquipmentID());
                    if (load != null) {
                        targetPMap.put(load.getId(), load.getP0());
                        sum.set(sum.get() + load.getP0());
                    } else {
                        createReport(subReporter, NetworkModificationException.Type.LOAD_SCALING_ERROR.name(), "load " + equipment.getEquipmentID() + " not found", TypedValue.ERROR_SEVERITY);
                    }
                });
        checkVariationFilter(filter, subReporter, targetPMap, notFoundEquipments);
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });
        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        scale(network, loadScalingVariation, sum, proportionalScalable);
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

    private void checkVariationFilter(FilterAttributes filter, Reporter subReporter, Map<String, Double> targetPMap, List<String> notFoundEquipments) {
        if (!notFoundEquipments.isEmpty() && notFoundEquipments.size() != filter.getFilterEquipmentsAttributes().size()) {
            // TODO check if this the right behavior
            // Send a warning when some of the load in one filter cannot be found
            createReport(subReporter,
                    "NetworkModificationException.Type.LOAD_SCALING_ERROR.name()",
                    "Loads of filter :" + filter.getId() + " not found : " + String.join(", ", notFoundEquipments),
                    TypedValue.WARN_SEVERITY);
        }

        if (targetPMap.isEmpty()) {
            // TODO check if this the right behavior
            // throw error when all of the load in one filter cannot be found
            throw new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR,
                    "Error while creating load scaling : All loads of filter " + filter.getId() +  " not found : " + String.join(", ", notFoundEquipments));
        }
        createReport(subReporter, "loadScalingCreated", "new load scaling created", TypedValue.INFO_SEVERITY);
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
