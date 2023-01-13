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
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.ScalingInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.context.ApplicationContext;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public abstract class AbstractScaling extends AbstractModification {
    protected final ScalingInfos scalingInfos;

    protected AbstractScaling(ScalingInfos scalingInfos) {
        this.scalingInfos = scalingInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter, ApplicationContext context) {
        // collect all filters from all variations
        List<FilterInfos> filterInfos = scalingInfos.getVariations().parallelStream()
                .flatMap(v -> v.getFilters().parallelStream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toList());

        Map<UUID, String> filters = filterInfos.parallelStream().collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        // export filters from filter server
        String workingVariantId = network.getVariantManager().getWorkingVariantId();
        UUID uuid = ((NetworkImpl) network).getUuid();
        Map<UUID, FilterEquipments> exportFilters = context.getBean(FilterService.class)
                .exportFilters(new ArrayList<>(filters.keySet()), uuid, workingVariantId)
                .parallelStream()
                .peek(t -> t.setFilterName(filters.get(t.getFilterId())))
                .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));

        // collect all filters with wrong equipments ids
        Map<UUID, FilterEquipments> filterWithWrongEquipmentsIds = exportFilters.entrySet().parallelStream()
                .filter(e -> !e.getValue().getNotFoundEquipments().isEmpty())
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        // check if all exported filters contain equipments with wrong ids
        if (filterWithWrongEquipmentsIds.size() == exportFilters.size()) {
            String errorMsg = "All filters contains equipments with wrong ids";
            createReport(subReporter, "invalidFilters", errorMsg, TypedValue.ERROR_SEVERITY);
            throw new NetworkModificationException(scalingInfos.getErrorType(), errorMsg);
        }

        // create report for each wrong filter
        filterWithWrongEquipmentsIds.values().forEach(f -> {
            StringBuilder equipmentIds = new StringBuilder();
            f.getNotFoundEquipments().forEach(equipmentIds::append);
            createReport(subReporter,
                    "filterEquipmentsNotFound",
                    String.format("Cannot find the following equipments %s in filter %s", equipmentIds, filters.get(f.getFilterId())),
                    TypedValue.WARN_SEVERITY);
        });

        // apply variations
        scalingInfos.getVariations().forEach(variation -> {
            List<IdentifiableAttributes> identifiableAttributes = variation.getFilters().stream()
                    .filter(f -> !filterWithWrongEquipmentsIds.containsKey(f.getId()))
                    .flatMap(f -> exportFilters.get(f.getId())
                            .getIdentifiableAttributes()
                            .stream())
                    .collect(Collectors.toList());

            if (CollectionUtils.isEmpty(identifiableAttributes)) {
                var filterNames = variation.getFilters().stream().map(FilterInfos::getName).collect(Collectors.joining(","));
                createReport(subReporter, "allFiltersWrong", String.format("All of the following variation's filters have equipments with wrong id: %s", filterNames), TypedValue.WARN_SEVERITY);
            } else {
                applyVariation(network, subReporter, identifiableAttributes, variation);
            }
        });
        createReport(subReporter, "scalingVariationCreated", "new scaling created", TypedValue.INFO_SEVERITY);
    }

    private void applyVariation(Network network,
                                Reporter subReporter,
                                List<IdentifiableAttributes> identifiableAttributes,
                                ScalingVariationInfos variation) {
        switch (variation.getVariationMode()) {
            case PROPORTIONAL:
                applyProportionalVariation(network, identifiableAttributes, variation, subReporter);
                break;
            case REGULAR_DISTRIBUTION:
                applyRegularDistributionVariation(network, identifiableAttributes, variation, subReporter);
                break;
            case VENTILATION:
                applyVentilationVariation(network, identifiableAttributes, variation, subReporter, getDistributionKeys(identifiableAttributes, subReporter));
                break;
            default:
                throw new NetworkModificationException(scalingInfos.getErrorType(), String.format("This variation mode is not supported : %s", variation.getVariationMode().name()));
        }
    }

    private double getDistributionKeys(List<IdentifiableAttributes> identifiableAttributes, Reporter subReporter) {
        var distributionKeys = identifiableAttributes.stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(IdentifiableAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            String message = "This mode is available only for equipment with distribution key";
            createReport(subReporter, "distributionKeysNotFound", message, TypedValue.ERROR_SEVERITY);
            throw new NetworkModificationException(scalingInfos.getErrorType(), message);
        }
        return distributionKeys;
    }

    protected abstract void applyVentilationVariation(Network network,
                                          List<IdentifiableAttributes> identifiableAttributes,
                                          ScalingVariationInfos scalingVariationInfos, Reporter subReporter,
                                                      double distributionKeys);

    protected abstract void applyRegularDistributionVariation(Network network,
                                                              List<IdentifiableAttributes> identifiableAttributes,
                                                              ScalingVariationInfos scalingVariationInfos, Reporter subReporter);

    protected abstract void applyProportionalVariation(Network network,
                                                       List<IdentifiableAttributes> identifiableAttributes,
                                                       ScalingVariationInfos scalingVariationInfos, Reporter subReporter);

    protected abstract double getAsked(ScalingVariationInfos variationInfos, AtomicReference<Double> sum);

    protected abstract Scalable getScalable(String id);
}
