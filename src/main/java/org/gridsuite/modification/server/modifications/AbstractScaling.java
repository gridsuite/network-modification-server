/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public abstract class AbstractScaling extends AbstractModification {
    protected final ScalingInfos scalingInfos;

    protected FilterService filterService;

    protected AbstractScaling(ScalingInfos scalingInfos) {
        this.scalingInfos = scalingInfos;
    }

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        var filters = scalingInfos.getVariations().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, scalingInfos.getErrorType());
        if (exportFilters != null) {
            Map<UUID, FilterEquipments> filtersWithWrongEquipmentIds = ModificationUtils.getUuidFilterWrongEquipmentsIdsMap(subReportNode, exportFilters, filters);

            // apply variations
            scalingInfos.getVariations().forEach(variation -> {
                List<IdentifiableAttributes> identifiableAttributes = ModificationUtils.getIdentifiableAttributes(exportFilters, filtersWithWrongEquipmentIds, variation.getFilters(), subReportNode);

                if (CollectionUtils.isEmpty(identifiableAttributes)) {
                    String filterNames = variation.getFilters().stream().map(FilterInfos::getName).collect(Collectors.joining(", "));
                    createReport(subReportNode,
                            "allFiltersWrong",
                            "All of the following variation's filters have equipments with wrong id : ${filterNames}",
                            Map.of("filterNames", filterNames), TypedValue.WARN_SEVERITY);
                } else {
                    applyVariation(network, subReportNode, identifiableAttributes, variation);
                }
            });
            createReport(subReportNode, "scalingCreated", "new scaling created", Map.of(), TypedValue.INFO_SEVERITY);
        }
    }

    private void applyVariation(Network network,
                                ReportNode subReportNode,
                                List<IdentifiableAttributes> identifiableAttributes,
                                ScalingVariationInfos variation) {
        switch (variation.getVariationMode()) {
            case PROPORTIONAL:
                applyProportionalVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            case PROPORTIONAL_TO_PMAX:
                applyProportionalToPmaxVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            case REGULAR_DISTRIBUTION:
                applyRegularDistributionVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            case VENTILATION:
                applyVentilationVariation(network, subReportNode, identifiableAttributes, variation, getDistributionKeys(identifiableAttributes, subReportNode));
                break;
            case STACKING_UP:
                applyStackingUpVariation(network, subReportNode, identifiableAttributes, variation);
                break;
            default:
                throw new NetworkModificationException(scalingInfos.getErrorType(), String.format("This variation mode is not supported : %s", variation.getVariationMode().name()));
        }
    }

    private Double getDistributionKeys(List<IdentifiableAttributes> identifiableAttributes, ReportNode subReportNode) {
        var distributionKeys = identifiableAttributes.stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(IdentifiableAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            createReport(subReportNode, "distributionKeysNotFound", "This mode is available only for equipment with distribution key", Map.of(), TypedValue.WARN_SEVERITY);
            return null;
        }
        return distributionKeys;
    }

    protected abstract void applyStackingUpVariation(Network network, ReportNode subReportNode, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract void applyVentilationVariation(Network network, ReportNode subReportNode, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos, Double distributionKeys);

    protected abstract void applyRegularDistributionVariation(Network network, ReportNode subReportNode, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract void applyProportionalToPmaxVariation(Network network, ReportNode subReportNode, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract void applyProportionalVariation(Network network, ReportNode subReportNode, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos);

    protected abstract double getAsked(ScalingVariationInfos variationInfos, AtomicReference<Double> sum);

    protected abstract Scalable getScalable(String id);

}
