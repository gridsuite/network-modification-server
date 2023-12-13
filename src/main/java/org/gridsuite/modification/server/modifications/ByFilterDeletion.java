/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.modification.topology.RemoveSubstationBuilder;
import com.powsybl.iidm.modification.topology.RemoveVoltageLevel;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
public class ByFilterDeletion extends AbstractModification {

    private final ByFilterDeletionInfos modificationInfos;

    protected FilterService filterService;

    private static final EnumSet<IdentifiableType> CONNECTABLE_EQUIPMENTS = EnumSet.of(
            IdentifiableType.LINE,
            IdentifiableType.TWO_WINDINGS_TRANSFORMER,
            IdentifiableType.THREE_WINDINGS_TRANSFORMER,
            IdentifiableType.GENERATOR,
            IdentifiableType.BATTERY,
            IdentifiableType.LOAD,
            IdentifiableType.SHUNT_COMPENSATOR,
            IdentifiableType.DANGLING_LINE,
            IdentifiableType.STATIC_VAR_COMPENSATOR
            );

    public ByFilterDeletion(ByFilterDeletionInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        var filters = modificationInfos.getEquipmentFilters().stream()
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getInstance().getUuidFilterEquipmentsMap(filterService, network, subReporter, filters, modificationInfos);
        if (exportFilters == null) {
            return;
        }
        Map<UUID, FilterEquipments> exportedFiltersWithWrongEquipmentIds = ModificationUtils.getInstance().getUuidFilterWrongEquipmentsIdsMap(subReporter, exportFilters, filters);
        List<IdentifiableAttributes> identifiableAttributes = ModificationUtils.getIdentifiableAttributes(exportFilters, exportedFiltersWithWrongEquipmentIds, modificationInfos.getEquipmentFilters(), subReporter);

        if (CollectionUtils.isEmpty(identifiableAttributes)) {
            String filterNames = modificationInfos.getEquipmentFilters().stream().map(FilterInfos::getName).collect(Collectors.joining(", "));
            createReport(subReporter,
                    "allFiltersWrong",
                    String.format("All of the following filters have equipments with wrong id : %s", filterNames),
                    TypedValue.WARN_SEVERITY);
        } else {
            applyFilterDeletion(network, subReporter, identifiableAttributes);
        }

        subReporter.report(Report.builder()
                .withKey("equipmentDeleted")
                .withDefaultMessage("equipment of type=${type} and ids=${ids} deleted")
                .withValue("type", modificationInfos.getEquipmentType())
                .withValue("ids", identifiableAttributes.stream().map(IdentifiableAttributes::getId).collect(Collectors.joining(", ")))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyFilterDeletion(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes) {
        IdentifiableType identifiableType = IdentifiableType.valueOf(modificationInfos.getEquipmentType());
        if (CONNECTABLE_EQUIPMENTS.contains(identifiableType)) {
            identifiableAttributes.forEach(identifiableAttribute -> new RemoveFeederBay(identifiableAttribute.getId()).apply(network, true, subReporter));
        } else if (identifiableType == IdentifiableType.VOLTAGE_LEVEL) {
            identifiableAttributes.forEach(identifiableAttribute -> new RemoveVoltageLevel(identifiableAttribute.getId()).apply(network, true, subReporter));
        } else if (identifiableType == IdentifiableType.SUBSTATION) {
            identifiableAttributes.forEach(identifiableAttribute -> new RemoveSubstationBuilder().withSubstationId(identifiableAttribute.getId()).build().apply(network, true, subReporter));
        } else {
            throw new PowsyblException("Unsupported equipment type");
        }
    }
}