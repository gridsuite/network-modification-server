/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BySimpleModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.*;
import org.gridsuite.modification.server.dto.byfilter.simple.SimpleModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.simple.PropertySimpleModificationByFilterInfos;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.*;

public class BySimpleModification extends AbstractByFilterModification {
    private final BySimpleModificationInfos modificationInfos;

    public BySimpleModification(BySimpleModificationInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_SIMPLE_MODIFICATION_ERROR, "Missing required attributes to modify the equipment");
        }

        if (CollectionUtils.isEmpty(modificationInfos.getSimpleModificationInfosList())) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_SIMPLE_MODIFICATION_ERROR, "At least one modification is required");
        }

        if (modificationInfos.getSimpleModificationInfosList().stream().anyMatch(formulaInfos -> CollectionUtils.isEmpty(formulaInfos.getFilters()))) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_SIMPLE_MODIFICATION_ERROR, "Every modification must have at least one filter");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        Map<UUID, String> filters = modificationInfos.getSimpleModificationInfosList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, modificationInfos.getErrorType());

        if (exportFilters != null) {
            ReportNode formulaSubReporter = subReportNode.newReportNode().withMessageTemplate("appliedFormulasModifications", "Formulas").add();
            List<ReportNode> formulaReports = new ArrayList<>();
            modificationInfos.getSimpleModificationInfosList().forEach(fieldModificationInfos ->
                    fieldModificationInfos.getFilters().forEach(filterInfos ->
                            applyModificationOnFilterEquipments(network, exportFilters, formulaReports, fieldModificationInfos, filterInfos)));
            subReportNode.newReportNode()
                    .withMessageTemplate("byFormulaModification", "New modification by formula on ${" + KEY_EQPT_TYPE + "}")
                    .withUntypedValue(KEY_EQPT_TYPE, modificationInfos.getIdentifiableType().name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            if (equipmentNotModifiedCount == 0 && equipmentNotFoundCount == 0) {
                subReportNode.newReportNode()
                        .withMessageTemplate("byFormulaModificationALL", "All equipment have been modified : ${" + KEY_VALUE + "} equipment(s)")
                        .withUntypedValue(KEY_VALUE, equipmentCount)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
                report(formulaSubReporter, formulaReports);
            } else {
                if (equipmentNotModifiedCount == equipmentCount) {
                    createReport(subReportNode, "byFormulaModificationNone",
                            "No equipment have been modified",
                            Map.of(), TypedValue.ERROR_SEVERITY);
                } else {
                    subReportNode.newReportNode()
                            .withMessageTemplate("byFormulaModificationSome", "Some of the equipment have been modified : ${" + KEY_NB_CHANGED + "} equipment(s) modified and ${" + KEY_NB_UNCHANGED + "} equipment(s) not modified")
                            .withUntypedValue(KEY_NB_CHANGED, equipmentCount - equipmentNotModifiedCount)
                            .withUntypedValue(KEY_NB_UNCHANGED, equipmentNotModifiedCount + equipmentNotFoundCount)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                    report(formulaSubReporter, formulaReports);
                }
            }
        }
    }

    private void report(ReportNode formulaSubReportNode, List<ReportNode> formulaReports) {
        formulaSubReportNode.newReportNode()
                .withMessageTemplate("appliedSimpleModifications", "  Simple modification")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        formulaReports.forEach(report -> insertReportNode(formulaSubReportNode, report));
    }

    private void applyModificationOnFilterEquipments(Network network,
                                                     Map<UUID, FilterEquipments> exportFilters,
                                                     List<ReportNode> formulaReports,
                                                     SimpleModificationByFilterInfos<?> simpleModificationInfos,
                                                     FilterInfos filterInfos) {
        FilterEquipments filterEquipments = exportFilters.get(filterInfos.getId());

        if (CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes())) {
            formulaReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("byFormulaModificationFormulaFilter_" + formulaReports.size(), "No equipments were found for filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            equipmentCount += filterEquipments.getIdentifiableAttributes().size();
            if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
                equipmentNotFoundCount += filterEquipments.getNotFoundEquipments().size();
            }
            List<String> notEditableEquipments = new ArrayList<>();
            List<ReportNode> equipmentsReport = new ArrayList<>();
            filterEquipments.getIdentifiableAttributes()
                    .stream()
                    .map(attributes -> network.getIdentifiable(attributes.getId()))
                    .filter(identifiable -> {
                        boolean isEditableEquipment = isEquipmentEditable(identifiable, simpleModificationInfos, equipmentsReport);
                        if (!isEditableEquipment) {
                            notEditableEquipments.add(identifiable.getId());
                            equipmentNotModifiedCount += 1;
                        }
                        return isEditableEquipment;
                    })
                    .forEach(identifiable -> applyModification(identifiable, simpleModificationInfos, equipmentsReport, notEditableEquipments));

            createByFilterModificationReports(formulaReports, simpleModificationInfos, filterInfos, filterEquipments, notEditableEquipments);

            formulaReports.addAll(equipmentsReport);
        }
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> identifiable, AbstractModificationByFilterInfos filterModificationInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        return true;
    }

    @Override
    protected Object applyValue(Identifiable<?> identifiable, AbstractModificationByFilterInfos filterModificationInfos) {
        SimpleModificationByFilterInfos<?> simpleModificationInfos = (SimpleModificationByFilterInfos<?>) filterModificationInfos;
        if (simpleModificationInfos.getDataType() == DataType.PROPERTY) {
            identifiable.setProperty(
                    ((PropertySimpleModificationByFilterInfos) simpleModificationInfos).getPropertyName(),
                    (String) simpleModificationInfos.getValue()
            );
        } else {
            switch (identifiable.getType()) {
                case GENERATOR -> GeneratorField.setNewValue((Generator) identifiable, simpleModificationInfos);
                case BATTERY -> BatteryField.setNewValue((Battery) identifiable, simpleModificationInfos);
                case SHUNT_COMPENSATOR -> ShuntCompensatorField.setNewValue((ShuntCompensator) identifiable, simpleModificationInfos);
                case VOLTAGE_LEVEL -> VoltageLevelField.setNewValue((VoltageLevel) identifiable, simpleModificationInfos);
                case LOAD -> LoadField.setNewValue((Load) identifiable, simpleModificationInfos);
                case TWO_WINDINGS_TRANSFORMER -> TwoWindingsTransformerField.setNewValue((TwoWindingsTransformer) identifiable, simpleModificationInfos);
                default -> throw new NetworkModificationException(NetworkModificationException.Type.BY_SIMPLE_MODIFICATION_ERROR, "Unsupported equipment");
            }
        }
        return simpleModificationInfos.getValue();
    }
}
