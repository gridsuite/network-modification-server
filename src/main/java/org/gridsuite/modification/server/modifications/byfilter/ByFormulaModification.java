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
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.*;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.jetbrains.annotations.NotNull;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.*;

public class ByFormulaModification extends AbstractByFilterModification {
    private final ByFormulaModificationInfos modificationInfos;

    public ByFormulaModification(ByFormulaModificationInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR, "Missing required attributes to modify the equipment");
        }

        if (CollectionUtils.isEmpty(modificationInfos.getFormulaInfosList())) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR, "At least one formula is required");
        }

        if (modificationInfos.getFormulaInfosList().stream().anyMatch(formulaInfos -> CollectionUtils.isEmpty(formulaInfos.getFilters()))) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR, "Every formula must have at least one filter");
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        Map<UUID, String> filters = modificationInfos.getFormulaInfosList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, modificationInfos.getErrorType());

        if (exportFilters != null) {
            ReportNode formulaSubReporter = subReportNode.newReportNode().withMessageTemplate("appliedFormulasModifications", "Formulas").add();
            List<ReportNode> formulaReports = new ArrayList<>();
            modificationInfos.getFormulaInfosList().forEach(formulaInfos ->
                    formulaInfos.getFilters().forEach(filterInfos ->
                            applyFormulaOnFilterEquipments(network, exportFilters, formulaReports, formulaInfos, filterInfos)));
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
                .withMessageTemplate("appliedFormulasModifications", "  Formulas")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        formulaReports.forEach(report -> insertReportNode(formulaSubReportNode, report));
    }

    private void applyFormulaOnFilterEquipments(Network network,
                                                Map<UUID, FilterEquipments> exportFilters,
                                                List<ReportNode> formulaReports,
                                                FormulaInfos formulaInfos,
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
                        boolean isEditableEquipment = isEquipmentEditable(identifiable, formulaInfos, equipmentsReport);
                        if (!isEditableEquipment) {
                            notEditableEquipments.add(identifiable.getId());
                            equipmentNotModifiedCount += 1;
                        }
                        return isEditableEquipment;
                    })
                    .forEach(identifiable -> applyModification(identifiable, formulaInfos, equipmentsReport, notEditableEquipments));

            createByFilterModificationReports(formulaReports, formulaInfos, filterInfos, filterEquipments, notEditableEquipments);

            formulaReports.addAll(equipmentsReport);
        }
    }

    @Override
    protected boolean preCheckValue(Identifiable<?> identifiable, AbstractModificationByFilterInfos filterModificationInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        FormulaInfos formulaInfos = (FormulaInfos) filterModificationInfos;
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(identifiable);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(identifiable);
        if (value1 == null || Double.isNaN(value1) || value2 == null || Double.isNaN(value2)) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(identifiable.getId());
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(EQUIPMENT_MODIFIED_REPORT_ERROR + reports.size(), "        Cannot modify equipment ${" + KEY_EQPT_NAME + "} : At least one of the value or referenced field is null")
                    .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                    .withSeverity(TypedValue.TRACE_SEVERITY)
                    .build());
            return false;
        }

        if (value2 == 0 && formulaInfos.getOperator() == Operator.DIVISION) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(identifiable.getId());
            return false;
        }
        return true;
    }

    @Override
    protected Object applyValue(Identifiable<?> identifiable, AbstractModificationByFilterInfos filterModificationInfos) {
        FormulaInfos formulaInfos = (FormulaInfos) filterModificationInfos;
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(identifiable);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(identifiable);
        final Double newValue = applyOperation(formulaInfos.getOperator(), value1, value2);
        switch (identifiable.getType()) {
            case GENERATOR -> GeneratorField.setNewValue((Generator) identifiable, formulaInfos.getEditedField(), newValue);
            case BATTERY -> BatteryField.setNewValue((Battery) identifiable, formulaInfos.getEditedField(), newValue);
            case SHUNT_COMPENSATOR -> ShuntCompensatorField.setNewValue((ShuntCompensator) identifiable, formulaInfos.getEditedField(), newValue);
            case VOLTAGE_LEVEL -> VoltageLevelField.setNewValue((VoltageLevel) identifiable, formulaInfos.getEditedField(), newValue);
            case LOAD -> LoadField.setNewValue((Load) identifiable, formulaInfos.getEditedField(), newValue);
            case TWO_WINDINGS_TRANSFORMER -> TwoWindingsTransformerField.setNewValue((TwoWindingsTransformer) identifiable, formulaInfos.getEditedField(), newValue);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR, "Unsupported equipment");
        }
        return newValue;
    }

    private Double applyOperation(Operator operator, @NotNull Double value1, @NotNull Double value2) {
        return switch (operator) {
            case ADDITION -> value1 + value2;
            case SUBTRACTION -> value1 - value2;
            case MULTIPLICATION -> value1 * value2;
            case DIVISION -> value1 / value2;
            case PERCENTAGE -> value1 * (value2 / 100);
        };
    }
}
