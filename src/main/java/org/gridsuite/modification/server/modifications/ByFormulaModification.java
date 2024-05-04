/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.equipmentfield.*;
import org.gridsuite.modification.server.service.FilterService;
import org.jetbrains.annotations.NotNull;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.*;

public class ByFormulaModification extends AbstractModification {
    public static final String EQUIPMENT_MODIFIED_REPORT_ERROR = "EquipmentModifiedReportError_";
    public static final String KEY_FILTER_NAME = "filterName";
    public static final String KEY_FIELD_NAME = "fieldName";
    public static final String KEY_EQPT_NAME = "eqptName";
    public static final String KEY_EQPT_TYPE = "eqptType";
    public static final String KEY_NB_CHANGED = "nbChanged";
    public static final String KEY_NB_UNCHANGED = "nbUnchanged";
    public static final String KEY_VALUE = "value";
    private final ByFormulaModificationInfos modificationInfos;
    protected FilterService filterService;
    private int equipmentNotModifiedCount;

    public ByFormulaModification(ByFormulaModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
        equipmentNotModifiedCount = 0;
    }

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
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
            long equipmentCount = exportFilters.values()
                    .stream()
                    .filter(filterEquipments -> !CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes()))
                    .mapToLong(filterEquipments -> filterEquipments.getIdentifiableAttributes().size())
                    .sum();
            long equipmentNotFoundCount = exportFilters.values()
                    .stream()
                    .filter(filterEquipments -> !CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments()))
                    .mapToLong(filterEquipments -> filterEquipments.getNotFoundEquipments().size())
                    .sum();
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
        formulaReports.forEach(report -> newReportNode(formulaSubReportNode, report));
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
                    .forEach(identifiable -> applyFormula(identifiable, formulaInfos, equipmentsReport, notEditableEquipments));

            createFormulaReports(formulaReports, formulaInfos, filterInfos, filterEquipments, notEditableEquipments);

            formulaReports.addAll(equipmentsReport);
        }
    }

    private void createFormulaReports(List<ReportNode> formulaReports, FormulaInfos formulaInfos, FilterInfos filterInfos, FilterEquipments filterEquipments, List<String> notEditableEquipments) {
        if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
            formulaReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("byFormulaModificationFormulaFilterFailed_" + formulaReports.size(), "No equipment(s) have been modified on filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            formulaReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("byFormulaModificationFormulaFilter_" + formulaReports.size(), "Successful application of new modification by formula on filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            formulaReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("numberOfValidEquipment" + formulaReports.size(), "      Number of equipment modified : ${" + KEY_NB_CHANGED + "}")
                    .withUntypedValue(KEY_NB_CHANGED, filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                formulaReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("NotEditedEquipmentsFilter_" + formulaReports.size(), "       ${" + KEY_NB_UNCHANGED + "} equipment(s) were not modified")
                        .withUntypedValue(KEY_NB_UNCHANGED, notEditableEquipments.size())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }
        }

        formulaReports.add(ReportNode.newRootReportNode()
                .withMessageTemplate("editedFieldFilter_" + formulaReports.size(), "      Edited field :${" + KEY_FIELD_NAME + "}")
                .withUntypedValue(KEY_FIELD_NAME, formulaInfos.getEditedField())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
            String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
            formulaReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("filterEquipmentsNotFound_" + formulaReports.size(), "      Equipment not found : ${" + KEY_VALUE + "}")
                    .withUntypedValue(KEY_VALUE, equipmentIds)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    private boolean isEquipmentEditable(Identifiable<?> identifiable,
                                        FormulaInfos formulaInfos,
                                        List<ReportNode> equipmentsReport) {
        if (formulaInfos.getEditedField() == null) {
            return false;
        }

        if (identifiable.getType() == IdentifiableType.TWO_WINDINGS_TRANSFORMER) {
            TwoWindingsTransformerField editedField = TwoWindingsTransformerField.valueOf(formulaInfos.getEditedField());
            TwoWindingsTransformer twoWindingsTransformer = (TwoWindingsTransformer) identifiable;
            return switch (editedField) {
                case TARGET_V, RATIO_LOW_TAP_POSITION, RATIO_TAP_POSITION, RATIO_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getRatioTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(ReportNode.newRootReportNode()
                                .withMessageTemplate(EQUIPMENT_MODIFIED_REPORT_ERROR + equipmentsReport.size(), "        Cannot modify field ${" + KEY_FIELD_NAME + "} of equipment ${" + KEY_EQPT_NAME + "} : Ratio tab changer is null")
                                .withUntypedValue(KEY_FIELD_NAME, editedField.name())
                                .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                                .withSeverity(TypedValue.TRACE_SEVERITY)
                                .build());
                    }
                    yield isEditable;
                }
                case REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION, PHASE_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getPhaseTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(ReportNode.newRootReportNode()
                                .withMessageTemplate(EQUIPMENT_MODIFIED_REPORT_ERROR + equipmentsReport.size(), "        Cannot modify field ${" + KEY_FIELD_NAME + "} of equipment ${" + KEY_EQPT_NAME + "} : Phase tab changer is null")
                                .withUntypedValue(KEY_FIELD_NAME, editedField.name())
                                .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                                .withSeverity(TypedValue.TRACE_SEVERITY)
                                .build());
                    }
                    yield isEditable;
                }
                default -> true;
            };
        }
        return true;
    }

    private void applyFormula(Identifiable<?> identifiable,
                              FormulaInfos formulaInfos,
                              List<ReportNode> reports,
                              List<String> notEditableEquipments) {
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
        } else if (value2 == 0 && formulaInfos.getOperator() == Operator.DIVISION) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(identifiable.getId());
        } else {
            try {
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
                reports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("EquipmentModifiedReport_" + reports.size(), "        ${" + KEY_EQPT_TYPE + "} id : ${" + KEY_EQPT_NAME + "}, new value of ${" + KEY_FIELD_NAME + "} : ${" + KEY_VALUE + "}")
                        .withUntypedValue(KEY_EQPT_TYPE, modificationInfos.getIdentifiableType().name())
                        .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                        .withUntypedValue(KEY_FIELD_NAME, formulaInfos.getEditedField())
                        .withUntypedValue(KEY_VALUE, newValue)
                        .withSeverity(TypedValue.TRACE_SEVERITY)
                        .build());
            } catch (Exception e) {
                notEditableEquipments.add(identifiable.getId());
                equipmentNotModifiedCount += 1;
                reports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("EquipmentModifiedReportExceptionf_" + reports.size(), "        Cannot modify equipment ${" + KEY_EQPT_NAME + "} : ${" + KEY_VALUE + "}")
                        .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                        .withUntypedValue(KEY_VALUE, e.getMessage())
                        .withSeverity(TypedValue.TRACE_SEVERITY)
                        .build());
            }
        }
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
