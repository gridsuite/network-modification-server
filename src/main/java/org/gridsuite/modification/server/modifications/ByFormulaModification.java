/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
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

import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

public class ByFormulaModification extends AbstractModification {
    public static final String EQUIPMENT_MODIFIED_REPORT_ERROR = "EquipmentModifiedReportError_";
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
    public void apply(Network network, Reporter subReporter) {
        // collect all filters from all variations
        Map<UUID, String> filters = modificationInfos.getFormulaInfosList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReporter, filters, modificationInfos.getErrorType());

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
            Reporter formulaSubReporter = subReporter.createSubReporter("appliedFormulasModifications", "Formulas");
            List<Report> formulaReports = new ArrayList<>();
            modificationInfos.getFormulaInfosList().forEach(formulaInfos ->
                    formulaInfos.getFilters().forEach(filterInfos ->
                            applyFormulaOnFilterEquipments(network, exportFilters, formulaReports, formulaInfos, filterInfos)));
            subReporter.report(Report.builder()
                    .withKey("byFormulaModification")
                    .withDefaultMessage("New modification by formula on ${equipmentType}")
                    .withValue("equipmentType", modificationInfos.getIdentifiableType().name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (equipmentNotModifiedCount == 0 && equipmentNotFoundCount == 0) {
                subReporter.report(Report.builder()
                        .withKey("byFormulaModificationALL")
                        .withDefaultMessage("All equipment have been modified : ${nbEqpt} equipment(s)")
                        .withValue("nbEqpt", equipmentCount)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
                report(formulaSubReporter, formulaReports);
            } else {
                if (equipmentNotModifiedCount == equipmentCount) {
                    createReport(subReporter, "byFormulaModificationNone",
                            "No equipment have been modified",
                            TypedValue.ERROR_SEVERITY);
                } else {
                    subReporter.report(Report.builder()
                            .withKey("byFormulaModificationSome")
                            .withDefaultMessage("Some of the equipment have been modified : ${nbChanged} equipment(s) modified and ${nbUnchanged} equipment(s) not modified")
                            .withValue("nbChanged", equipmentCount - equipmentNotModifiedCount)
                            .withValue("nbUnchanged", equipmentNotModifiedCount + equipmentNotFoundCount)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                    report(formulaSubReporter, formulaReports);
                }
            }
        }
    }

    private void report(Reporter formulaSubReporter, List<Report> formulaReports) {
        formulaSubReporter.report(Report.builder()
                .withKey("appliedFormulasModifications")
                .withDefaultMessage("  Formulas")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        formulaReports.forEach(formulaSubReporter::report);
    }

    private void applyFormulaOnFilterEquipments(Network network,
                                                Map<UUID, FilterEquipments> exportFilters,
                                                List<Report> formulaReports,
                                                FormulaInfos formulaInfos,
                                                FilterInfos filterInfos) {
        FilterEquipments filterEquipments = exportFilters.get(filterInfos.getId());

        if (CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes())) {
            formulaReports.add(Report.builder()
                    .withKey("byFormulaModificationFormulaFilter_" + formulaReports.size())
                    .withDefaultMessage("No equipments were found for filter ${filterName}")
                    .withValue("filterName", filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            List<String> notEditableEquipments = new ArrayList<>();
            List<Report> equipmentsReport = new ArrayList<>();
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

    private void createFormulaReports(List<Report> formulaReports, FormulaInfos formulaInfos, FilterInfos filterInfos, FilterEquipments filterEquipments, List<String> notEditableEquipments) {
        if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
            formulaReports.add(Report.builder()
                    .withKey("byFormulaModificationFormulaFilterFailed_" + formulaReports.size())
                    .withDefaultMessage("No equipment(s) have been modified on filter ${filterName}")
                    .withValue("filterName", filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            formulaReports.add(Report.builder()
                    .withKey("byFormulaModificationFormulaFilter_" + formulaReports.size())
                    .withDefaultMessage("Successful application of new modification by formula on filter ${filterName}")
                    .withValue("filterName", filterInfos.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            formulaReports.add(Report.builder()
                    .withKey("numberOfValidEquipment" + formulaReports.size())
                    .withDefaultMessage("      Number of equipment modified : ${nbChanged}")
                    .withValue("nbChanged", filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                formulaReports.add(Report.builder()
                        .withKey("NotEditedEquipmentsFilter_" + formulaReports.size())
                        .withDefaultMessage("       ${nbUnchanged} equipment(s) were not modified")
                        .withValue("nbUnchanged", notEditableEquipments.size())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }
        }

        formulaReports.add(Report.builder()
                .withKey("editedFieldFilter_" + formulaReports.size())
                .withDefaultMessage("      Edited field :${fieldName}")
                .withValue("fieldName", formulaInfos.getEditedField())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
            String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
            formulaReports.add(Report.builder()
                    .withKey("filterEquipmentsNotFound_" + formulaReports.size())
                    .withDefaultMessage("      Equipment not found : ${nbNotFound}")
                    .withValue("nbNotFound", equipmentIds)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    private boolean isEquipmentEditable(Identifiable<?> identifiable,
                                        FormulaInfos formulaInfos,
                                        List<Report> equipmentsReport) {
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
                        equipmentsReport.add(Report.builder()
                                .withKey(EQUIPMENT_MODIFIED_REPORT_ERROR + equipmentsReport.size())
                                .withDefaultMessage("        Cannot modify field ${fieldName} of equipment ${eqptName} : Ratio tab changer is null")
                                .withValue("fieldName", editedField.name())
                                .withValue("eqptName", identifiable.getId())
                                .withSeverity(TypedValue.TRACE_SEVERITY)
                                .build());
                    }
                    yield isEditable;
                }
                case REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION, PHASE_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getPhaseTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(Report.builder()
                                .withKey(EQUIPMENT_MODIFIED_REPORT_ERROR + equipmentsReport.size())
                                .withDefaultMessage("        Cannot modify field ${fieldName} of equipment ${eqptName} : Phase tab changer is null")
                                .withValue("fieldName", editedField.name())
                                .withValue("eqptName", identifiable.getId())
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
                              List<Report> reports,
                              List<String> notEditableEquipments) {
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(identifiable);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(identifiable);
        if (value1 == null || value2 == null) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(identifiable.getId());
            reports.add(Report.builder()
                    .withKey(EQUIPMENT_MODIFIED_REPORT_ERROR + reports.size())
                    .withDefaultMessage("        Cannot modify equipment ${eqptName} : At least one of the value or referenced field is null")
                    .withValue("eqptName", identifiable.getId())
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
                reports.add(Report.builder()
                        .withKey("EquipmentModifiedReport_" + reports.size())
                        .withDefaultMessage("        ${eqptType} id : ${eqptName}, new value of ${fieldName} : ${newValue}")
                        .withValue("eqptType", modificationInfos.getIdentifiableType().name())
                        .withValue("eqptName", identifiable.getId())
                        .withValue("fieldName", formulaInfos.getEditedField())
                        .withValue("newValue", newValue)
                        .withSeverity(TypedValue.TRACE_SEVERITY)
                        .build());
            } catch (Exception e) {
                notEditableEquipments.add(identifiable.getId());
                equipmentNotModifiedCount += 1;
                reports.add(Report.builder()
                        .withKey("EquipmentModifiedReportExceptionf_" + reports.size())
                        .withDefaultMessage("        Cannot modify equipment ${eqptName} : ${msg}")
                        .withValue("eqptName", identifiable.getId())
                        .withValue("msg", e.getMessage())
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
