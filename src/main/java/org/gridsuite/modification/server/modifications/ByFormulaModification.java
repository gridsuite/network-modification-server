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
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.GeneratorField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.LoadField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.ShuntCompensatorField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.TwoWindingsTransformerField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.VoltageLevelField;
import org.gridsuite.modification.server.service.FilterService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

public class ByFormulaModification extends AbstractModification {
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

        Map<UUID, FilterEquipments> exportFilters = getUuidFilterEquipmentsMap(network, subReporter, filters);

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

            createReport(subReporter, "byFormulaModification", "New modification by formula", TypedValue.INFO_SEVERITY);
            if (equipmentNotModifiedCount == 0 && equipmentNotFoundCount == 0) {
                createReport(subReporter, "byFormulaModificationALL",
                        String.format("All equipment have been modified : %s equipment(s)", equipmentCount),
                        TypedValue.INFO_SEVERITY);
            } else {
                if (equipmentNotModifiedCount == equipmentCount) {
                    createReport(subReporter, "byFormulaModificationNone",
                            "No equipment have been modified",
                            TypedValue.ERROR_SEVERITY);
                } else {
                    createReport(subReporter, "byFormulaModificationSome",
                            String.format("Some of the equipment have been modified : %s equipment(s) modified and %s equipment(s) not modified",
                                    equipmentCount - equipmentNotModifiedCount, equipmentNotModifiedCount + equipmentNotFoundCount),
                            TypedValue.WARN_SEVERITY);
                }
            }
            formulaSubReporter.report(Report.builder()
                    .withKey("appliedFormulasModifications")
                    .withDefaultMessage("  Formulas")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            formulaReports.forEach(formulaSubReporter::report);
        }
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
                    .withDefaultMessage(String.format("No equipments were found for filter %s",
                            filterInfos.getName()))
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            List<String> notEditableEquipments = new ArrayList<>();
            List<Report> equipmentsReport = new ArrayList<>();
            filterEquipments.getIdentifiableAttributes()
                    .stream()
                    .map(attributes -> network.getIdentifiable(attributes.getId()))
                    .filter(identifiable -> {
                        boolean isEditableEquipment = isEquipmentEditable(identifiable, formulaInfos);
                        if (!isEditableEquipment) {
                            notEditableEquipments.add(identifiable.getId());
                            equipmentNotModifiedCount += 1;
                        }
                        return isEditableEquipment;
                    })
                    .forEach(identifiable -> applyFormula(identifiable, formulaInfos, equipmentsReport, notEditableEquipments));

            if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
                formulaReports.add(Report.builder()
                        .withKey("byFormulaModificationFormulaFilter_" + formulaReports.size())
                        .withDefaultMessage(String.format("No equipment(s) have been modified on filter %s",
                                filterInfos.getName()))
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            } else {
                formulaReports.add(Report.builder()
                        .withKey("byFormulaModificationFormulaFilter_" + formulaReports.size())
                        .withDefaultMessage(String.format("Successful application of new modification by formula on filter %s",
                                filterInfos.getName()))
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

                formulaReports.add(Report.builder()
                        .withKey("numberOfValidEquipment" + formulaReports.size())
                        .withDefaultMessage(String.format("      Number of equipment modified : %s",
                                filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size()))
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

                if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                    formulaReports.add(Report.builder()
                            .withKey("NotEditedEquipmentsFilter_" + formulaReports.size())
                            .withDefaultMessage(String.format("      The following equipment were not modified : %s", String.join(", ", notEditableEquipments)))
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .build());
                }
            }

            formulaReports.add(Report.builder()
                    .withKey("editedFieldFilter_" + formulaReports.size())
                    .withDefaultMessage(String.format("      Edited field : %s", formulaInfos.getEditedField()))
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
                String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
                formulaReports.add(Report.builder()
                        .withKey("filterEquipmentsNotFound_" + formulaReports.size())
                        .withDefaultMessage(String.format("      Equipment not found : %s",
                                equipmentIds))
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }

            formulaReports.addAll(equipmentsReport);
        }
    }

    @Nullable
    private Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Reporter subReporter, Map<UUID, String> filters) {
        // export filters from filter server
        Map<UUID, FilterEquipments> exportFilters = filterService.getUuidFilterEquipmentsMap(network, filters);

        boolean isValidFilter = ModificationUtils.getInstance().isValidFilter(subReporter, modificationInfos.getErrorType(), exportFilters);
        return isValidFilter ? exportFilters : null;
    }

    private boolean isEquipmentEditable(Identifiable<?> identifiable,
                                        FormulaInfos formulaInfos) {
        if (formulaInfos.getEditedField() == null) {
            return false;
        }

        if (identifiable.getType() == IdentifiableType.TWO_WINDINGS_TRANSFORMER) {
            TwoWindingsTransformerField editedField = TwoWindingsTransformerField.valueOf(formulaInfos.getEditedField());
            TwoWindingsTransformer twoWindingsTransformer = (TwoWindingsTransformer) identifiable;
            return switch (editedField) {
                case TARGET_V, RATIO_LOW_TAP_POSITION, RATIO_TAP_POSITION, RATIO_TARGET_DEADBAND -> twoWindingsTransformer.getRatioTapChanger() != null;
                case REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION, PHASE_TARGET_DEADBAND -> twoWindingsTransformer.getPhaseTapChanger() != null;
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
                    .withKey("EquipmentModifiedReport_" + reports.size())
                    .withDefaultMessage(String.format("        Cannot modify equipment %s : At least one of the value or referenced field is null",
                            identifiable.getId()))
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
                        .withDefaultMessage(String.format("        %s id : %s, new value of %s : %s",
                                modificationInfos.getIdentifiableType(),
                                identifiable.getId(),
                                formulaInfos.getEditedField(),
                                newValue))
                        .withSeverity(TypedValue.TRACE_SEVERITY)
                        .build());
            } catch (Exception e) {
                notEditableEquipments.add(identifiable.getId());
                equipmentNotModifiedCount += 1;
                reports.add(Report.builder()
                        .withKey("EquipmentModifiedReport_" + reports.size())
                        .withDefaultMessage(String.format("        Cannot modify equipment %s : %s",
                                identifiable.getId(),
                                e.getMessage()))
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
