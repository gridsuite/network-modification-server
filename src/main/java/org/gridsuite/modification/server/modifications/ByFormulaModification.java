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
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensator;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.GeneratorField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.ShuntCompensatorField;
import org.gridsuite.modification.server.service.FilterService;
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

    public ByFormulaModification(ByFormulaModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
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
            Reporter formulaSubReporter = subReporter.createSubReporter("appliedFormulasModifications", "Formulas");
            List<Report> formulaReports = new ArrayList<>();
            modificationInfos.getFormulaInfosList().forEach(formulaInfos ->
                    formulaInfos.getFilters().forEach(filterInfos ->
                            applyFormulaOnFilterEquipments(network, exportFilters, formulaReports, formulaInfos, filterInfos)));

            createReport(subReporter, "byFormulaModification", "new modification by formula", TypedValue.INFO_SEVERITY);
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

        formulaReports.add(Report.builder()
                .withKey("byFormulaModificationFormulaFilter_" + formulaReports.size())
                .withDefaultMessage(String.format("Successful application of new modification by formula on filter %s",
                        filterInfos.getName()))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        formulaReports.add(Report.builder()
                .withKey("numberOfValidEquipment" + formulaReports.size())
                .withDefaultMessage(String.format("      Number of equipment modified : %s", filterEquipments.getIdentifiableAttributes().size()))
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

        formulaReports.add(Report.builder()
                .withKey("editedFieldFilter_" + formulaReports.size())
                .withDefaultMessage(String.format("      Edited field : %s", formulaInfos.getEditedField()))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        filterEquipments.getIdentifiableAttributes().forEach(attributes -> applyFormula(network,
                attributes.getId(),
                formulaInfos,
                formulaReports));
    }

    @Nullable
    private Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Reporter subReporter, Map<UUID, String> filters) {
        // export filters from filter server
        Map<UUID, FilterEquipments> exportFilters = filterService.getUuidFilterEquipmentsMap(network, filters);

        boolean isValidFilter = ModificationUtils.getInstance().isValidFilter(subReporter, modificationInfos.getErrorType(), exportFilters);
        return isValidFilter ? exportFilters : null;
    }

    private void applyFormula(Network network,
                              String identifiableId,
                              FormulaInfos formulaInfos,
                              List<Report> reports) {
        Identifiable<?> identifiable = network.getIdentifiable(identifiableId);
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(identifiable);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(identifiable);
        final Double newValue = applyOperation(formulaInfos.getOperator(), value1, value2);
        switch (identifiable.getType()) {
            case GENERATOR -> GeneratorField.setNewValue((Generator) identifiable, formulaInfos.getEditedField(), newValue);
            case BATTERY -> BatteryField.setNewValue((Battery) identifiable, formulaInfos.getEditedField(), newValue);
            case SHUNT_COMPENSATOR -> ShuntCompensatorField.setNewValue((ShuntCompensator) identifiable, formulaInfos.getEditedField(), newValue);
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
    }

    private Double applyOperation(Operator operator, Double value1, Double value2) {
        if (value1 == null ||
            value2 == null) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR, "at least one of the value or referenced field is null");
        } else {
            return switch (operator) {
                case ADDITION -> value1 + value2;
                case SUBTRACTION -> value1 - value2;
                case MULTIPLICATION -> value1 * value2;
                case DIVISION -> {
                    if (value2 == 0) {
                        throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR,
                                "there is a division by zero in a formula");
                    } else {
                        yield value1 / value2;
                    }
                }
                case PERCENTAGE -> value1 * (value2 / 100);
            };
        }
    }
}
