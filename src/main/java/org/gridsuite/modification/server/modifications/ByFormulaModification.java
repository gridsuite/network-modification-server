package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.GeneratorField;
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
    private ByFormulaModificationInfos modificationInfos;
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
        var filters = modificationInfos.getFormulaInfosList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        Map<UUID, FilterEquipments> exportFilters = getUuidFilterEquipmentsMap(network, subReporter, filters);

        if (exportFilters != null) {
            List<Report> formulaReports = new ArrayList<>();
            modificationInfos.getFormulaInfosList().forEach(formulaInfos -> formulaInfos.getFilters().forEach(filterInfos -> {
                var filterEquipments = exportFilters.get(filterInfos.getId());
                filterEquipments.getIdentifiableAttributes().forEach(attributes -> applyFormula(network,
                        attributes.getId(),
                        formulaInfos, formulaReports));
            }));

            createReport(subReporter, "byFormulaModification", "new modification by formula", TypedValue.INFO_SEVERITY);
            ModificationUtils.getInstance().reportModifications(subReporter,
                    formulaReports,
                    "appliedFormulasModifications",
                    "Formulas : ");
        }
    }

    @Nullable
    private Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Reporter subReporter, Map<UUID, String> filters) {
        // export filters from filter server
        Map<UUID, FilterEquipments> exportFilters = filterService.getUuidFilterEquipmentsMap(network, filters);

        boolean isValidFilter = ModificationUtils.getInstance().isValidFilter(subReporter, modificationInfos.getErrorType(), filters, exportFilters);
        return isValidFilter ? exportFilters : null;
    }

    private void applyFormula(Network network,
                              String identifiableId,
                              FormulaInfos formulaInfos,
                              List<Report> formulaReports) {
        Identifiable<?> identifiable = network.getIdentifiable(identifiableId);
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(identifiable);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(identifiable);
        final Double newValue = applyOperation(formulaInfos.getOperator(), value1, value2);
        switch (identifiable.getType()) {
            case GENERATOR -> GeneratorField.setNewValue((Generator) identifiable,
                    formulaInfos.getEditedField(),
                    newValue);
            case BATTERY -> BatteryField.setNewValue((Battery) identifiable,
                    formulaInfos.getEditedField(),
                    newValue);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR, "Unsupported equipment");
        }

        formulaReports.add(Report.builder()
                .withKey("byFormulaModificationFormula" + formulaReports.size())
                .withDefaultMessage(String.format("successful application of new modification by formula on %s for %s %s",
                        formulaInfos.getEditedField(),
                        identifiable.getType(),
                        identifiable.getId()))
                .withSeverity(TypedValue.INFO_SEVERITY)
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
                                "there is a division by 0 in one formula");
                    } else {
                        yield value1 / value2;
                    }
                }
                case MODULUS -> value1 % value2;
            };
        }
    }
}
