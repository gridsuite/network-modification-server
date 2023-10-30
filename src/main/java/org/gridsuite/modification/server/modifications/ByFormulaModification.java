package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.GeneratorField;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
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
    public void apply(Network network, Reporter subReporter) {
        // collect all filters from all variations
        var filters = modificationInfos.getFormulaInfosList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));

        // export filters from filter server
        String workingVariantId = network.getVariantManager().getWorkingVariantId();
        UUID uuid = ((NetworkImpl) network).getUuid();
        Map<UUID, FilterEquipments> exportFilters = filterService
                .exportFilters(new ArrayList<>(filters.keySet()), uuid, workingVariantId)
                .stream()
                .peek(t -> t.setFilterName(filters.get(t.getFilterId())))
                .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));

        // collect all filters with wrong equipments ids
        Map<UUID, FilterEquipments> filterWithWrongEquipmentsIds = exportFilters.entrySet().stream()
                .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        Boolean noValidEquipmentId = exportFilters.values().stream()
                .allMatch(filterEquipments -> filterEquipments.getIdentifiableAttributes().isEmpty());

        if (noValidEquipmentId) {
            String errorMsg = modificationInfos.getErrorType() + ": There is no valid equipment ID among the provided filter(s)";
            createReport(subReporter, "invalidFilters", errorMsg, TypedValue.ERROR_SEVERITY);
            return;
        }

        // create report for each wrong filter
        filterWithWrongEquipmentsIds.values().forEach(f -> {
            var equipmentIds = String.join(", ", f.getNotFoundEquipments());
            createReport(subReporter,
                    "filterEquipmentsNotFound_" + f.getFilterName(),
                    String.format("Cannot find the following equipments %s in filter %s", equipmentIds, filters.get(f.getFilterId())),
                    TypedValue.WARN_SEVERITY);
        });

        modificationInfos.getFormulaInfosList().forEach(formulaInfos -> formulaInfos.getFilters().forEach(filterInfos -> {
            var filterEquipments = exportFilters.get(filterInfos.getId());
            filterEquipments.getIdentifiableAttributes().forEach(attributes -> applyFormula(network, attributes.getId(), formulaInfos));
        }));
    }

    private void applyFormula(Network network, String identifiableId, FormulaInfos formulaInfos) {
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
            default -> throw new NetworkModificationException(NetworkModificationException.Type.WRONG_EQUIPMENT_TYPE, "Unsupported equipment");
        }
    }

    private Double applyOperation(Operator operator, Double value1, Double value2) {
        if (value1 == null ||
            value2 == null ||
            value2 == 0 && operator == Operator.DIVISION) {
            throw new UnsupportedOperationException("TODO");
        }

        return switch (operator) {
            case ADDITION -> value1 + value2;
            case SUBTRACTION -> value1 - value2;
            case MULTIPLICATION -> value1 * value2;
            case DIVISION -> value1 / value2;
            case MODULUS -> value1 % value2;
        };
    }
}
