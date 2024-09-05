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
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.*;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR;

public class ByFormulaModification extends AbstractByFilterModification {
    private final ByFormulaModificationInfos modificationInfos;

    public ByFormulaModification(ByFormulaModificationInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getModificationLabel() {
        return "modification by formula";
    }

    @Override
    public ModificationInfos getModificationInfos() {
        return modificationInfos;
    }

    @Override
    public IdentifiableType getIdentifiableType() {
        return modificationInfos.getIdentifiableType();
    }

    @Override
    public List<AbstractModificationByFilterInfos> getModificationByFilterInfosList() {
        return Collections.unmodifiableList(modificationInfos.getFormulaInfosList());
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return BY_FORMULA_MODIFICATION_ERROR;
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
                    .withMessageTemplate(KEY_EQUIPMENT_MODIFIED_ERROR + reports.size(), "        Cannot modify equipment ${" + KEY_EQPT_NAME + "} : At least one of the value or referenced field is null")
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
            default -> throw new NetworkModificationException(BY_FORMULA_MODIFICATION_ERROR, "Unsupported equipment");
        }
        return newValue;
    }

    private Double applyOperation(Operator operator, @Nonnull Double value1, @Nonnull Double value2) {
        return switch (operator) {
            case ADDITION -> value1 + value2;
            case SUBTRACTION -> value1 - value2;
            case MULTIPLICATION -> value1 * value2;
            case DIVISION -> value1 / value2;
            case PERCENTAGE -> value1 * (value2 / 100);
        };
    }
}
