/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ByFormulaModification extends AbstractByFilterModification {
    private final ByFormulaModificationInfos modificationInfos;

    public ByFormulaModification(ByFormulaModificationInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getModificationTypeLabel() {
        return "formula";
    }

    @Override
    public ModificationInfos getModificationInfos() {
        return modificationInfos;
    }

    @Override
    public IdentifiableType getEquipmentType() {
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
    protected boolean preCheckValue(Identifiable<?> equipment, AbstractModificationByFilterInfos modificationByFilterInfos, List<ReportNode> reports, List<String> notEditableEquipments) {
        FormulaInfos formulaInfos = (FormulaInfos) modificationByFilterInfos;
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(equipment);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(equipment);
        if (value1 == null || Double.isNaN(value1) || value2 == null || Double.isNaN(value2)) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(equipment.getId());
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(REPORT_KEY_EQUIPMENT_MODIFIED_ERROR, "        Cannot modify equipment ${" + VALUE_KEY_EQUIPMENT_NAME + "} : At least one of the value or referenced field is null")
                    .withUntypedValue(VALUE_KEY_EQUIPMENT_NAME, equipment.getId())
                    .withSeverity(TypedValue.TRACE_SEVERITY)
                    .build());
            return false;
        }

        if (value2 == 0 && formulaInfos.getOperator() == Operator.DIVISION) {
            equipmentNotModifiedCount += 1;
            notEditableEquipments.add(equipment.getId());
            return false;
        }
        return true;
    }

    @Override
    protected <T> T getNewValue(Identifiable<?> equipment, AbstractModificationByFilterInfos modificationByFilterInfos) {
        FormulaInfos formulaInfos = (FormulaInfos) modificationByFilterInfos;
        Double value1 = formulaInfos.getFieldOrValue1().getRefOrValue(equipment);
        Double value2 = formulaInfos.getFieldOrValue2().getRefOrValue(equipment);
        return (T) applyOperation(formulaInfos.getOperator(), value1, value2);
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
