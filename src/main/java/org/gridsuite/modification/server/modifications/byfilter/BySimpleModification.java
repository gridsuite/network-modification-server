/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BySimpleModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.*;
import org.gridsuite.modification.server.dto.byfilter.simple.PropertyModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.simple.SimpleModificationByFilterInfos;

import java.util.Collections;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BY_SIMPLE_MODIFICATION_ERROR;

public class BySimpleModification extends AbstractByFilterModification {
    private final BySimpleModificationInfos modificationInfos;

    public BySimpleModification(BySimpleModificationInfos modificationInfos) {
        super();
        this.modificationInfos = modificationInfos;
    }

    @Override
    public String getModificationLabel() {
        return "modification by filter";
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
        return Collections.unmodifiableList(modificationInfos.getSimpleModificationInfosList());
    }

    @Override
    public NetworkModificationException.Type getExceptionType() {
        return BY_SIMPLE_MODIFICATION_ERROR;
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
                    ((PropertyModificationByFilterInfos) simpleModificationInfos).getPropertyName(),
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
                default -> throw new NetworkModificationException(BY_SIMPLE_MODIFICATION_ERROR, "Unsupported equipment");
            }
        }
        return simpleModificationInfos.getValue();
    }
}
