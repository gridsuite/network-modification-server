/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.EquipmentAttributeModification;
import org.springframework.lang.NonNull;

import java.util.Map;
import java.util.Set;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Equipment attribute modification")
public class EquipmentAttributeModificationInfos extends EquipmentModificationInfos {
    @Schema(description = "Equipment attribute name")
    private String equipmentAttributeName;

    @Schema(description = "Equipment attribute value")
    private Object equipmentAttributeValue;

    @Schema(description = "Equipment type")
    @NonNull
    private IdentifiableType equipmentType;

    @Override
    public AbstractModification toModification() {
        return new EquipmentAttributeModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MODIFICATION_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION.name(), "${EquipmentType} '${EquipmentId}' change",
            Map.of("EquipmentType", new TypedValue(equipmentType.name(), TypedValue.UNTYPED), "EquipmentId", new TypedValue(getId(), TypedValue.UNTYPED)));
    }

    @Override
    public EquipmentAttributeModificationEntity toEntity() {
        return createEntity();
    }

    private <T> EquipmentAttributeModificationEntity<T> createEntity() {
        EquipmentAttributeModificationEntity<?> modification;
        if (equipmentAttributeValue == null) {
            modification = new StringEquipmentAttributeModificationEntity(this);
        } else {
            switch (equipmentAttributeValue.getClass().getSimpleName()) {
                case "String":
                    modification = new StringEquipmentAttributeModificationEntity(this);
                    break;
                case "Boolean":
                    modification = new BooleanEquipmentAttributeModificationEntity(this);
                    break;
                case "Integer":
                    modification = new IntegerEquipmentAttributeModificationEntity(this);
                    break;
                case "Float":
                    modification = new FloatEquipmentAttributeModificationEntity(this);
                    break;
                case "Double":
                    modification = new DoubleEquipmentAttributeModificationEntity(this);
                    break;
                default:
                    if (equipmentAttributeValue.getClass().isEnum()) {
                        modification = new StringEquipmentAttributeModificationEntity(this);
                    } else {
                        throw new PowsyblException("Value type invalid : " + equipmentAttributeValue.getClass().getSimpleName());
                    }
            }
        }

        return (EquipmentAttributeModificationEntity<T>) modification;
    }

    @Override
    public void check() {
        super.check();
        if (equipmentType == IdentifiableType.SWITCH) {
            checkSwitchStatusModificationInfos();
        }
    }

    private void checkSwitchStatusModificationInfos() {
        if (!equipmentAttributeName.equals("open")) {
            throw new NetworkModificationException(EQUIPMENT_ATTRIBUTE_NAME_ERROR, "For switch status, the attribute name is only 'open'");
        }
        Set<Boolean> possibleValues = Set.of(true, false);
        if (!possibleValues.contains(equipmentAttributeValue)) {
            throw new NetworkModificationException(EQUIPMENT_ATTRIBUTE_VALUE_ERROR, "For switch status, the attribute values are only " + possibleValues);
        }
    }
}
