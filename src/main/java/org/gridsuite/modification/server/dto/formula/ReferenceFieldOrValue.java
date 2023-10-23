package org.gridsuite.modification.server.dto.formula;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.EquipmentField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.GeneratorField;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ReferenceFieldOrValue {
    private EquipmentField equipmentField;

    private Double value;

    public Double getRefOrValue(Identifiable<?> identifiable) {
        if (value != null) {
            return value;
        }

        IdentifiableType identifiableType = identifiable.getType();
        return switch (identifiableType) {
            case GENERATOR -> GeneratorField.getReferenceValue((Generator) identifiable, (GeneratorField) equipmentField);
            case BATTERY -> BatteryField.getReferenceValue((Battery) identifiable, (BatteryField) equipmentField);
            default -> throw new UnsupportedOperationException("TODO");
        };
    }
}
