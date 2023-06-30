package org.gridsuite.modification.server.entities.equipment.deletion;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.AbstractEquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.HvdcLccDeletionInfos;

import javax.persistence.*;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "hvdcLccDeletion")
public class HvdcLccDeletionEntity extends AbstractEquipmentDeletionEntity {
    @ElementCollection
    @CollectionTable(name = "shuntCompensatorsSide1")
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide1;

    @ElementCollection
    @CollectionTable(name = "shuntCompensatorsSide2")
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide2;

    @Override
    public AbstractEquipmentDeletionInfos toSpecificInfos() {
        return shuntCompensatorsSide1 != null && !shuntCompensatorsSide1.isEmpty() ||
            shuntCompensatorsSide2 != null && !shuntCompensatorsSide2.isEmpty() ?
            new HvdcLccDeletionInfos(shuntCompensatorsSide1, shuntCompensatorsSide2) : null;
    }
}
