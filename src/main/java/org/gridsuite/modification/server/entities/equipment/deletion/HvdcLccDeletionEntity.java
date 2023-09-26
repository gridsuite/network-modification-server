/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.server.dto.AbstractEquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.HvdcLccDeletionInfos;
import jakarta.persistence.*;
import java.util.List;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "hvdcLccDeletion")
public class HvdcLccDeletionEntity extends AbstractEquipmentDeletionEntity {
    @ElementCollection
    @CollectionTable(name = "HvdcLccDeletionShuntCompensatorsSide1",
        indexes = {@Index(name = "HvdcLccDeletionEntity_shuntCompensatorsSide1_idx1", columnList = "hvdc_lcc_deletion_entity_id")},
        foreignKey = @ForeignKey(name = "HvdcLccDeletionEntity_shuntCompensatorsSide1_fk1"))
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide1;

    @ElementCollection
    @CollectionTable(name = "HvdcLccDeletionShuntCompensatorsSide2",
        indexes = {@Index(name = "HvdcLccDeletionEntity_shuntCompensatorsSide2_idx1", columnList = "hvdc_lcc_deletion_entity_id")},
        foreignKey = @ForeignKey(name = "HvdcLccDeletionEntity_shuntCompensatorsSide2_fk1"))
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide2;

    @Override
    public AbstractEquipmentDeletionInfos toModificationInfos() {
        return CollectionUtils.isNotEmpty(shuntCompensatorsSide1) || CollectionUtils.isNotEmpty(shuntCompensatorsSide2) ?
            new HvdcLccDeletionInfos(shuntCompensatorsSide1, shuntCompensatorsSide2) : null;
    }
}
