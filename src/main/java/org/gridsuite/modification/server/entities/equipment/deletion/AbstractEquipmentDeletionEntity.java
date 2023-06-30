/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;
import org.gridsuite.modification.server.dto.AbstractEquipmentDeletionInfos;
import javax.persistence.*;
import java.util.UUID;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */

@Entity
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public abstract class AbstractEquipmentDeletionEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    public abstract AbstractEquipmentDeletionInfos toSpecificInfos();
}
