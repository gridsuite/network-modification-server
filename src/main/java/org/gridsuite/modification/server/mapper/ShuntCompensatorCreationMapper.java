/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEntity;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class ShuntCompensatorCreationMapper extends ModificationMapper<ShuntCompensatorCreationInfos, ShuntCompensatorCreationEntity> {
    public ShuntCompensatorCreationMapper() {
        super(ShuntCompensatorCreationEntity.class, ShuntCompensatorCreationInfos.class);
    }
}
