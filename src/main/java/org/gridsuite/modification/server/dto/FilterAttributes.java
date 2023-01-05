/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.util.Date;
import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class FilterAttributes {
    private String id;
    private Date modificationDate;
    private String equipmentType;
    private List<FilterEquipmentAttributes> filterEquipmentsAttributes;
}
