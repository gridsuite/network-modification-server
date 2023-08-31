/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

/**
 * @author Achour Berrahma <achour.berrahma at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@ToString
@EqualsAndHashCode
public class SubstationInfos {
    private String id;
    private String name;
}
