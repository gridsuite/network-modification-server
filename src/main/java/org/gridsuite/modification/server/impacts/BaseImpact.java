/**
  Copyright (c) 2023, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.impacts;

import com.powsybl.iidm.network.IdentifiableType;

import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * This class describes a base impact
 * This is the base type of all network impacts
 *
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Setter
@Getter
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
@ToString
public class BaseImpact {

    private IdentifiableType elementType;
}
