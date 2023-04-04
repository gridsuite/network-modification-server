/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.impacts;

import com.powsybl.iidm.network.IdentifiableType;
import lombok.*;

import java.util.Set;

/**
 * This class describes an element type network impact
 * This type of network impact only describes an individual impacted item and the list of associated subtractions
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Setter
@Getter
@Builder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
@ToString
public class SimpleElementImpact {
    public enum SimpleImpactType {
        CREATION,
        MODIFICATION,
        DELETION
    }

    private SimpleImpactType impactType;

    /** The impacted element ID */
    private String elementId;

    private IdentifiableType elementType;

    /** The impacted substations IDs */
    private Set<String> substationIds;
}
