/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.impacts;

import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.Set;

/**
 * This class describes an element type network impact
 * This type of network impact only describes an individual impacted item and the list of associated substations
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@AllArgsConstructor(access = AccessLevel.PRIVATE)
@Setter
@Getter
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@ToString
public class SimpleElementImpact extends BaseImpact {
    public enum SimpleImpactType {
        CREATION,
        MODIFICATION,
        DELETION
    }

    private SimpleImpactType impactType;

    /** The impacted element ID */
    private String elementId;

    /** The impacted substation IDs */
    private Set<String> substationIds;
}
