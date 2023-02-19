/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.impacts;

import com.powsybl.iidm.network.IdentifiableType;
import lombok.Builder;

import java.util.Set;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Builder
public class CollectionElementImpact extends AbstractElementImpact {

    private IdentifiableType elementType;

    private Set<String> elementsIds = Set.of();
}
