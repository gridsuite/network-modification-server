/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import java.util.UUID;

/**
 * Whether a modification is applied on the root network a tag names, as read from the database.
 *
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public record ModificationApplicability(UUID modificationId, String rootNetworkTag, Boolean applicable) {
}
