/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import lombok.NonNull;

import java.util.UUID;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 *
 * One move of a batch: origin and target groups are given once per request,
 * a null composite designates the group itself.
 */
public record ModificationMoveInfos(
        @NonNull UUID modificationUuid,
        UUID sourceCompositeUuid,
        UUID targetCompositeUuid,
        UUID insertBeforeUuid) { }
