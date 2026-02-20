/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import lombok.Getter;

import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Getter
public class CaseResultInfos {
    private final UUID caseResultUuid;

    private final UUID executionUuid;

    private final UUID reportUuid;

    private final UUID resultUuid;

    private final String stepType;

    private final String status;

    public CaseResultInfos(UUID caseResultUuid, UUID executionUuid, UUID reportUuid, UUID resultUuid, String stepType, String status) {
        this.caseResultUuid = caseResultUuid;
        this.executionUuid = executionUuid;
        this.reportUuid = reportUuid;
        this.resultUuid = resultUuid;
        this.stepType = stepType;
        this.status = status;
    }
}
