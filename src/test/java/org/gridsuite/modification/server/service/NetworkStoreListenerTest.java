/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

@Tag("UnitTest")
class NetworkStoreListenerTest {
    @ParameterizedTest
    @MethodSource("provideArgumentsForWorstStatus")
    void worstStatus(ApplicationStatus firstStatus, ApplicationStatus secondStatus) {
        assertEquals(secondStatus, firstStatus.max(secondStatus));
    }

    private static Stream<Arguments> provideArgumentsForWorstStatus() {
        return Stream.of(
            Arguments.of(ApplicationStatus.ALL_OK, ApplicationStatus.WITH_WARNINGS),
            Arguments.of(ApplicationStatus.ALL_OK, ApplicationStatus.WITH_ERRORS),
            Arguments.of(ApplicationStatus.WITH_WARNINGS, ApplicationStatus.WITH_ERRORS)
        );
    }
}
