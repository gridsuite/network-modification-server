package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.modifications.NetworkStoreListener;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class NetworkStoreListenerTest {

    @ParameterizedTest
    @MethodSource("provideArgumentsForWorstStatus")
    void worstStatus(NetworkModificationResult.ApplicationStatus firstStatus, NetworkModificationResult.ApplicationStatus secondStatus) {
        assertEquals(secondStatus, NetworkStoreListener.worstStatus(firstStatus, secondStatus));
    }

    private static Stream<Arguments> provideArgumentsForWorstStatus() {
        return Stream.of(
                Arguments.of(NetworkModificationResult.ApplicationStatus.ALL_OK,
                        NetworkModificationResult.ApplicationStatus.WITH_WARNINGS),
                Arguments.of(NetworkModificationResult.ApplicationStatus.ALL_OK,
                        NetworkModificationResult.ApplicationStatus.WITH_ERRORS),
                Arguments.of(NetworkModificationResult.ApplicationStatus.WITH_WARNINGS,
                        NetworkModificationResult.ApplicationStatus.WITH_ERRORS)
        );
    }
}