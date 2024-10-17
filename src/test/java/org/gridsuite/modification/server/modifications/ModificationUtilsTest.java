/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;

/**
 * @author David SARTORI <david.sartori_externe at rte-france.com>
 */
class ModificationUtilsTest {
    private static final ModificationUtils INSTANCE = ModificationUtils.getInstance();

    @DisplayName("checkMaxQGreaterThanMinQ() success")
    @ParameterizedTest(name = "{1}")
    @MethodSource("successCases")
    void testCheckMaxQGreaterThanMinQSuccess(final ReactiveCapabilityCurveModificationInfos point, final String errorMessage) {
        INSTANCE.checkMaxQGreaterThanMinQ(Collections.singletonList(point), MODIFY_GENERATOR_ERROR, errorMessage);
    }

    private static List<Arguments> successCases() {
        return List.of(
            Arguments.of(ReactiveCapabilityCurveModificationInfos.builder().oldMinQ(2.0).oldMaxQ(1.0).minQ(10.0).maxQ(20.0).build(), "old KO, new OK: No exception should be thrown"),
            Arguments.of(ReactiveCapabilityCurveModificationInfos.builder().minQ(10.0).maxQ(20.0).build(), "old null, new OK: No exception should be thrown"),
            Arguments.of(ReactiveCapabilityCurveModificationInfos.builder().oldMinQ(10.0).oldMaxQ(20.0).build(), "old OK, new null: No exception should be thrown")
        );
    }

    @DisplayName("checkMaxQGreaterThanMinQ() fail")
    @ParameterizedTest(name = "{1}")
    @MethodSource("failureCases")
    void testCheckMaxQGreaterThanMinQFailure(final ReactiveCapabilityCurveModificationInfos point, final String errorMessage) {
        assertThatExceptionOfType(NetworkModificationException.class)
            .isThrownBy(() -> INSTANCE.checkMaxQGreaterThanMinQ(Collections.singletonList(point), MODIFY_GENERATOR_ERROR, errorMessage))
            .withMessage(MODIFY_GENERATOR_ERROR.name() + " : " + errorMessage + "maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0")
            .extracting(NetworkModificationException::getType).isEqualTo(MODIFY_GENERATOR_ERROR);
    }

    private static List<Arguments> failureCases() {
        return List.of(
            Arguments.of(ReactiveCapabilityCurveModificationInfos.builder().oldMinQ(1.0).oldMaxQ(2.0).minQ(20.0).maxQ(10.0).build(), "old OK, new KO: "),
            Arguments.of(ReactiveCapabilityCurveModificationInfos.builder().minQ(20.0).maxQ(10.0).build(), "old null, new KO: "),
            Arguments.of(ReactiveCapabilityCurveModificationInfos.builder().oldMinQ(20.0).oldMaxQ(10.0).build(), "old KO, new null: ")
        );
    }
}
