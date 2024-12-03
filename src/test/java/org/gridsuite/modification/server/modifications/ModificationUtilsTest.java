/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchRuntimeException;
import static org.gridsuite.modification.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;

/**
 * @author David SARTORI <david.sartori_externe at rte-france.com>
 */

class ModificationUtilsTest {

    @Test
    void testCheckMaxQGreaterThanMinQ() {
        var point1 = ReactiveCapabilityCurveCreationInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point1),
                MODIFY_GENERATOR_ERROR,
                "old KO, new OK: No exception should be thrown");

        var point2 = ReactiveCapabilityCurveCreationInfos.builder().minQ(20.0).maxQ(10.0).build();
        var exception = (NetworkModificationException) catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point2),
                MODIFY_GENERATOR_ERROR,
                "old OK, new KO: ")
        );
        assertThat(exception.getType()).isEqualTo(MODIFY_GENERATOR_ERROR);
        assertThat(exception)
                .hasMessageEndingWith("old OK, new KO: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");

        var point3 = ReactiveCapabilityCurveCreationInfos.builder().minQ(20.0).maxQ(10.0).build();
        exception = (NetworkModificationException) catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point3),
                MODIFY_GENERATOR_ERROR,
                "old null, new KO: ")
        );
        assertThat(exception.getType()).isEqualTo(MODIFY_GENERATOR_ERROR);
        assertThat(exception)
                .hasMessageEndingWith("old null, new KO: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");

        var point4 = ReactiveCapabilityCurveCreationInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point4),
                MODIFY_GENERATOR_ERROR,
                "old null, new OK: No exception should be thrown");

        var point5 = ReactiveCapabilityCurveCreationInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point5),
                MODIFY_GENERATOR_ERROR,
                "old OK, new null: No exception should be thrown");

        var point6 = ReactiveCapabilityCurveCreationInfos.builder().minQ(20.0).maxQ(10.0).build();
        exception = (NetworkModificationException) catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point6),
                MODIFY_GENERATOR_ERROR,
                "old KO, new null: ")
        );
        assertThat(exception.getType()).isEqualTo(MODIFY_GENERATOR_ERROR);
        assertThat(exception)
                .hasMessageEndingWith("old KO, new null: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");
    }
}
