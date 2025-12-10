/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.utils.ModificationUtils;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchRuntimeException;

/**
 * @author David SARTORI <david.sartori_externe at rte-france.com>
 */

class ModificationUtilsTest {

    @Test
    void testCheckMaxQGreaterThanMinQ() {
        var point1 = ReactiveCapabilityCurvePointsInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point1),
                "old KO, new OK: No exception should be thrown");

        var point2 = ReactiveCapabilityCurvePointsInfos.builder().minQ(20.0).maxQ(10.0).build();
        var exception = catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point2),
                "old OK, new KO: ")
        );
        assertThat(exception)
                .hasMessageEndingWith("old OK, new KO: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");

        var point3 = ReactiveCapabilityCurvePointsInfos.builder().minQ(20.0).maxQ(10.0).build();
        exception = catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point3),
                "old null, new KO: ")
        );
        assertThat(exception)
                .hasMessageEndingWith("old null, new KO: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");

        var point4 = ReactiveCapabilityCurvePointsInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point4),
                "old null, new OK: No exception should be thrown");

        var point5 = ReactiveCapabilityCurvePointsInfos.builder().minQ(10.0).maxQ(20.0).build();
        ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point5),
                "old OK, new null: No exception should be thrown");

        var point6 = ReactiveCapabilityCurvePointsInfos.builder().minQ(20.0).maxQ(10.0).build();
        exception = catchRuntimeException(() -> ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(
                Collections.singletonList(point6),
                "old KO, new null: ")
        );
        assertThat(exception)
                .hasMessageEndingWith("old KO, new null: maximum reactive power 10.0 is expected to be greater than or equal to minimum reactive power 20.0");
    }
}
