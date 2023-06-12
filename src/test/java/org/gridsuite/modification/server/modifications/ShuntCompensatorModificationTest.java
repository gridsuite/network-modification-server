/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.MatcherShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;

import java.util.UUID;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class ShuntCompensatorModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ShuntCompensatorModificationInfos.builder()
                .equipmentId("v2shunt")
                .shuntCompensatorType(new AttributeModification<>(ShuntCompensatorType.CAPACITOR, OperationType.SET))
                .qAtNominalV(new AttributeModification<>(15.0, OperationType.SET))
                .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                .build();

    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return ShuntCompensatorModificationInfos.builder()
                .equipmentId("v2shunt")
                .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                .susceptancePerSection(new AttributeModification<>(0.5, OperationType.SET))
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherShuntCompensatorModificationInfos.createMatcherShuntCompensatorModificationInfos((ShuntCompensatorModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        var shuntCompensator = getNetwork().getShuntCompensator("v2shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(2.9629E-4, model.getBPerSection(), 0.0001);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        var shuntCompensator = getNetwork().getShuntCompensator("v2shunt");
        var model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        assertNotNull(model);
        assertEquals(1.0, model.getBPerSection(), 0);
    }
}
