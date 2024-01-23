/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Injection;
import org.gridsuite.modification.server.dto.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */

@Tag("IntegrationTest")
public abstract class AbstractInjectionModificationTest extends AbstractNetworkModificationTest {
    protected void assertChangeConnectionState(Injection<?> existingEquipment, boolean expectedState) throws Exception {
        assertChangeConnectionState(existingEquipment, (InjectionModificationInfos) buildModification(), expectedState);
    }

    protected void assertChangeConnectionState(Injection<?> existingEquipment, InjectionModificationInfos modificationInfos, boolean expectedState) throws Exception {
        modificationInfos.setConnected(new AttributeModification<>(expectedState, OperationType.SET));

        if (expectedState) {
            if (existingEquipment.getTerminal().isConnected()) {
                existingEquipment.getTerminal().disconnect();
            }
        } else {
            if (!existingEquipment.getTerminal().isConnected()) {
                existingEquipment.getTerminal().connect();
            }
        }
        Assertions.assertEquals(!expectedState, existingEquipment.getTerminal().isConnected());

        String modificationInfosJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // connection state has changed
        Assertions.assertEquals(expectedState, existingEquipment.getTerminal().isConnected());

        // try to modify again => no change on connection state
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        Assertions.assertEquals(expectedState, existingEquipment.getTerminal().isConnected());
    }
}
