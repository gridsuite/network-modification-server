/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;
import java.util.UUID;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class ApiUtils {

    private ApiUtils() {
        // Should not be instantiated
    }

    public static void getGroupModifications(MockMvc mockMvc, UUID groupUuid) throws Exception {
        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", groupUuid))
            .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
            .andReturn();
    }

    public static void getModification(MockMvc mockMvc, UUID modificationUuid) throws Exception {
        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid))
            .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
            .andReturn();
    }

    public static void postGroups(MockMvc mockMvc, UUID originGroupUuid) throws Exception {
        mockMvc.perform(
            post("/v1/groups")
                .param("groupUuid", UUID.randomUUID().toString())
                .param("duplicateFrom", originGroupUuid.toString())
            )
            .andExpectAll(status().isOk())
            .andReturn();
    }

    public static void putGroupsDuplications(MockMvc mockMvc, UUID originGroupUuid, UUID networkUuid) throws Exception {
        mockMvc.perform(
            put("/v1/groups/{groupUuid}/duplications", UUID.randomUUID())
                .param("networkUuid", networkUuid.toString())
                .param("reporterId", UUID.randomUUID().toString())
                .param("duplicateFrom", originGroupUuid.toString())
                .param("reportUuid", UUID.randomUUID().toString())
                .param("variantId", UUID.randomUUID().toString())
            )
            .andExpectAll(status().isOk())
            .andReturn();
    }

    public static void putGroupsWithCopy(MockMvc mockMvc, List<UUID> modificationUuids, UUID networkUuid) throws Exception {
        mockMvc.perform(
                put("/v1/groups/{groupUuid}", UUID.randomUUID())
                    .param("action", "COPY")
                    .param("networkUuid", networkUuid.toString())
                    .param("reporterId", UUID.randomUUID().toString())
                    .param("reportUuid", UUID.randomUUID().toString())
                    .param("variantId", UUID.randomUUID().toString())
                    .contentType("application/json")
                    .content(new ObjectMapper().writeValueAsString(modificationUuids))
            )
            .andExpectAll(status().isOk())
            .andReturn();
    }
}
