/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class ApiUtils {
    private ApiUtils() {
        throw new IllegalCallerException("Utility class");
    }

    public static List<ModificationInfos> getGroupModifications(MockMvc mockMvc, UUID groupUuid) throws Exception {
        MvcResult mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", groupUuid))
            .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
            .andReturn();
        return getObjectMapper().readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
    }

    public static ModificationInfos getModification(MockMvc mockMvc, UUID modificationUuid) throws Exception {
        MvcResult mvcResult = mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid))
            .andExpectAll(status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
            .andReturn();
        return getObjectMapper().readValue(mvcResult.getResponse().getContentAsString(), ModificationInfos.class);
    }

    public static void postGroups(MockMvc mockMvc, UUID originGroupUuid, UUID targetGroupUuid) throws Exception {
        mockMvc.perform(
                post("/v1/groups")
                    .param("groupUuid", targetGroupUuid.toString())
                    .param("duplicateFrom", originGroupUuid.toString())
            )
            .andExpectAll(status().isOk());
    }

    public static Optional<NetworkModificationResult> putGroupsDuplications(MockMvc mockMvc, UUID originGroupUuid, UUID targetGroupUuid, UUID networkUuid) throws Exception {
        ModificationApplicationContext applicationContext = new ModificationApplicationContext(networkUuid, UUID.randomUUID().toString(), UUID.randomUUID(), UUID.randomUUID(), Set.of());
        String bodyJson = getObjectMapper().writeValueAsString(org.springframework.data.util.Pair.of(List.of(), List.of(applicationContext)));
        MvcResult mvcResult = mockMvc.perform(
                put("/v1/groups/{groupUuid}", targetGroupUuid)
                    .param("action", "COPY")
                    .param("originGroupUuid", originGroupUuid.toString())
                    .content(bodyJson)
                    .contentType(MediaType.APPLICATION_JSON)
            )
            .andExpectAll(status().isOk())
            .andReturn();
        NetworkModificationsResult result = getObjectMapper().readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        return result.modificationResults().isEmpty() ? Optional.empty() : result.modificationResults().get(0);
    }

    public static NetworkModificationsResult putGroupsWithCopy(MockMvc mockMvc, UUID targetGroupUuid, List<UUID> modificationUuids, UUID networkUuid) throws Exception {
        ModificationApplicationContext applicationContext = new ModificationApplicationContext(networkUuid, UUID.randomUUID().toString(), UUID.randomUUID(), UUID.randomUUID(), Set.of());
        String body = getObjectMapper().writeValueAsString(org.springframework.data.util.Pair.of(modificationUuids, List.of(applicationContext)));

        MvcResult mvcResult = mockMvc.perform(
                put("/v1/groups/{groupUuid}", targetGroupUuid)
                    .param("action", "COPY")
                    .contentType("application/json")
                    .content(body)
            )
            .andExpectAll(status().isOk())
            .andReturn();
        return getObjectMapper().readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
    }

    public static Map<UUID, UUID> postNetworkModificationsDuplicate(MockMvc mockMvc, List<UUID> modificationUuids) throws Exception {
        MvcResult mvcResult = mockMvc.perform(
                post("/v1/network-composite-modifications/duplication")
                    .contentType("application/json")
                    .content(new ObjectMapper().writeValueAsString(modificationUuids))
            )
            .andExpectAll(status().isOk())
            .andReturn();
        return getObjectMapper().readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
    }

    public static void deleteGroup(MockMvc mockMvc, UUID groupUuid) throws Exception {
        mockMvc.perform(delete("/v1/groups/{groupUuid}", groupUuid)).andExpectAll(status().isOk());
    }

    public static void deleteStashedInGroup(MockMvc mockMvc, UUID groupUuid) throws Exception {
        mockMvc.perform(delete("/v1/groups/{groupUuid}/stashed-modifications", groupUuid)).andExpectAll(status().isOk());
    }

    public static void deleteNetworkModificationsInGroup(MockMvc mockMvc, UUID groupUuid) throws Exception {
        mockMvc.perform(
                delete("/v1/network-modifications")
                    .param("groupUuid", groupUuid.toString())
            )
            .andExpectAll(status().isOk());
    }

    public static void deleteNetworkModificationsInGroup(MockMvc mockMvc, UUID groupUuid, List<UUID> uuids) throws Exception {
        mockMvc.perform(
                delete("/v1/network-modifications")
                    .param("uuids", uuids.stream().map(Objects::toString).toList().toArray(new String[0]))
                    .param("groupUuid", groupUuid.toString())
            )
            .andExpectAll(status().isOk());
    }

    public static void deleteNetworkModifications(MockMvc mockMvc, List<UUID> uuids) throws Exception {
        mockMvc.perform(
                delete("/v1/network-modifications")
                    .param("uuids", uuids.stream().map(Objects::toString).toList().toArray(new String[0]))
            )
            .andExpectAll(status().isOk());
    }

    public static void stashNetworkModifications(MockMvc mockMvc, List<UUID> uuids) throws Exception {
        mockMvc.perform(
                put("/v1/network-modifications")
                    .param("uuids", uuids.stream().map(Objects::toString).toList().toArray(new String[0]))
                    .param("groupUuid", UUID.randomUUID().toString())
                    .param("stashed", "true")
            )
            .andExpectAll(status().isOk());
    }

    private static ObjectMapper getObjectMapper() {
        return new ObjectMapper().registerModule(new JavaTimeModule());
    }
}
