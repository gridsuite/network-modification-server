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
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.UUID;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class ApiUtils {

    private ApiUtils() {
        // Should not be instantiated
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

    private static ObjectMapper getObjectMapper() {
        return new ObjectMapper().registerModule(new JavaTimeModule());
    }
}
