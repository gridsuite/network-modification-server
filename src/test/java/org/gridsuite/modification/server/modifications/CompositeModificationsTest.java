/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.CompositeModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.reset;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@Tag("IntegrationTest")
public class CompositeModificationsTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD", "v1"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1")
        );
        return CompositeModificationInfos.builder()
                .modificationsList(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD", "v1"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1")
        );
        return CompositeModificationInfos.builder()
                .modificationsList(modifications)
                .stashed(false)
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLoad("idLoad"));
        assertNotNull(getNetwork().getGenerator("idGenerator"));
        assertNotNull(getNetwork().getBattery("idBattery"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() { }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.COMPOSITE_MODIFICATION.name(), createdValues.get("compositeModificationType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertNotNull(ModificationType.COMPOSITE_MODIFICATION.name(), modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        Assertions.assertEquals(ModificationType.COMPOSITE_MODIFICATION.name(), updatedValues.get("compositeModificationType"));
    }

    @Test
    public void testCheckSqlRequestsCount() throws Exception {
        UUID modificationUuid = saveModification(buildModification());
        reset();

        mockMvc.perform(get("/v1/network-modifications/{uuid}", modificationUuid)).andExpectAll(
                        status().isOk(), content().contentType(MediaType.APPLICATION_JSON))
                .andReturn();
        assertSelectCount(7);

        mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications", getGroupId()))
                .andExpect(status().isOk());
        assertSelectCount(15);
    }

    @Test
    public void testAllModificationsSucceeded() throws Exception {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1")
        );
        ModificationInfos modifInfo = CompositeModificationInfos.builder()
                .modificationsList(modifications)
                .stashed(false)
                .build();
        saveModification(modifInfo);
        String compositeCreationJson = mapper.writeValueAsString(modifInfo);

        mockMvc.perform(post(getNetworkModificationUri()).content(compositeCreationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("Composite modification", "compositeCOMPOSITE_MODIFICATION", reportService);
    }

    @Test
    public void testModificationsFailed() throws Exception {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD", "v1"),
                ModificationCreation.getCreationLoad("v1", "idLoad", "nameLoad", "1.1", LoadType.UNDEFINED),
                ModificationCreation.getCreationBattery("v1", "idBattery", "nameBattry", "1.1")
        );
        ModificationInfos modifInfo = CompositeModificationInfos.builder()
                .modificationsList(modifications)
                .stashed(false)
                .build();
        saveModification(modifInfo);
        String compositeCreationJson = mapper.writeValueAsString(modifInfo);

        mockMvc.perform(post(getNetworkModificationUri()).content(compositeCreationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("Composite modification", "compositeCOMPOSITE_MODIFICATIONWarning", reportService);
    }

    @Test
    public void testAllModificationsFailed() throws Exception {
        List<ModificationInfos> modifications = List.of(
                ModificationCreation.getCreationGenerator("v1", "idGenerator", "nameGenerator", "1B", "v2load", "LOAD", "v1")
        );
        ModificationInfos modifInfo = CompositeModificationInfos.builder()
                .modificationsList(modifications)
                .stashed(false)
                .build();
        saveModification(modifInfo);
        String compositeCreationJson = mapper.writeValueAsString(modifInfo);

        mockMvc.perform(post(getNetworkModificationUri()).content(compositeCreationJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("Composite modification", "compositeCOMPOSITE_MODIFICATIONError", reportService);
    }
}
