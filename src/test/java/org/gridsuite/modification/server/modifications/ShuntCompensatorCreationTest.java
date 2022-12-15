package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUS_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherShuntCompensatorCreationInfos.createMatcher;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class ShuntCompensatorCreationTest extends AbstractNetworkModificationTest {

    @Override
    public void testCreate() throws Exception {

        ShuntCompensatorCreationInfos modificationToCreate = buildShuntCompensatorCreationInfos();
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        mockMvc.perform(post(URI_NETWORK_MODIF).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        ShuntCompensatorCreationInfos createdModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(createdModification, createMatcher(modificationToCreate));
        assertNotNull(network.getShuntCompensator("shuntOneId"));  // shunt compensator was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    public void testCreateWithError() throws Exception {

        ShuntCompensatorCreationInfos modificationToCreate = buildShuntCompensatorCreationInfos();
        // Current number of sections above maximum allowed
        modificationToCreate.setIsIdenticalSection(false);
        modificationToCreate.setCurrentNumberOfSections(6);
        modificationToCreate.setMaximumNumberOfSections(2);
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        mockMvc.perform(post(URI_NETWORK_MODIF).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError());
    }

    @Override
    public void testRead() throws Exception {

        ShuntCompensatorCreationInfos modificationToRead = buildShuntCompensatorCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToRead.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        MvcResult mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        ShuntCompensatorCreationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() {
        });

        assertThat(receivedModification, createMatcher(modificationToRead));
    }

    @Override
    public void testUpdate() throws Exception {

        ShuntCompensatorCreationInfos modificationToUpdate = buildShuntCompensatorCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToUpdate.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        modificationToUpdate = buildShuntCompensatorCreationInfosUpdate();

        String modificationToUpdateJson = mapper.writeValueAsString(modificationToUpdate);

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationUuid).content(modificationToUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        ShuntCompensatorCreationInfos updatedModification = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(updatedModification, createMatcher(modificationToUpdate));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Override
    public void testDelete() throws Exception {

        ShuntCompensatorCreationInfos modificationToDelete = buildShuntCompensatorCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToDelete.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        mockMvc.perform(delete(URI_NETWORK_MODIF)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", modificationUuid.toString()))
                .andExpect(status().isOk()).andReturn();

        List<ModificationInfos> storedModifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);

        assertTrue(storedModifications.isEmpty());
        assertNull(network.getShuntCompensator("shuntOneId"));
    }

    @Override
    public void testCopy() throws Exception {

        ShuntCompensatorCreationInfos modificationToCopy = buildShuntCompensatorCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToCopy.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        mockMvc.perform(put(COPY_URI_STRING)
                        .content(mapper.writeValueAsString(List.of(modificationUuid)))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        List<ShuntCompensatorCreationInfos> modifications = modificationRepository
                .getModifications(TEST_GROUP_ID, false, true)
                .stream().map(ShuntCompensatorCreationInfos.class::cast).collect(Collectors.toList());

        assertEquals(2, modifications.size());
        assertThat(modifications.get(0), createMatcher(modificationToCopy));
        assertThat(modifications.get(1), createMatcher(modificationToCopy));
    }

    // old test moved here
    @Test
    public void testCreateShuntCompensatorInBusBreaker() throws Exception {

        ShuntCompensatorCreationInfos shunt = ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("shuntOneId")
                .equipmentName("hopOne")
                .currentNumberOfSections(4)
                .maximumNumberOfSections(9)
                .susceptancePerSection(1.)
                .isIdenticalSection(true)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("bus2")
                .connectionName("cn2")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();

        String shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        ShuntCompensatorCreationInfos bsmlrShuntCompensator = (ShuntCompensatorCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(bsmlrShuntCompensator, createMatcher(shunt));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        shunt.setEquipmentId("shuntTwoId");
        shunt.setEquipmentName("hopTwo");
        shunt.setIsIdenticalSection(false);
        shunt.setCurrentNumberOfSections(6);
        shunt.setMaximumNumberOfSections(2);
        shunt.setSusceptancePerSection(2.);
        shunt.setVoltageLevelId("v1");
        shunt.setBusOrBusbarSectionId("bus1");
        shunt.setUuid(bsmlrShuntCompensator.getUuid());
        shuntJson = mapper.writeValueAsString(shunt);
        // it works with maximumNumberOfSections < currentNumberOfSections ?
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        shunt.setBusOrBusbarSectionId("notFoundBus");
        shuntJson = mapper.writeValueAsString(shunt);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(shuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage()));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);
    }

    private ShuntCompensatorCreationInfos buildShuntCompensatorCreationInfos() {
        return ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("shuntOneId")
                .equipmentName("hop")
                .currentNumberOfSections(4)
                .maximumNumberOfSections(9)
                .susceptancePerSection(1.)
                .isIdenticalSection(true)
                .voltageLevelId("v2")
                .busOrBusbarSectionId("1B")
                .connectionName("cn")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

    private ShuntCompensatorCreationInfos buildShuntCompensatorCreationInfosUpdate() {
        return ShuntCompensatorCreationInfos.builder()
                .type(ModificationType.SHUNT_COMPENSATOR_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("shuntOneIdEdited")
                .equipmentName("hopEdited")
                .currentNumberOfSections(6)
                .maximumNumberOfSections(12)
                .susceptancePerSection(1.)
                .isIdenticalSection(false)
                .voltageLevelId("v4")
                .busOrBusbarSectionId("1.A")
                .connectionName("cnEdited")
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
                .build();
    }

}
