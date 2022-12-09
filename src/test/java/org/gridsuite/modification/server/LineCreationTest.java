package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherLineCreationInfos.createMatcherLineCreationInfos;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LineCreationTest extends AbstractNetworkModificationTest {

    public void testCreate() throws Exception {

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "1.1" and
        //         voltage level "v2" and busbar section "1.A"
        LineCreationInfos modificationToCreate = buildLineCreationInfos();
        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        mockMvc.perform(post(URI_NETWORK_MODIF).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(createdModification, createMatcherLineCreationInfos(modificationToCreate));
        assertNotNull(network.getLine("idLine"));  // line was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        modificationToCreate.setEquipmentId("idLine2");
        modificationToCreate.setEquipmentName("nameLine2");
        MvcResult mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_VARIANT).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> modifications = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertNotNull(modifications);
        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getLine("idLine2"));  // line was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    public void testRead() throws Exception {

        LineCreationInfos modificationToRead = buildLineCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToRead.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        MvcResult mvcResult = mockMvc.perform(get(URI_NETWORK_MODIF_GET_PUT + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        LineCreationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() {
        });

        assertThat(receivedModification, createMatcherLineCreationInfos(modificationToRead));
    }

    public void testUpdate() throws Exception {

        LineCreationInfos modificationToUpdate = buildLineCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToUpdate.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        modificationToUpdate = buildLineCreationInfosBis();
        String modificationToUpdateJson = mapper.writeValueAsString(modificationToUpdate);

        mockMvc.perform(put(URI_NETWORK_MODIF_GET_PUT + modificationUuid).content(modificationToUpdateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos updatedModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);

        assertThat(updatedModification, createMatcherLineCreationInfos(modificationToUpdate));
        //TODO is it normal ?
//        assertNotNull(network.getLine("idLineEdited"));
        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    public void testDelete() throws Exception {

        LineCreationInfos modificationToDelete = buildLineCreationInfos();

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationToDelete.toEntity()));
        UUID modificationUuid = modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0).getUuid();

        mockMvc.perform(delete(URI_NETWORK_MODIF)
                        .queryParam("groupUuid", TEST_GROUP_ID.toString())
                        .queryParam("uuids", modificationUuid.toString()))
                .andExpect(status().isOk()).andReturn();

        List<ModificationInfos> storedModifications = modificationRepository.getModifications(TEST_GROUP_ID, false, true);

        assertTrue(storedModifications.isEmpty());
        assertNull(network.getLine("idLine"));
    }

    // old tests moved here
    @Test
    public void testCreateWithErrors() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LineCreationInfos lineCreationInfos = new LineCreationEntity(
                "idLine4edited",
                "nameLine4edited",
                110.0,
                110.0,
                15.0,
                15.,
                25.,
                25.,
                "v2",
                "1A",
                "v1",
                "1.1",
                5.,
                5.,
                "cn13",
                ConnectablePosition.Direction.TOP,
                "cn23",
                ConnectablePosition.Direction.BOTTOM)
                .toModificationInfos();
        lineCreationInfos.setType(ModificationType.LINE_CREATION);
        lineCreationInfos.setDate(ZonedDateTime.now());

        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BAD_NETWORK).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        lineCreationInfos.setEquipmentId("");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "Invalid id ''").getMessage());

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setSeriesResistance(Double.NaN);
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': r is invalid").getMessage());

        lineCreationInfos.setSeriesResistance(100.0);
        lineCreationInfos.setSeriesReactance(Double.NaN);
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': x is invalid").getMessage());
    }

    @Test
    public void testCreateLineInBusBreaker() throws Exception {

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .build();

        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(createdModification, createMatcherLineCreationInfos(lineCreationInfos));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineInBusBreakerWithErrors() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("notFoundBus")
                .build();

        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());
    }

    @Test
    public void testCreateLineInMixedTypology() throws Exception {

        // create new line in voltage levels with node breaker topology and bus breaker topology
        // between voltage level "v1" and busbar section "1.1" type NODE_BREAKER and
        //         voltage level "v2" and busbar section "bus2 type BUS_BREAKER"
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .connectionName1("cn1Line1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2Line1")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build();

        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF_MIXED_TOPO).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(createdModification, createMatcherLineCreationInfos(lineCreationInfos));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineInMixedTypologyWithErrors() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("notFoundBus")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .connectionName1("cn1Line1")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2Line1")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build();

        //create line with errors
        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_MIXED_TOPO).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage());

        // same
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setBusOrBusbarSectionId2("notFoundBus");
        lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfos);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_MIXED_TOPO).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());
    }

    @Test
    public void testCreateLineOptionalParameters() throws Exception {

        // create new line without shunt conductance or reactance
        LineCreationInfos lineCreationInfosNoShunt = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .build();

        String lineCreationInfosNoShuntJson = mapper.writeValueAsString(lineCreationInfosNoShunt);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosNoShuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(createdModification, createMatcherLineCreationInfos(lineCreationInfosNoShunt));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters2() throws Exception {

        // create new line without shunt conductance or reactance
        LineCreationInfos lineCreationInfosNoShunt = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine1")
                .equipmentName("nameLine1")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .build();

        lineCreationInfosNoShunt.setShuntConductance1(50.0);
        lineCreationInfosNoShunt.setShuntConductance2(null);
        lineCreationInfosNoShunt.setShuntSusceptance1(null);
        lineCreationInfosNoShunt.setShuntSusceptance2(60.0);

        String lineCreationInfosNoShuntJson = mapper.writeValueAsString(lineCreationInfosNoShunt);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosNoShuntJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(createdModification, createMatcherLineCreationInfos(lineCreationInfosNoShunt));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters3() throws Exception {

        LineCreationInfos lineCreationInfosPermanentLimitOK = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(1.0).build())
                .build();

        String lineCreationInfosPermanentLimitOKJson = mapper.writeValueAsString(lineCreationInfosPermanentLimitOK);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosPermanentLimitOKJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(createdModification, createMatcherLineCreationInfos(lineCreationInfosPermanentLimitOK));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters4() throws Exception {

        LineCreationInfos lineCreationInfosPermanentLimitOK = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(null).build())
                .build();

        String lineCreationInfosPermanentLimitOKJson = mapper.writeValueAsString(lineCreationInfosPermanentLimitOK);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosPermanentLimitOKJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        lineCreationInfosPermanentLimitOK.setCurrentLimits2(null); // if permanentLimit is null then no currentLimit created
        LineCreationInfos createdModification = (LineCreationInfos) modificationRepository.getModifications(TEST_GROUP_ID, false, true).get(0);
        assertThat(createdModification, createMatcherLineCreationInfos(lineCreationInfosPermanentLimitOK));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters5() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        LineCreationInfos lineCreationInfosPermanentLimitNOK = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine2")
                .equipmentName("nameLine2")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(0.0).build())
                .build();
        String lineCreationInfosPermanentLimitNOKJson = mapper.writeValueAsString(lineCreationInfosPermanentLimitNOK);
        mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosPermanentLimitNOKJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString  = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine2': permanent limit must be defined and be > 0").getMessage());
    }

    @Test
    public void testCreateLineOptionalParameters6() throws Exception {

        LineCreationInfos lineCreationInfosOK = LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine3")
                .equipmentName("nameLine3")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("bus2")
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(1.0).build())
                .build();

        String lineCreationInfosJson = mapper.writeValueAsString(lineCreationInfosOK);
        mockMvc.perform(post(URI_NETWORK_MODIF_BUS_BREAKER).content(lineCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }

    private LineCreationInfos buildLineCreationInfos() {
        return LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLine")
                .equipmentName("nameLine")
                .seriesResistance(100.0)
                .seriesReactance(100.0)
                .shuntConductance1(10.0)
                .shuntSusceptance1(10.0)
                .shuntConductance2(20.0)
                .shuntSusceptance2(20.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connectionName1("cn1Line")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn2Line")
                .connectionDirection2(ConnectablePosition.Direction.BOTTOM)
                .build();
    }

    private LineCreationInfos buildLineCreationInfosBis() {
        return LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .date(ZonedDateTime.now())
                .equipmentId("idLineEdited")
                .equipmentName("nameLineEdited")
                .seriesResistance(110.0)
                .seriesReactance(110.0)
                .shuntConductance1(15.0)
                .shuntSusceptance1(15.0)
                .shuntConductance2(25.0)
                .shuntSusceptance2(25.0)
                .voltageLevelId1("v2")
                .busOrBusbarSectionId1("1A")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(5.).build())
                .connectionName1("cn1LineEdited")
                .connectionDirection1(ConnectablePosition.Direction.BOTTOM)
                .connectionName2("cn2LineEdited")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build();
    }
}
