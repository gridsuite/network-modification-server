/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;

import lombok.SneakyThrows;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentDeletionInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.util.NestedServletException;

import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThrows;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.powsybl.iidm.network.VariantManagerConstants;

public class EquipmentDeletionTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v1load")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("GENERATOR")
                .equipmentId("idGenerator")
                .build();
    }

    @Override
    protected MatcherEquipmentDeletionInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos((EquipmentDeletionInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getLoad("v1load"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1load", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLoad("v1load"));
    }

    @SneakyThrows
    @Test
    public void testDeleteEquipments() throws Exception {
        MvcResult mvcResult;
        String resultAsString;

        assertTrue(equipmentInfosService.findAllEquipmentInfos(getNetworkId()).isEmpty());
        assertTrue(equipmentInfosService.findAllTombstonedEquipmentInfos(getNetworkId()).isEmpty());

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v1load")
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        // delete load
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> bsmlrEquipmentDeletion = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsmlrEquipmentDeletion.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1load", "LOAD", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 1);

        // load and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getLoad("v1load"));
        assertNull(getNetwork().getSwitch("v1d1"));
        assertNull(getNetwork().getSwitch("v1b1"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1load", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1d1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1b1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // Test delete load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be deleted
        equipmentDeletionInfos.setEquipmentType("LOAD");
        equipmentDeletionInfos.setEquipmentId("v3load");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletions = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(deletions.isEmpty());  // no modifications returned
        assertNotNull(getNetwork().getLoad("v3load"));  // load was not deleted
        testNetworkModificationsCount(getGroupId(), 2);  // new modification stored in the database

        equipmentDeletionInfos.setEquipmentType("LOAD");
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isNotFound()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=notFoundLoad not found or of bad type").getMessage());

        testNetworkModificationsCount(getGroupId(), 3);

        // delete shunt compensator
        equipmentDeletionInfos.setEquipmentType("SHUNT_COMPENSATOR");
        equipmentDeletionInfos.setEquipmentId("v2shunt");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsEquipmentDeletion = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsEquipmentDeletion.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2shunt", "SHUNT_COMPENSATOR", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 4);

        // shunt compensator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getShuntCompensator("v2shunt"));
        assertNull(getNetwork().getSwitch("v2bshunt"));
        assertNull(getNetwork().getSwitch("v2dshunt"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2shunt", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bshunt", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dshunt", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete generator
        equipmentDeletionInfos.setEquipmentType("GENERATOR");
        equipmentDeletionInfos.setEquipmentId("idGenerator");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDeletionNetwork = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDeletionNetwork.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "idGenerator", "GENERATOR", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 5);

        // generator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getGenerator("idGenerator"));
        assertNull(getNetwork().getSwitch("v2bgenerator"));
        assertNull(getNetwork().getSwitch("v2dgenerator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("idGenerator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bgenerator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dgenerator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete line
        equipmentDeletionInfos.setEquipmentType("LINE");
        equipmentDeletionInfos.setEquipmentId("line2");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
             .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDeletionEquipement = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDeletionEquipement.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "line2", "LINE", Set.of("s1", "s2")));

        testNetworkModificationsCount(getGroupId(), 6);

        // line and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getSwitch("v1dl2"));
        assertNull(getNetwork().getSwitch("v1bl2"));
        assertNull(getNetwork().getSwitch("v3dl2"));
        assertNull(getNetwork().getSwitch("v3bl2"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("line2", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dl2", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1bl2", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dl2", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bl2", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete two windings transformer
        equipmentDeletionInfos.setEquipmentType("TWO_WINDINGS_TRANSFORMER");
        equipmentDeletionInfos.setEquipmentId("trf1");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDelEquipement = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDelEquipement.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf1", "TWO_WINDINGS_TRANSFORMER", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 7);

        // 2 windings transformer and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getTwoWindingsTransformer("trf1"));
        assertNull(getNetwork().getSwitch("v1btrf1"));
        // disconnector 'v1dtrf1' was not removed (2wt 'trf1' in double feeder with 3wt 'trf6' in voltage level 'v1')
        assertNull(getNetwork().getSwitch("v1dtrf1"));
        assertNull(getNetwork().getSwitch("v2btrf1"));
        assertNull(getNetwork().getSwitch("v2dtrf1"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("trf1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1btrf1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dtrf1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2btrf1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dtrf1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete three windings transformer
        equipmentDeletionInfos.setEquipmentType("THREE_WINDINGS_TRANSFORMER");
        equipmentDeletionInfos.setEquipmentId("trf6");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsDelEquip = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsDelEquip.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf6", "THREE_WINDINGS_TRANSFORMER", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 8);

        // 3 windings transformer and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getThreeWindingsTransformer("trf6"));
        assertNull(getNetwork().getSwitch("v2btrf6"));
        assertNull(getNetwork().getSwitch("v2dtrf6"));
        assertNull(getNetwork().getSwitch("v4btrf6"));
        assertNull(getNetwork().getSwitch("v4dtrf6"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("trf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1btrf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dtrf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2btrf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dtrf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v4btrf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v4dtrf6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete static var compensator
        equipmentDeletionInfos.setEquipmentType("STATIC_VAR_COMPENSATOR");
        equipmentDeletionInfos.setEquipmentId("v3Compensator");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsEquipement = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsEquipement.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Compensator", "STATIC_VAR_COMPENSATOR", Set.of("s2")));

        testNetworkModificationsCount(getGroupId(), 9);

        // static var compensator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getStaticVarCompensator("v3Compensator"));
        assertNull(getNetwork().getSwitch("v3dCompensator"));
        assertNull(getNetwork().getSwitch("v3bCompensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3Compensator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dCompensator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bCompensator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete battery
        equipmentDeletionInfos.setEquipmentType("BATTERY");
        equipmentDeletionInfos.setEquipmentId("v3Battery");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV3Battery = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV3Battery.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Battery", "BATTERY", Set.of("s2")));

        testNetworkModificationsCount(getGroupId(), 10);

        // battery and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getBattery("v3Battery"));
        assertNull(getNetwork().getSwitch("v3dBattery"));
        assertNull(getNetwork().getSwitch("v3bBattery"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3Battery", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dBattery", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bBattery", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete dangling line
        equipmentDeletionInfos.setEquipmentType("DANGLING_LINE");
        equipmentDeletionInfos.setEquipmentId("v2Dangling");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV2Dangling = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV2Dangling.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2Dangling", "DANGLING_LINE", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 11);

        // dangling line and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getDanglingLine("v2Dangling"));
        assertNull(getNetwork().getSwitch("v2bdangling"));
        assertNull(getNetwork().getSwitch("v2ddangling"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2Dangling", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bdangling", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2ddangling", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete hvdc line
        equipmentDeletionInfos.setEquipmentType("HVDC_LINE");
        equipmentDeletionInfos.setEquipmentId("hvdcLine");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionshHdcLine = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionshHdcLine.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "hvdcLine", "HVDC_LINE", Set.of("s1")));
        testNetworkModificationsCount(getGroupId(), 12);

        // hvdc line has been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getHvdcLine("hvdcLine"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("hvdcLine", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete vsc converter station
        equipmentDeletionInfos.setEquipmentType("HVDC_CONVERTER_STATION");
        equipmentDeletionInfos.setEquipmentId("v2vsc");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV2Vsc = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV2Vsc.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2vsc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 13);

        // vsc converter station and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getVscConverterStation("v2vsc"));
        assertNull(getNetwork().getSwitch("v2bvsc"));
        assertNull(getNetwork().getSwitch("v2dvsc"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2vsc", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bvsc", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dvsc", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete lcc converter station
        equipmentDeletionInfos.setEquipmentType("HVDC_CONVERTER_STATION");
        equipmentDeletionInfos.setEquipmentId("v1lcc");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentDeletionInfos> deletionsV1Lcc = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(deletionsV1Lcc.get(0), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1lcc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(getGroupId(), 14);

        // lcc converter station and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getLccConverterStation("v1lcc"));
        assertNull(getNetwork().getSwitch("v1dlcc"));
        assertNull(getNetwork().getSwitch("v1blcc"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1lcc", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dlcc", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1blcc", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete voltage level
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v5");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        List<EquipmentDeletionInfos> deletionsV5 = modificationRepository.getModifications(getGroupId(), false, true)
                .stream().map(EquipmentDeletionInfos.class::cast).collect(Collectors.toList());
        EquipmentDeletionInfos lastCreatedEquipmentDeletion = deletionsV5.get(deletionsV5.size() - 1);
        assertThat(deletionsV5.get(14), MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v5", "VOLTAGE_LEVEL", Set.of()));
        assertEquals(ModificationType.EQUIPMENT_DELETION, lastCreatedEquipmentDeletion.getType());
        assertEquals("VOLTAGE_LEVEL", lastCreatedEquipmentDeletion.getEquipmentType());
        assertEquals("v5", lastCreatedEquipmentDeletion.getEquipmentId());

        testNetworkModificationsCount(getGroupId(), 15);

        // voltage level and equipments have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getVoltageLevel("v5"));
        assertNull(getNetwork().getBusbarSection("1A1"));
        assertNull(getNetwork().getLoad("v5load"));
        assertNull(getNetwork().getGenerator("v5generator"));
        assertNull(getNetwork().getShuntCompensator("v5shunt"));
        assertNull(getNetwork().getStaticVarCompensator("v5Compensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("1A1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5load", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5generator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5shunt", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5Compensator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete substation
        equipmentDeletionInfos.setEquipmentType("SUBSTATION");
        equipmentDeletionInfos.setEquipmentId("s3");
        equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        List<EquipmentDeletionInfos> deletionsS3 = modificationRepository.getModifications(getGroupId(), false, true)
                .stream().map(EquipmentDeletionInfos.class::cast).collect(Collectors.toList());
        lastCreatedEquipmentDeletion = deletionsS3.get(deletionsS3.size() - 1);
        assertEquals(ModificationType.EQUIPMENT_DELETION, lastCreatedEquipmentDeletion.getType());
        assertEquals("SUBSTATION", lastCreatedEquipmentDeletion.getEquipmentType());
        assertEquals("s3", lastCreatedEquipmentDeletion.getEquipmentId());

        testNetworkModificationsCount(getGroupId(), 16);

        // substation and equipments have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(getNetwork().getSubstation("s3"));
        assertNull(getNetwork().getVoltageLevel("v6"));
        assertNull(getNetwork().getBusbarSection("1B1"));
        assertNull(getNetwork().getLoad("v6load"));
        assertNull(getNetwork().getGenerator("v6generator"));
        assertNull(getNetwork().getShuntCompensator("v6shunt"));
        assertNull(getNetwork().getStaticVarCompensator("v6Compensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("s3", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("1B1", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6load", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6generator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6shunt", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6Compensator", getNetworkId(), VariantManagerConstants.INITIAL_VARIANT_ID));
    }

    @SneakyThrows
    @Test
    public void testOkWhenRemovingIsolatedEquipment() throws Exception {

        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
                .type(ModificationType.EQUIPMENT_DELETION)
                .equipmentType("LOAD")
                .equipmentId("v5load")
                .build();
        String equipmentDeletionInfosJson = mapper.writeValueAsString(equipmentDeletionInfos);

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        mockMvc.perform(post(getNetworkModificationUri()).content(equipmentDeletionInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        var v5 = getNetwork().getVoltageLevel("v5");
        assertNull(v5.getNodeBreakerView().getTerminal(2));
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // delete load (fail because the load is not found)
        EquipmentDeletionInfos equipmentDeletionInfos = (EquipmentDeletionInfos) buildModification();
        equipmentDeletionInfos.setEquipmentId("notFoundLoad");
        mockMvc
                .perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpectAll(status().isNotFound(),
                        content().string(new NetworkModificationException(EQUIPMENT_NOT_FOUND,
                                "Equipment with id=notFoundLoad not found or of bad type").getMessage()));

        // delete voltage level (fail because the vl is connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        assertThrows("\"status\":500,\"error\":\"Internal Server Error\",\"message\":\"The voltage level 'v4' cannot be removed because of a remaining THREE_WINDINGS_TRANSFORMER",
                NestedServletException.class, () -> mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos))
                                .contentType(MediaType.APPLICATION_JSON)));
        assertNotNull(getNetwork().getVoltageLevel("v4"));

        // delete substation (fail because the substations is connected)
        equipmentDeletionInfos.setEquipmentType("VOLTAGE_LEVEL");
        equipmentDeletionInfos.setEquipmentId("v4");
        assertThrows("DELETE_EQUIPMENT_ERROR : The substation s2 is still connected to another substation",
                NestedServletException.class, () -> mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(equipmentDeletionInfos))
                                .contentType(MediaType.APPLICATION_JSON))
                        .andReturn());
        assertNotNull(getNetwork().getSubstation("s2"));
    }
}
