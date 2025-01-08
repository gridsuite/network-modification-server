package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VariantManager;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@SpringBootTest
@DisableElasticsearch
class ModificationControllerUnitTest {
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_GROUP_MODIF_BASE = "/v1/groups";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    ObjectMapper objectMapper;

    @MockBean
    NetworkStoreService networkStoreService;

    @Autowired
    private ModificationRepository modificationRepository;

    @SpyBean
    private NetworkModificationRepository networkModificationRepository;

    @Captor
    ArgumentCaptor<List<ModificationInfos>> modificationInfosCaptor;

    @Captor
    ArgumentCaptor<NetworkInfos> networkInfoCaptor;

    @Captor
    ArgumentCaptor<ReportInfos> reportInfosCaptor;

    @SpyBean
    private NetworkModificationApplicator networkModificationApplicator;

    @Mock
    private Network network;

    @Mock
    private VariantManager variantManager;

    @Test
    void testCreateModificationWithoutApplying() throws Exception {
        UUID groupUuid = UUID.randomUUID();
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentId("v1b1")
            .equipmentAttributeValue(true)
            .build();

        MvcResult mvcResult = mockMvc.perform(post(URI_NETWORK_MODIF_BASE)
            .param("groupUuid", groupUuid.toString())
            .content(objectMapper.writeValueAsString(switchStatusModificationInfos))
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();

        UUID result = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), UUID.class);

        List<ModificationEntity> modificationEntityList = modificationRepository.findAllByGroupId(groupUuid, false);
        assertEquals(1, modificationEntityList.size());
        EquipmentAttributeModificationEntity<?> modificationEntity = (EquipmentAttributeModificationEntity<?>) modificationEntityList.get(0);
        assertEquals("open", modificationEntity.getAttributeName());
        assertEquals("v1b1", modificationEntity.getEquipmentId());
        assertEquals(result, modificationEntity.getId());

        // since we're not applying the modification, no network should have been loaded
        Mockito.verify(networkStoreService, never()).getNetwork(any(), any());
    }

    @Test
    void testApplyExistingModification() throws Exception {
        // insert modifications in database - we assume they already exist for this test
        UUID groupUuid = UUID.randomUUID();
        List<? extends ModificationEntity> modificationEntities = insertMultipleModifications(groupUuid);

        // define multiple network context
        NetworkModificationContextInfos networkContext1 = new NetworkModificationContextInfos(UUID.randomUUID(), "variant1", UUID.randomUUID(), UUID.randomUUID());
        NetworkModificationContextInfos networkContext2 = new NetworkModificationContextInfos(UUID.randomUUID(), "variant2", UUID.randomUUID(), UUID.randomUUID());

        // apply switchStatusModificationInfos and loadCreationInfos on those network contexts
        MultipleNetworkModificationsInfos multipleNetworkModificationsInfos = new MultipleNetworkModificationsInfos(
            List.of(modificationEntities.get(0).getId(), modificationEntities.get(2).getId()),
            List.of(networkContext1, networkContext2)
        );

        Mockito.doReturn(NetworkModificationResult.builder().applicationStatus(NetworkModificationResult.ApplicationStatus.ALL_OK).build())
            .when(networkModificationApplicator).applyModifications(any(), any(), any());

        when(networkStoreService.getNetwork(any(UUID.class), any(PreloadingStrategy.class))).thenReturn(network);
        when(network.getVariantManager()).thenReturn(variantManager);
        when(variantManager.getVariantIds()).thenReturn(List.of(networkContext1.variantId(), networkContext2.variantId()));

        mockMvc.perform(post(URI_NETWORK_MODIF_BASE + "/apply")
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(multipleNetworkModificationsInfos)))
            .andExpect(status().isOk());

        //  there should be as many "applyModifications" calls as the number of network contexts
        Mockito.verify(networkModificationApplicator, times(multipleNetworkModificationsInfos.networkModificationContextInfos().size()))
            .applyModifications(modificationInfosCaptor.capture(), networkInfoCaptor.capture(), reportInfosCaptor.capture());

        // for each network context
        multipleNetworkModificationsInfos.networkModificationContextInfos().forEach(networkContext -> {
            // check "applyModifications" report infos match networkContext one
            assertTrue(reportInfosCaptor.getAllValues().stream().anyMatch(reportInfos ->
                reportInfos.getReportUuid().equals(networkContext.reportUuid()) && reportInfos.getNodeUuid().equals(networkContext.nodeUuid())
            ));

            // check "applyModifications" network infos match networkContext one
            assertTrue(networkInfoCaptor.getAllValues().stream().anyMatch(networkInfo -> networkInfo.getNetworkUuuid().equals(networkContext.networkUuid())));
        });

        // check "applyModifications" modification uuids match multipleNetworkModificationsInfos one
        modificationInfosCaptor.getAllValues().forEach(modificationInfos ->
            assertThat(multipleNetworkModificationsInfos.modificationsUuid()).usingRecursiveComparison().isEqualTo(List.of(modificationEntities.get(0).getId(), modificationEntities.get(2).getId()))
        );
    }

    @Test
    void testDuplicateModificationsWithoutApplying() throws Exception {
        // prepare test - create modification in origin group
        UUID originGroupUuid = UUID.randomUUID();
        List<? extends ModificationEntity> modificationEntities = insertMultipleModifications(originGroupUuid);

        // duplicate them into target group UUID
        UUID targetGroupUuid = UUID.randomUUID();
        List<UUID> modificationToDuplicateUuids = modificationEntities.stream().map(ModificationEntity::getId).toList();
        MvcResult mvcResult = mockMvc.perform(put(URI_GROUP_MODIF_BASE + "/{groupUuid}", targetGroupUuid.toString())
            .param("action", NetworkModificationController.GroupModificationAction.COPY.name())
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(modificationToDuplicateUuids)))
            .andExpect(status().isOk())
            .andReturn();

        // check returned UUIDs matched what is stored in database
        List<UUID> result = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        List<UUID> expectedResult = modificationRepository.findAllByGroupId(targetGroupUuid, false).stream().map(ModificationEntity::getId).toList();
        assertThat(result).usingRecursiveComparison().isEqualTo(expectedResult);
        assertEquals(3, expectedResult.size());

        // check origin group modifications are still here
        assertEquals(3, modificationRepository.findAllByGroupId(originGroupUuid, false).size());

        // since we're not applying the modification, no network should have been loaded
        Mockito.verify(networkStoreService, never()).getNetwork(any(), any());
    }

    @Test
    void testMoveModificationsWithoutApplying() throws Exception {
        // prepare test - create modification in origin group
        UUID originGroupUuid = UUID.randomUUID();
        List<? extends ModificationEntity> modificationEntities = insertMultipleModifications(originGroupUuid);

        // move them into target group UUID
        UUID targetGroupUuid = UUID.randomUUID();
        List<UUID> modificationToDuplicateUuids = modificationEntities.stream().map(ModificationEntity::getId).toList();
        MvcResult mvcResult = mockMvc.perform(put(URI_GROUP_MODIF_BASE + "/{groupUuid}", targetGroupUuid.toString())
                .param("originGroupUuid", originGroupUuid.toString())
                .param("action", NetworkModificationController.GroupModificationAction.MOVE.name())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(modificationToDuplicateUuids)))
            .andExpect(status().isOk())
            .andReturn();

        // check returned UUIDs matched what is stored in database
        List<UUID> result = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        List<UUID> expectedResult = modificationRepository.findAllByGroupId(targetGroupUuid, false).stream().map(ModificationEntity::getId).toList();
        assertThat(result).usingRecursiveComparison().isEqualTo(expectedResult);
        assertEquals(3, expectedResult.size());

        // check origin group modifications are not here anymore
        assertEquals(0, modificationRepository.findAllByGroupId(originGroupUuid, false).size());

        // since we're not applying the modification, no network should have been loaded
        Mockito.verify(networkStoreService, never()).getNetwork(any(), any());
    }

    @Test
    void testInsertCompositeModificationsWithoutApplying() throws Exception {
        // prepare test - create composite modification that will be used for mocking
        UUID originGroupUuid = UUID.randomUUID();
        List<? extends ModificationEntity> modificationEntities = insertMultipleModifications(originGroupUuid);

        UUID compositeModificationUuid = UUID.randomUUID();
        Mockito.doReturn(List.of(CompositeModificationInfos.builder().modifications(modificationEntities.stream().map(ModificationEntity::toModificationInfos).toList()).build()))
            .when(networkModificationRepository).getCompositeModificationsInfos(List.of(compositeModificationUuid));

        // insert it into target group UUID
        UUID targetGroupUuid = UUID.randomUUID();
        MvcResult mvcResult = mockMvc.perform(put(URI_GROUP_MODIF_BASE + "/{groupUuid}", targetGroupUuid.toString())
                .param("action", NetworkModificationController.GroupModificationAction.INSERT.name())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(List.of(compositeModificationUuid))))
            .andExpect(status().isOk())
            .andReturn();

        // check returned UUID matched what is stored in database
        List<UUID> result = objectMapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        List<UUID> expectedResult = modificationRepository.findAllByGroupId(targetGroupUuid, false).stream().map(ModificationEntity::getId).toList();
        assertThat(result).usingRecursiveComparison().isEqualTo(expectedResult);
        assertEquals(1, expectedResult.size());

        // since we're not the modification, no network should have been loaded
        Mockito.verify(networkStoreService, never()).getNetwork(any(), any());
    }

    private List<? extends ModificationEntity> insertMultipleModifications(UUID groupUuid) {
        // insert modifications in database - we assume they already exist for this test
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentId("v1b1")
            .equipmentAttributeValue(true)
            .build();
        SubstationCreationInfos substationCreationInfos = SubstationCreationInfos.builder()
            .equipmentId("substationId")
            .country(Country.FR)
            .build();
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .equipmentId("load1")
            .equipmentName("loadName")
            .build();
        return networkModificationRepository.saveModificationInfos(groupUuid, List.of(switchStatusModificationInfos, substationCreationInfos, loadCreationInfos));
    }
}
