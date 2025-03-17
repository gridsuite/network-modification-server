package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VariantManagerConstants;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.TombstonedEquipmentInfosRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;
import java.util.UUID;

import static com.powsybl.iidm.network.VariantManagerConstants.INITIAL_VARIANT_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
@SpringBootTest
@Tag("IntegrationTest")
class ModificationElasticsearchTest {
    private static final UUID NETWORK_UUID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + NETWORK_UUID + URI_NETWORK_MODIF_PARAMS;

    private static final String NEW_VARIANT = "NewVariant";
    private static final String NEW_VARIANT_2 = "NewVariant2";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper mapper;

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Autowired
    private ModificationRepository modificationRepository;

    @MockBean
    private NetworkStoreService networkStoreService;

    @MockBean
    private ReportService reportService;

    @Autowired
    private EquipmentInfosRepository equipmentInfosRepository;

    @Autowired
    private TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    private Network network;

    @BeforeEach
    public void setUp() {
        network = NetworkCreation.create(NETWORK_UUID, true);
        when(networkStoreService.getNetwork(eq(NETWORK_UUID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);

        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @Test
    void testModificationsToImpactElasticsearch() throws Exception {
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NEW_VARIANT);
        network.getVariantManager().setWorkingVariant(NEW_VARIANT);

        // load creation - assert name and vl name values
        LoadCreationInfos loadCreationInfos = ModificationCreation.getCreationLoad("v1", "v1Load", "v1load_name", "1.1", LoadType.UNDEFINED);
        String loadCreationJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals("v1load_name", equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1Load"), NETWORK_UUID, NEW_VARIANT).get(0).getName());
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1Load"), NETWORK_UUID, NEW_VARIANT).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v1")));

        // load modification - assert name modification
        LoadModificationInfos loadModification = ModificationCreation.getModificationLoad("v1Load", null, "v1load_newname", null, null, null, null);
        String loadModificationJson = mapper.writeValueAsString(loadModification);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals("v1load_newname", equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1Load"), NETWORK_UUID, NEW_VARIANT).get(0).getName());
    }

    @Test
    void testModificationsNotIndexed() throws Exception {
        // load creation in INITIAL_VARIANT_ID
        LoadCreationInfos loadCreationInfos = ModificationCreation.getCreationLoad("v1", "v1Load", "v1load_name", "1.1", LoadType.UNDEFINED);
        String loadCreationJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals("v1load_name", equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1Load"), NETWORK_UUID, INITIAL_VARIANT_ID).get(0).getName());
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1Load"), NETWORK_UUID, INITIAL_VARIANT_ID).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v1")));

        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NEW_VARIANT);
        network.getVariantManager().setWorkingVariant(NEW_VARIANT);

        // load modification that modify load type is not indexed
        LoadModificationInfos loadModification = ModificationCreation.getModificationLoad("v1Load", null, null, null, LoadType.FICTITIOUS, null, null);
        String loadModificationJson = mapper.writeValueAsString(loadModification);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertTrue(equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NEW_VARIANT).isEmpty());

        // voltage level name modification modifies only equipments indexed contained in the voltage level (not switch, bbs, etc)
        VoltageLevelModificationInfos vlModification = ModificationCreation.getModificationVoltageLevel("v1", "newVlName");
        String vlModificationJson = mapper.writeValueAsString(vlModification);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(vlModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals(11, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NEW_VARIANT).size());
    }

    @Test
    void testEquipmentTypesCreationNotIndexed() throws Exception {
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NEW_VARIANT);
        network.getVariantManager().setWorkingVariant(NEW_VARIANT);

        // vl creation should only index vl, not switch or bbs
        VoltageLevelCreationInfos volageLevelCreationInfos = ModificationCreation.getCreationVoltageLevel("s1", "v1test", "v1test");
        String voltageLevelCreationJson = mapper.writeValueAsString(volageLevelCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(voltageLevelCreationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals(1, equipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NEW_VARIANT).size());
    }

    @Test
    void testModifyGenThenDeleteVl() throws Exception {
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NEW_VARIANT);
        network.getVariantManager().setWorkingVariant(NEW_VARIANT);

        // modify generator in the new variant
        GeneratorModificationInfos generatorModificationInfos = ModificationCreation.getModificationGenerator("idGenerator", "modifiedGeneratorName");
        String generatorModificationJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(generatorModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals("modifiedGeneratorName", equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("idGenerator"), NETWORK_UUID, NEW_VARIANT).get(0).getName());
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("idGenerator"), NETWORK_UUID, NEW_VARIANT).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v2")));

        //then delete the voltage level containing the generator we just modified
        EquipmentDeletionInfos voltageLevelDeletionInfos = EquipmentDeletionInfos.builder().stashed(false).equipmentType(IdentifiableType.VOLTAGE_LEVEL).equipmentId("v2").build();
        String substationDeletionJson = mapper.writeValueAsString(voltageLevelDeletionInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(substationDeletionJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v2"), NETWORK_UUID, NEW_VARIANT).isEmpty());

        //check that the generator is also deleted and that it's present in the tombstonedEquipment in elastic
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("idGenerator"), NETWORK_UUID, NEW_VARIANT).isEmpty());
        assertTrue(tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(NETWORK_UUID, NEW_VARIANT).stream().anyMatch(tombstonedEquipmentInfos -> tombstonedEquipmentInfos.getId().equals("idGenerator")));

    }

    @Test
    void testVLModificationsToImpactElasticsearch() throws Exception {
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NEW_VARIANT);
        network.getVariantManager().setWorkingVariant(NEW_VARIANT);

        // vl modification - assert vl name modification, linked load modification and parent subsation modification
        VoltageLevelModificationInfos vlModification = ModificationCreation.getModificationVoltageLevel("v1", "v1_newname");
        String vlModificationJson = mapper.writeValueAsString(vlModification);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(vlModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        checkSomeEquipmentsVoltageLevel(NETWORK_UUID, NEW_VARIANT, "v1_newname");

        // in a new variant, load modification - assert name modification - assert old data are still here
        network.getVariantManager().cloneVariant(NEW_VARIANT, NEW_VARIANT_2);
        network.getVariantManager().setWorkingVariant(NEW_VARIANT_2);
        // vl modification - assert vl name modification, linked load modification and parent subsation modification
        VoltageLevelModificationInfos vlModification2 = ModificationCreation.getModificationVoltageLevel("v1", "v1_newname_2");
        String vlModification2Json = mapper.writeValueAsString(vlModification2);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(vlModification2Json).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();

        checkSomeEquipmentsVoltageLevel(NETWORK_UUID, NEW_VARIANT, "v1_newname");
        checkSomeEquipmentsVoltageLevel(NETWORK_UUID, NEW_VARIANT_2, "v1_newname_2");
    }

    private void checkSomeEquipmentsVoltageLevel(UUID networkUuid, String variantId, String voltageLevelName) {
        // assert targeted voltage level has been updated
        assertEquals(voltageLevelName, equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1"), networkUuid, variantId).get(0).getName());

        // assert linked load has been updated (linked connectable)
        assertTrue(checkEquipmentHasVoltageLevelWithName(networkUuid, variantId, "v1Load", voltageLevelName));

        // assert switch has not been indexed
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1Switch"), networkUuid, variantId).isEmpty());

        // assert parent substation has been updated
        assertTrue(checkEquipmentHasVoltageLevelWithName(networkUuid, variantId, "s1", voltageLevelName));
    }

    private boolean checkEquipmentHasVoltageLevelWithName(UUID networkUuid, String variantId, String equipmentId, String voltageLevelName) {
        return equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of(equipmentId), networkUuid, variantId).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals(voltageLevelName));
    }
}
