package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.LoadModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.jupiter.api.Tag;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.UUID;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest
@Tag("IntegrationTest")
public class ModificationElasticsearchTest {
    private static UUID NETWORK_UUID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIF_PARAMS = "&groupUuid=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID + "&reporterId=" + UUID.randomUUID();
    private static final String URI_NETWORK_MODIF = URI_NETWORK_MODIF_BASE + "?networkUuid=" + NETWORK_UUID + URI_NETWORK_MODIF_PARAMS;

    private static final String INITIAL_VARIANT = "InitialState";

    @Autowired
    MockMvc mockMvc;

    @Autowired
    private ObjectMapper mapper;

    @Autowired
    EquipmentInfosService equipmentInfosService;

    @Autowired
    ModificationRepository modificationRepository;

    @MockBean
    NetworkStoreService networkStoreService;

    @MockBean
    private ReportService reportService;

    @Autowired
    private EquipmentInfosRepository equipmentInfosRepository;

    Network network;

    private ObjectWriter objectWriter;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        network = NetworkCreation.create(NETWORK_UUID, true);
        when(networkStoreService.getNetwork(eq(NETWORK_UUID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);

        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @Test
    public void modificationsImpactElasticsearch() throws Exception{
        // load creation - assert name and vl name values
        LoadCreationInfos loadCreationInfos = ModificationCreation.getCreationLoad("v1", "v1Load", "v1load_name", "1.1", LoadType.UNDEFINED);
        String loadCreationJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadCreationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1load"), NETWORK_UUID, INITIAL_VARIANT).get(0).getName(), "v1load_name");
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1load"), NETWORK_UUID, INITIAL_VARIANT).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v1")));

        // load modification - assert name modification
        LoadModificationInfos loadModification = ModificationCreation.getModificationLoad("v1load", null, "v1load_newname", null, null, null, null);
        String loadModificationJson = mapper.writeValueAsString(loadModification);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(loadModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1load"), NETWORK_UUID, INITIAL_VARIANT).get(0).getName(), "v1load_newname");

        // vl modification - assert vl name modification, linked load modification and parent subsation modification
        VoltageLevelModificationInfos vlModification = ModificationCreation.getModificationVoltageLevel("v1", "v1_newname");
        String vlModificationJson = mapper.writeValueAsString(vlModification);
        mockMvc.perform(post(URI_NETWORK_MODIF).content(vlModificationJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        assertEquals(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1"), NETWORK_UUID, INITIAL_VARIANT).get(0).getName(), "v1_newname");
        assertFalse(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1load"), NETWORK_UUID, INITIAL_VARIANT).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v1")));
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("v1load"), NETWORK_UUID, INITIAL_VARIANT).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v1_newname")));
        assertTrue(equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("s1"), NETWORK_UUID, INITIAL_VARIANT).get(0).getVoltageLevels().stream().anyMatch(vl -> vl.getName().equals("v1_newname")));
    }
}
