/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.BusbarConnectionCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.creation.BusbarSectionCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LineSplitWithVoltageLevelEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.BuildFailedPublisherService;
import org.gridsuite.modification.server.service.BuildStoppedPublisherService;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.hamcrest.CoreMatchers;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cloud.stream.binder.test.OutputDestination;
import org.springframework.cloud.stream.binder.test.TestChannelBinderConfiguration;
import org.springframework.http.*;
import org.springframework.messaging.Message;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.ContextHierarchy;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_ERROR;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest(properties = {"spring.data.elasticsearch.enabled=true"})
@ContextHierarchy({@ContextConfiguration(classes = {NetworkModificationApplication.class, TestChannelBinderConfiguration.class})})
public class BuildTest {

    @Autowired
    private MockMvc mockMvc;
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_STOP_BUILD_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID_2 = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID_2 = UUID.randomUUID();

    private static final int TIMEOUT = 1000;

    private static final String VARIANT_ID_2 = "variant_2";

    private ExecutorService executorService = Executors.newCachedThreadPool();

    private CountDownLatch waitStartBuild;
    private CountDownLatch blockBuild;

    @Value("${spring.cloud.stream.bindings.consumeBuild-in-0.destination}")
    private String consumeBuildDestination;

    @Value("${spring.cloud.stream.bindings.consumeCancelBuild-in-0.destination}")
    private String cancelBuildDestination;

    @Value("${spring.cloud.stream.bindings.publishResultBuild-out-0.destination}")
    private String buildResultDestination;

    @Value("${spring.cloud.stream.bindings.publishStoppedBuild-out-0.destination}")
    private String buildStoppedDestination;

    @Value("${spring.cloud.stream.bindings.publishFailedBuild-out-0.destination}")
    private String buildFailedDestination;

    @Autowired
    private OutputDestination output;

    @MockBean
    private NetworkStoreService networkStoreService;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @MockBean
    @Qualifier("reportServer")
    private RestTemplate reportServerRest;

    @Autowired
    private NetworkModificationService networkModificationService;

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Autowired
    private ObjectMapper mapper;
    private ObjectWriter objectWriter;

    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        // create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });

        waitStartBuild = new CountDownLatch(1);
        blockBuild = new CountDownLatch(1);
        when(networkStoreService.getNetwork(TEST_NETWORK_STOP_BUILD_ID)).then((Answer<Network>) invocation -> {
            // Needed so the stop call doesn't arrive too late
            waitStartBuild.countDown();
            blockBuild.await();

            network = NetworkCreation.create(TEST_NETWORK_STOP_BUILD_ID, true);
            return network;
        });

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_NETWORK_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
            .willReturn(new ResponseEntity<>(HttpStatus.OK));
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_NETWORK_STOP_BUILD_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
            .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteVariants(TEST_NETWORK_ID, List.of(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID, VARIANT_ID_2));
    }

    @Test
    public void runBuildForLineSplits() throws  Exception {
        List<ModificationEntity> entities1 = new ArrayList<>();
        entities1.add(modificationRepository.createLineEntity("newLine", "newLine", 1., 2., 3., 4., 5., 6., "v1", "1.1", "v2", "1B", null, null));
        entities1.add(LineSplitWithVoltageLevelEntity.toEntity("line3", 0.32, null, "vl1", "sjb1", "un", "One", "deux", "Two"));
        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);

        List<ModificationEntity> entities2 = new ArrayList<>();
        entities2.add(modificationRepository.createVoltageLevelEntity("vl9", "vl9", 225, "s1",
            List.of(new BusbarSectionCreationEmbeddable("1.1", "1.1", 1, 1),
                new BusbarSectionCreationEmbeddable("1.2", "1.2", 1, 2)),
            List.of(new BusbarConnectionCreationEmbeddable("1.1", "1.2", SwitchKind.BREAKER))));
        modificationRepository.saveModifications(TEST_GROUP_ID_2, entities2);

        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(new GroupAndReportInfos(TEST_GROUP_ID, TEST_REPORT_ID), new GroupAndReportInfos(TEST_GROUP_ID_2, TEST_REPORT_ID_2)),
            new HashSet<>());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(mapper.writeValueAsString(buildInfos)))
                .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        Message<byte[]> resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));

        BuildInfos newBuildInfos = new BuildInfos(NetworkCreation.VARIANT_ID,
            VARIANT_ID_2,
            List.of(),
            new HashSet<>());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(mapper.writeValueAsString(newBuildInfos)))
                .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("", new String(resultMessage.getPayload()));
    }

    @Test
    public void runBuildTest() throws Exception {
        // create modification entities in the database
        List<ModificationEntity> entities1 = new ArrayList<>();
        entities1.add(modificationRepository.createEquipmentAttributeModification("v1d1", "open", true));
        entities1.add(modificationRepository.createEquipmentAttributeModification("line1", "branchStatus", BranchStatus.Status.PLANNED_OUTAGE));
        entities1.add(modificationRepository.createEquipmentAttributeModification("idGenerator", "targetP", 50.));
        entities1.add(modificationRepository.createEquipmentAttributeModification("trf1", "ratioTapChanger.tapPosition", 2));
        entities1.add(modificationRepository.createEquipmentAttributeModification("trf6", "phaseTapChanger1.tapPosition", 0));
        entities1.add(modificationRepository.createLoadCreationEntity("newLoad", "newLoad", LoadType.AUXILIARY, "v1", "1.1", 10., 20.));
        entities1.add(modificationRepository.createSubstationEntity("newSubstation", "newSubstation", Country.FR));

        List<ModificationEntity> entities2 = new ArrayList<>();
        entities2.add(modificationRepository.createGeneratorEntity("newGenerator", "newGenerator", EnergySource.HYDRO, "v2", "1A", 0., 500., 1., 100., 50., true, 225.));
        entities2.add(modificationRepository.createLineEntity("newLine", "newLine", 1., 2., 3., 4., 5., 6., "v1", "1.1", "v2", "1B", null, null));
        entities2.add(modificationRepository.createTwoWindingsTransformerEntity("new2wt", "new2wt", 1., 2., 3., 4., 5., 6., "v1", "1.1", "v2", "1A", null, null));
        entities2.add(modificationRepository.createEquipmentDeletionEntity("v2shunt", "SHUNT_COMPENSATOR"));
        entities2.add(modificationRepository.createGroovyScriptModificationEntity("network.getGenerator('idGenerator').targetP=55\n"));
        entities2.add(modificationRepository.createBranchStatusModificationEntity("line2", BranchStatusModificationInfos.ActionType.TRIP));
        entities2.add(modificationRepository.createVoltageLevelEntity("vl9", "vl9", 225, "s1",
            List.of(new BusbarSectionCreationEmbeddable("1.1", "1.1", 1, 1),
                    new BusbarSectionCreationEmbeddable("1.2", "1.2", 1, 2)),
            List.of(new BusbarConnectionCreationEmbeddable("1.1", "1.2", SwitchKind.BREAKER))));
        entities2.add(modificationRepository.createShuntCompensatorEntity(ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt9")
            .equipmentName("shunt9")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1A")
            .maximumNumberOfSections(2)
            .currentNumberOfSections(2)
            .susceptancePerSection(1.)
            .isIdenticalSection(true)
            .build()));

        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);
        modificationRepository.saveModifications(TEST_GROUP_ID_2, entities2);

        testNetworkModificationsCount(TEST_GROUP_ID, 7);
        testNetworkModificationsCount(TEST_GROUP_ID_2, 8);

        // build VARIANT_ID by cloning network initial variant and applying all modifications in all groups
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(new GroupAndReportInfos(TEST_GROUP_ID, TEST_REPORT_ID), new GroupAndReportInfos(TEST_GROUP_ID_2, TEST_REPORT_ID_2)),
            new HashSet<>());
        String buildInfosJson = objectWriter.writeValueAsString(buildInfos);
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(buildInfosJson))
                .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        Message<byte[]> resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("newSubstation,s1,s2", new String(resultMessage.getPayload()));

        // test all modifications have been made on variant VARIANT_ID
        network.getVariantManager().setWorkingVariant(NetworkCreation.VARIANT_ID);
        assertTrue(network.getSwitch("v1d1").isOpen());
        BranchStatus<Line> branchStatus = network.getLine("line1").getExtension(BranchStatus.class);
        assertNotNull(branchStatus);
        assertEquals(BranchStatus.Status.PLANNED_OUTAGE, branchStatus.getStatus());
        branchStatus = network.getLine("line2").getExtension(BranchStatus.class);
        assertNotNull(branchStatus);
        assertEquals(BranchStatus.Status.FORCED_OUTAGE, branchStatus.getStatus());

        assertEquals(55., network.getGenerator("idGenerator").getTargetP(), 0.1);
        assertEquals(2, network.getTwoWindingsTransformer("trf1").getRatioTapChanger().getTapPosition());
        assertEquals(0, network.getThreeWindingsTransformer("trf6").getLeg1().getPhaseTapChanger().getTapPosition());
        assertEquals(LoadType.AUXILIARY, network.getLoad("newLoad").getLoadType());
        assertEquals(10., network.getLoad("newLoad").getP0(), 0.1);
        assertEquals(20., network.getLoad("newLoad").getQ0(), 0.1);
        assertEquals(EnergySource.HYDRO, network.getGenerator("newGenerator").getEnergySource());
        assertEquals("v2", network.getGenerator("newGenerator").getTerminal().getVoltageLevel().getId());
        assertEquals(500., network.getGenerator("newGenerator").getMaxP(), 0.1);
        assertEquals(100., network.getGenerator("newGenerator").getTargetP(), 0.1);
        assertTrue(network.getGenerator("newGenerator").isVoltageRegulatorOn());
        assertEquals(225., network.getGenerator("newGenerator").getTargetV(), 0.1);
        assertEquals("v1", network.getLine("newLine").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", network.getLine("newLine").getTerminal2().getVoltageLevel().getId());
        assertEquals(4., network.getLine("newLine").getB1(), 0.1);
        assertEquals("v1", network.getTwoWindingsTransformer("new2wt").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", network.getTwoWindingsTransformer("new2wt").getTerminal2().getVoltageLevel().getId());
        assertEquals(2., network.getTwoWindingsTransformer("new2wt").getX(), 0.1);
        assertEquals(5., network.getTwoWindingsTransformer("new2wt").getRatedU1(), 0.1);
        assertNull(network.getShuntCompensator("v2shunt"));
        assertEquals(Country.FR, network.getSubstation("newSubstation").getCountry().orElse(Country.AF));
        assertNotNull(network.getVoltageLevel("vl9"));
        assertNotNull(network.getShuntCompensator("shunt9"));

        // Test that no modifications have been made on initial variant
        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);
        assertFalse(network.getSwitch("v1d1").isOpen());
        assertNull(network.getLine("line1").getExtension(BranchStatus.class));
        assertNull(network.getLine("line2").getExtension(BranchStatus.class));
        assertEquals(42.1, network.getGenerator("idGenerator").getTargetP(), 0.1);
        assertEquals(1, network.getTwoWindingsTransformer("trf1").getRatioTapChanger().getTapPosition());
        assertEquals(0, network.getThreeWindingsTransformer("trf6").getLeg1().getPhaseTapChanger().getTapPosition());
        assertNull(network.getLoad("newLoad"));
        assertNull(network.getGenerator("newGenerator"));
        assertNull(network.getGenerator("newLine"));
        assertNull(network.getGenerator("new2wt"));
        assertNotNull(network.getShuntCompensator("v2shunt"));
        assertNull(network.getSubstation("newSubstation"));
        assertNull(network.getVoltageLevel("vl9"));
        assertNull(network.getShuntCompensator("shunt9"));

        // No new modification entity should have been added to the database
        testNetworkModificationsCount(TEST_GROUP_ID, 7);
        testNetworkModificationsCount(TEST_GROUP_ID_2, 8);

        // Execute another build starting from variant VARIANT_ID to variant VARIANT_ID_2
        // to check
        BuildInfos newBuildInfos = new BuildInfos(NetworkCreation.VARIANT_ID,
            VARIANT_ID_2,
            List.of(),
            new HashSet<>());
        buildInfosJson = objectWriter.writeValueAsString(newBuildInfos);
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(buildInfosJson)
                                 ).andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("", new String(resultMessage.getPayload()));

        List<EquipmentInfos> eqVariant1 = equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).stream().filter(eq -> eq.getVariantId().equals(NetworkCreation.VARIANT_ID)).collect(Collectors.toList());
        List<EquipmentInfos> eqVariant2 = equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).stream().filter(eq -> eq.getVariantId().equals(VARIANT_ID_2)).collect(Collectors.toList());
        assertTrue(eqVariant2.size() > 0);
        assertEquals(eqVariant1.size(), eqVariant2.size());

        List<TombstonedEquipmentInfos> tbseqVariant1 = equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).stream().filter(eq -> eq.getVariantId().equals(NetworkCreation.VARIANT_ID)).collect(Collectors.toList());
        List<TombstonedEquipmentInfos> tbseqVariant2 = equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).stream().filter(eq -> eq.getVariantId().equals(VARIANT_ID_2)).collect(Collectors.toList());
        // v2shunt was deleted from initial variant => v2shunt and the cell switches (breaker and disconnector) have been added as TombstonedEquipmentInfos in ElasticSearch
        assertEquals(3, tbseqVariant1.size());
        assertEquals(tbseqVariant1.size(), tbseqVariant2.size());
        // deactivate some modifications and rebuild VARIANT_ID
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID, true);

        AtomicReference<UUID> lineModificationEntityUuid = new AtomicReference<>();
        AtomicReference<UUID> loadCreationEntityUuid = new AtomicReference<>();
        AtomicReference<UUID> equipmentDeletionEntityUuid = new AtomicReference<>();
        modificationRepository.getModificationsEntities(List.of(TEST_GROUP_ID, TEST_GROUP_ID_2)).forEach(entity -> {
            if (entity.getType().equals(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION.name())) {
                if (((EquipmentAttributeModificationEntity<?>) entity).getEquipmentId().equals("line1")) {
                    lineModificationEntityUuid.set(entity.getId());
                }
            } else if (entity.getType().equals(ModificationType.LOAD_CREATION.name())) {
                if (((LoadCreationEntity) entity).getEquipmentId().equals("newLoad")) {
                    loadCreationEntityUuid.set(entity.getId());
                }
            } else if (entity.getType().equals(ModificationType.EQUIPMENT_DELETION.name())) {
                if (((EquipmentDeletionEntity) entity).getEquipmentId().equals("v2shunt")) {
                    equipmentDeletionEntityUuid.set(entity.getId());
                }
            }
        });

        buildInfos.addModificationToExclude(lineModificationEntityUuid.get());
        buildInfos.addModificationToExclude(loadCreationEntityUuid.get());
        buildInfos.addModificationToExclude(equipmentDeletionEntityUuid.get());
        buildInfosJson = objectWriter.writeValueAsString(buildInfos);

        mockMvc.perform(post(uriString, TEST_NETWORK_ID).content(buildInfosJson)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("newSubstation,s1,s2", new String(resultMessage.getPayload()));

        // test that only active modifications have been made on variant VARIANT_ID
        network.getVariantManager().setWorkingVariant(NetworkCreation.VARIANT_ID);
        assertTrue(network.getSwitch("v1d1").isOpen());
        assertNull(network.getLine("line1").getExtension(BranchStatus.class));
        assertEquals(55., network.getGenerator("idGenerator").getTargetP(), 0.1);
        assertEquals(2, network.getTwoWindingsTransformer("trf1").getRatioTapChanger().getTapPosition());
        assertEquals(0, network.getThreeWindingsTransformer("trf6").getLeg1().getPhaseTapChanger().getTapPosition());
        assertNull(network.getLoad("newLoad"));
        assertEquals(EnergySource.HYDRO, network.getGenerator("newGenerator").getEnergySource());
        assertEquals("v2", network.getGenerator("newGenerator").getTerminal().getVoltageLevel().getId());
        assertEquals(500., network.getGenerator("newGenerator").getMaxP(), 0.1);
        assertEquals(100., network.getGenerator("newGenerator").getTargetP(), 0.1);
        assertTrue(network.getGenerator("newGenerator").isVoltageRegulatorOn());
        assertEquals(225., network.getGenerator("newGenerator").getTargetV(), 0.1);
        assertEquals("v1", network.getLine("newLine").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", network.getLine("newLine").getTerminal2().getVoltageLevel().getId());
        assertEquals(4., network.getLine("newLine").getB1(), 0.1);
        assertEquals("v1", network.getTwoWindingsTransformer("new2wt").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", network.getTwoWindingsTransformer("new2wt").getTerminal2().getVoltageLevel().getId());
        assertEquals(2., network.getTwoWindingsTransformer("new2wt").getX(), 0.1);
        assertEquals(5., network.getTwoWindingsTransformer("new2wt").getRatedU1(), 0.1);
        assertNotNull(network.getShuntCompensator("v2shunt"));
        assertEquals(Country.FR, network.getSubstation("newSubstation").getCountry().orElse(Country.AF));
        assertNotNull(network.getVoltageLevel("vl9"));
        assertNotNull(network.getShuntCompensator("shunt9"));
    }

    @Test
    public void stopBuildTest() throws Exception {
        List<ModificationEntity> entities = List.of(
            modificationRepository.createEquipmentAttributeModification("v1d1", "open", true),
            modificationRepository.createEquipmentAttributeModification("line1", "branchStatus", BranchStatus.Status.PLANNED_OUTAGE)
        );

        modificationRepository.saveModifications(TEST_GROUP_ID, entities);  // save all modification entities in group TEST_GROUP_ID
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        // Build VARIANT_ID by cloning network initial variant and applying all modifications in group uuid TEST_GROUP_ID
        // Because TestChannelBinder implementation is synchronous the build is made in a different thread
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(new GroupAndReportInfos(TEST_GROUP_ID, TEST_REPORT_ID)),
            Set.of());
        String buildInfosJson = mapper.writeValueAsString(buildInfos);
        CompletableFuture.runAsync(() -> {
            try {
                mockMvc.perform(post("/v1/networks/{networkUuid}/build?receiver=me", TEST_NETWORK_STOP_BUILD_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(buildInfosJson))
                    .andExpect(status().isOk());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }, executorService);

        // stop build
        waitStartBuild.await();
        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        mockMvc.perform(put("/v1/build/stop?receiver=me")).andExpect(status().isOk());
        assertNotNull(output.receive(TIMEOUT, cancelBuildDestination));

        Message<byte[]> message = output.receive(TIMEOUT, buildStoppedDestination);
        assertNotNull(message);
        assertEquals("me", message.getHeaders().get("receiver"));
        assertEquals(BuildStoppedPublisherService.CANCEL_MESSAGE, message.getHeaders().get("message"));
    }

    @Test
    public void runBuildWithReportErrorTest() throws Exception {
        // mock exception when sending to report server
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), any(HttpEntity.class), eq(ReporterModel.class)))
            .willThrow(RestClientException.class);

        modificationRepository.saveModifications(TEST_GROUP_ID, List.of(modificationRepository.createEquipmentAttributeModification("v1d1", "open", true)));

        // build VARIANT_ID by cloning network initial variant and applying all modifications in all groups
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(new GroupAndReportInfos(TEST_GROUP_ID, TEST_REPORT_ID)),
            new HashSet<>());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .content(mapper.writeValueAsString(buildInfos)))
            .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        assertNull(output.receive(TIMEOUT, buildResultDestination));
        Message<byte[]> message = output.receive(TIMEOUT * 3, buildFailedDestination);
        assertEquals("me", message.getHeaders().get("receiver"));
        assertThat((String) message.getHeaders().get("message"), CoreMatchers.startsWith(BuildFailedPublisherService.FAIL_MESSAGE));
    }

    @Test
    public void doActionWithUncheckedExceptionTest() {
        Network networkTest = NetworkCreation.create(TEST_NETWORK_ID, true);
        NetworkStoreListener listener = NetworkStoreListener.create(networkTest, TEST_NETWORK_ID, null, modificationRepository, equipmentInfosService, true, true);
        ReporterModel reporter = new ReporterModel("reportKey", "reportName");
        Reporter subReporter = reporter.createSubReporter("AttributeModification", "Attribute modification");
        assertThrows("unexpected error", RuntimeException.class, () ->
            networkModificationService.doAction(listener, () -> {
                throw new RuntimeException("unexpected error");
            }, MODIFICATION_ERROR, TEST_NETWORK_ID, reporter, subReporter)
        );
    }

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) throws Exception {
        // get all modifications for the given group of a network
        MvcResult mvcResult = mockMvc.perform(get("/v1/groups/{groupUuid}/modifications", groupUuid)).andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        List<ModificationInfos> modificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertEquals(actualSize, modificationInfos.size());
    }

    @After
    public void tearDown() {
        List<String> destinations = List.of(consumeBuildDestination, cancelBuildDestination, buildResultDestination, buildStoppedDestination, buildFailedDestination);
        try {
            destinations.forEach(destination -> assertNull("Should not be any messages in queue " + destination + " : ", output.receive(TIMEOUT, destination)));
        } finally {
            output.clear(); // purge in order to not fail the other tests
        }
    }
}
