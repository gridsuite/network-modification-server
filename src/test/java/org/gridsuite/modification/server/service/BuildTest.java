/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import mockwebserver3.Dispatcher;
import mockwebserver3.MockResponse;
import mockwebserver3.MockWebServer;
import mockwebserver3.RecordedRequest;
import mockwebserver3.junit5.internal.MockWebServerExtension;
import okhttp3.HttpUrl;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.ContextConfigurationWithTestChannel;
import org.gridsuite.modification.TapChangerType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosRepository;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.TombstonedEquipmentInfosRepository;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.equipment.creation.TapChangerStepCreationEmbeddable;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationGroupRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.assertions.Assertions;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.stubbing.Answer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cloud.stream.binder.test.OutputDestination;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.messaging.Message;
import org.springframework.test.web.servlet.MockMvc;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;

import static com.powsybl.iidm.network.ReactiveLimitsKind.MIN_MAX;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.*;
import static org.gridsuite.modification.server.service.BuildWorkerService.CANCEL_MESSAGE;
import static org.gridsuite.modification.server.service.BuildWorkerService.FAIL_MESSAGE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@ExtendWith(MockWebServerExtension.class)
@AutoConfigureMockMvc
@SpringBootTest
@ContextConfigurationWithTestChannel
@Tag("IntegrationTest")
class BuildTest {
    private static final Logger LOGGER = LoggerFactory.getLogger(BuildTest.class);

    @Autowired
    private MockMvc mockMvc;
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_STOP_BUILD_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID_2 = UUID.randomUUID();

    private static final UUID TEST_ERROR_REPORT_ID = UUID.randomUUID();
    private static final UUID TEST_SUB_REPORTER_ID_1 = UUID.randomUUID();
    private static final UUID TEST_SUB_REPORTER_ID_2 = UUID.randomUUID();

    private static final int TIMEOUT = 1000;

    private static final String VARIANT_ID_2 = "variant_2";
    private static final String NEW_GENERATOR_ID = "newGenerator";

    private final ExecutorService executorService = Executors.newCachedThreadPool();

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
    private ModificationGroupRepository modificationGroupRepository;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Autowired
    private NetworkModificationService networkModificationService;

    @Autowired
    private NetworkModificationApplicator networkModificationApplicator;

    @Autowired
    private ReportService reportService;

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @Autowired
    private TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    @Autowired
    private EquipmentInfosRepository equipmentInfosRepository;

    @Autowired
    private ObjectMapper mapper;
    private ObjectWriter objectWriter;

    private Network network;

    @BeforeEach
    public void setUp(final MockWebServer mockWebServer) {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        // create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), any(PreloadingStrategy.class))).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });

        waitStartBuild = new CountDownLatch(1);
        blockBuild = new CountDownLatch(1);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_STOP_BUILD_ID), any(PreloadingStrategy.class))).then((Answer<Network>) invocation -> {
            // Needed so the stop call doesn't arrive too late
            waitStartBuild.countDown();
            blockBuild.await();

            network = NetworkCreation.create(TEST_NETWORK_STOP_BUILD_ID, true);
            return network;
        });

        initMockWebServer(mockWebServer);
    }

    @AfterEach
    void cleanDB() {
        modificationRepository.deleteAll();
        equipmentInfosService.deleteVariants(TEST_NETWORK_ID, List.of(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID, VARIANT_ID_2));
    }

    private void initMockWebServer(final MockWebServer server) {
        // Ask the server for its URL. You'll need this to make HTTP requests.
        HttpUrl baseHttpUrl = server.url("");
        String baseUrl = baseHttpUrl.toString().substring(0, baseHttpUrl.toString().length() - 1);
        reportService.setReportServerBaseUri(baseUrl);

        final Dispatcher dispatcher = new Dispatcher() {
            @Override
            @NotNull
            public MockResponse dispatch(RecordedRequest request) {
                String path = Objects.requireNonNull(request.getPath());
                if (path.matches("/v1/reports/.*") && Objects.equals(request.getMethod(), HttpMethod.PUT.name())) {
                    String reportUuid = Objects.requireNonNull(request.getRequestUrl()).pathSegments().get(2);
                    if (TEST_ERROR_REPORT_ID.toString().equals(reportUuid)) {
                        return new MockResponse(HttpStatus.INTERNAL_SERVER_ERROR.value());
                    }
                    return new MockResponse(HttpStatus.OK.value());
                } else {
                    LOGGER.error("Unhandled method+path: " + request.getMethod() + " " + request.getPath());
                    return new MockResponse.Builder().code(HttpStatus.I_AM_A_TEAPOT.value()).body("Unhandled method+path: " + request.getMethod() + " " + request.getPath()).build();
                }
            }
        };
        server.setDispatcher(dispatcher);
    }

    @Test
    void runBuildForLineSplits(final MockWebServer server) throws Exception {
        List<ModificationEntity> entities1 = List.of(
                ModificationEntity.fromDTO(LineCreationInfos.builder()
                        .equipmentId("newLine")
                        .equipmentName("newLine")
                        .r(1.0)
                        .x(2.0)
                        .g1(3.0)
                        .b1(4.0)
                        .g2(5.0)
                        .b2(6.0)
                        .voltageLevelId1("v1")
                        .busOrBusbarSectionId1("1.1")
                        .voltageLevelId2("v2")
                        .busOrBusbarSectionId2("1B")
                        .connectionName1("cn11")
                        .connectionDirection1(ConnectablePosition.Direction.TOP)
                        .connectionName2("cn22")
                        .connectionDirection2(ConnectablePosition.Direction.TOP)
                        .build()),
                ModificationEntity.fromDTO(LineSplitWithVoltageLevelInfos.builder()
                        .lineToSplitId("line3")
                        .percent(0.32)
                        .mayNewVoltageLevelInfos(null)
                        .existingVoltageLevelId("vl1")
                        .bbsOrBusId("sjb1")
                        .newLine1Id("un")
                        .newLine1Name("One")
                        .newLine2Id("deux")
                        .newLine2Name("Two")
                        .build())
        );
        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);

        List<ModificationEntity> entities2 = new ArrayList<>();
        entities2.add(ModificationEntity.fromDTO(VoltageLevelCreationInfos.builder()
                .equipmentId("vl9")
                .equipmentName("vl9")
                .nominalV(225)
                .substationId("s1")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("vl9_1_1").busbarSectionId2("vl9_2_1").build()))
                .build()));
        modificationRepository.saveModifications(TEST_GROUP_ID_2, entities2);

        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID, TEST_GROUP_ID_2),
            List.of(new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_1), new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_2)));
        mockMvc.perform(post(uriString, TEST_NETWORK_ID)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(mapper.writeValueAsString(buildInfos)))
                .andExpect(status().isOk());

        Message<byte[]> resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        Message<byte[]> buildMessage = output.receive(TIMEOUT, consumeBuildDestination);
        assertNotNull(buildMessage);
        assertEquals("me", buildMessage.getHeaders().get("receiver"));

        BuildInfos newBuildInfos = new BuildInfos(NetworkCreation.VARIANT_ID,
            VARIANT_ID_2,
            List.of(),
            List.of());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID)
                                .contentType(MediaType.APPLICATION_JSON)
                                .content(mapper.writeValueAsString(newBuildInfos)))
                .andExpect(status().isOk());

        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        buildMessage = output.receive(TIMEOUT, consumeBuildDestination);
        assertNotNull(buildMessage);
        assertEquals("me", buildMessage.getHeaders().get("receiver"));

        testEmptyImpacts(mapper, new String(resultMessage.getPayload()));

        TestUtils.purgeRequests(server);
    }

    @Test
    void runBuildWithEmptyGroupTest(final MockWebServer server) throws Exception {
        Network network = NetworkCreation.create(TEST_NETWORK_ID, false);
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID),
            List.of(new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_1)));
        String expectedBody = mapper.writeValueAsString(ReportNode.newRootReportNode()
                .withMessageTemplate(TEST_SUB_REPORTER_ID_1.toString(), TEST_SUB_REPORTER_ID_1.toString())
                .build());

        // Group does not exist
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(mapper.writeValueAsString(buildInfos)))
            .andExpect(status().isOk());
        RecordedRequest request = server.takeRequest(TIMEOUT, TimeUnit.MILLISECONDS);
        assertNotNull(request);
        assertEquals(expectedBody, request.getBody().readUtf8());

        assertNotNull(output.receive(TIMEOUT, buildResultDestination));
        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));

        // Group is empty
        modificationGroupRepository.save(new ModificationGroupEntity(TEST_GROUP_ID));
        networkModificationService.buildVariant(TEST_NETWORK_ID, buildInfos);
        request = server.takeRequest(TIMEOUT, TimeUnit.MILLISECONDS);
        assertNotNull(request);
        assertEquals(expectedBody, request.getBody().readUtf8());
    }

    @Test
    void testIndexationAfterBuild(final MockWebServer server) {
        List<ModificationEntity> equipmentsToAdd = new ArrayList<>();
        // add new voltage level
        equipmentsToAdd.add(ModificationEntity.fromDTO(VoltageLevelCreationInfos.builder()
                .equipmentId("vl1")
                .equipmentName("vl1")
                .nominalV(225)
                .substationId("s1")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("vl9_1_1").busbarSectionId2("vl9_2_1").build()))
                .build()));
        // add new Load
        equipmentsToAdd.add(ModificationEntity.fromDTO(LoadCreationInfos.builder()
                .equipmentId("newLoad")
                .equipmentName("newLoad")
                .loadType(LoadType.AUXILIARY)
                .voltageLevelId("vl1")
                .busOrBusbarSectionId("1.1")
                .p0(10.)
                .q0(20.)
                .connectionName("vn")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build()));

        //add new Line
        equipmentsToAdd.add(ModificationEntity.fromDTO(LineCreationInfos.builder()
                .equipmentId("newLine")
                .equipmentName("newLine")
                .r(1.0)
                .x(2.0)
                .g1(3.0)
                .b1(4.0)
                .g2(5.0)
                .b2(6.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1B")
                .connectionName1("cn11")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn22")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build()));

        //add new Line with same voltage level
        equipmentsToAdd.add(ModificationEntity.fromDTO(LineCreationInfos.builder()
                .equipmentId("newLine2")
                .equipmentName("newLine2")
                .r(1.0)
                .x(2.0)
                .g1(3.0)
                .b1(4.0)
                .g2(5.0)
                .b2(6.0)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v1")
                .busOrBusbarSectionId2("1.1")
                .connectionName1("cn11")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn11")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .build()));

        // save modifications
        modificationRepository.saveModifications(TEST_GROUP_ID, equipmentsToAdd);

        // Create build infos
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID),
            List.of(new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_1)));

        // Build variant
        networkModificationService.buildVariant(TEST_NETWORK_ID, buildInfos);

        // Check if added equipments are indexed in elasticsearch
        //check voltage level indexation
        var expectedEquipmentInfos = EquipmentInfos.builder()
                .networkUuid(TEST_NETWORK_ID)
                .variantId(NetworkCreation.VARIANT_ID)
                .id("vl1")
                .name("vl1")
                .type(IdentifiableType.VOLTAGE_LEVEL.name())
                .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build()))
                .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
                .build();
        var equipmentInfos = equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("vl1"), TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).get(0);
        Assertions.assertThat(equipmentInfos)
                .usingRecursiveComparison()
                .ignoringFields("uniqueId")
                .isEqualTo(expectedEquipmentInfos);

        //check load indexation
        expectedEquipmentInfos = EquipmentInfos.builder()
                .networkUuid(TEST_NETWORK_ID)
                .variantId(NetworkCreation.VARIANT_ID)
                .id("newLoad")
                .name("newLoad")
                .type(IdentifiableType.LOAD.name())
                .voltageLevels(Set.of(VoltageLevelInfos.builder().id("vl1").name("vl1").build()))
                .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
                .build();
        equipmentInfos = equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("newLoad"), TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).get(0);
        Assertions.assertThat(equipmentInfos)
                .usingRecursiveComparison()
                .ignoringFields("uniqueId")
                .isEqualTo(expectedEquipmentInfos);

        //check line indexation
        expectedEquipmentInfos = EquipmentInfos.builder()
                .networkUuid(TEST_NETWORK_ID)
                .variantId(NetworkCreation.VARIANT_ID)
                .id("newLine")
                .name("newLine")
                .type(IdentifiableType.LINE.name())
                .voltageLevels(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build(), VoltageLevelInfos.builder().id("v2").name("v2").build()))
                .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
                .build();
        equipmentInfos = equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("newLine"), TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).get(0);
        Assertions.assertThat(equipmentInfos)
                .usingRecursiveComparison()
                .ignoringFields("uniqueId")
                .isEqualTo(expectedEquipmentInfos);

        //check line2 indexation
        expectedEquipmentInfos = EquipmentInfos.builder()
                .networkUuid(TEST_NETWORK_ID)
                .variantId(NetworkCreation.VARIANT_ID)
                .id("newLine2")
                .name("newLine2")
                .type(IdentifiableType.LINE.name())
                .voltageLevels(Set.of(VoltageLevelInfos.builder().id("v1").name("v1").build()))
                .substations(Set.of(SubstationInfos.builder().id("s1").name("s1").build()))
                .build();
        equipmentInfos = equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(List.of("newLine2"), TEST_NETWORK_ID, NetworkCreation.VARIANT_ID).get(0);
        Assertions.assertThat(equipmentInfos)
                .usingRecursiveComparison()
                .ignoringFields("uniqueId")
                .isEqualTo(expectedEquipmentInfos);
        assertTrue(TestUtils.getRequestsDone(1, server).stream().anyMatch(r -> r.matches("/v1/reports/.*")));
    }

    @Test
    void runBuildTest(final MockWebServer server) throws Exception {
        // create modification entities in the database
        List<ModificationEntity> entities1 = new ArrayList<>();
        entities1.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("v1d1").equipmentAttributeName("open").equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build()));
        entities1.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("line1").equipmentAttributeName("operatingStatus").equipmentAttributeValue(OperatingStatus.Status.PLANNED_OUTAGE).equipmentType(IdentifiableType.LINE).build()));
        entities1.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("idGenerator").equipmentAttributeName("targetP").equipmentAttributeValue(50.).equipmentType(IdentifiableType.GENERATOR).build()));
        entities1.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("trf1").equipmentAttributeName("ratioTapChanger.tapPosition").equipmentAttributeValue(2).equipmentType(IdentifiableType.TWO_WINDINGS_TRANSFORMER).build()));
        entities1.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("trf6").equipmentAttributeName("phaseTapChanger1.tapPosition").equipmentAttributeValue(0).equipmentType(IdentifiableType.THREE_WINDINGS_TRANSFORMER).build()));

        entities1.add(ModificationEntity.fromDTO(LoadCreationInfos.builder().equipmentId("newLoad").equipmentName("newLoad").loadType(LoadType.AUXILIARY).voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).connectionName("vn").connectionDirection(ConnectablePosition.Direction.TOP).terminalConnected(true).build()));
        entities1.add(ModificationEntity.fromDTO(LoadCreationInfos.builder().equipmentId("newLoad1").equipmentName("newLoad1").loadType(LoadType.AUXILIARY).voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).connectionName("cn1").connectionDirection(ConnectablePosition.Direction.BOTTOM).terminalConnected(true).build()));
        entities1.add(ModificationEntity.fromDTO(LoadCreationInfos.builder().equipmentId("newLoad2").equipmentName("newLoad2").loadType(LoadType.AUXILIARY).voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).connectionName("cn2").connectionDirection(ConnectablePosition.Direction.UNDEFINED).terminalConnected(true).build()));
        entities1.add(ModificationEntity.fromDTO(LoadCreationInfos.builder().equipmentId("newLoad3").equipmentName("newLoad3").loadType(LoadType.AUXILIARY).voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).connectionName(null).connectionDirection(ConnectablePosition.Direction.UNDEFINED).terminalConnected(true).build()));

        List<FreePropertyInfos> properties = List.of(FreePropertyInfos.builder().name("DEMO").value("Demo1").build());
        entities1.add(ModificationEntity.fromDTO(SubstationCreationInfos.builder()
                .equipmentId("newSubstation")
                .equipmentName("newSubstation")
                .country(Country.FR)
                .properties(properties)
                .build()));

        List<ModificationEntity> entities2 = new ArrayList<>();
        entities2.add(ModificationEntity.fromDTO(GeneratorCreationInfos.builder()
                .equipmentId(NEW_GENERATOR_ID).equipmentName(NEW_GENERATOR_ID)
                .energySource(EnergySource.HYDRO).voltageLevelId("v2")
                .busOrBusbarSectionId("1A").minP(0)
                .maxP(500).ratedS(1.)
                .targetP(100).targetQ(50.)
                .voltageRegulationOn(true).targetV(225.)
                .plannedActivePowerSetPoint(80.)
                .marginalCost(82.)
                .plannedOutageRate(83.).forcedOutageRate(84.)
                .minQ(20.).maxQ(50.)
                .participate(true).droop(9F).directTransX(35.)
                .stepUpTransformerX(25.).regulatingTerminalId("v2load")
                .regulatingTerminalType("LOAD").regulatingTerminalVlId("v2")
                .qPercent(25.).reactiveCapabilityCurve(false).reactiveCapabilityCurvePoints(List.of())
                .connectionName("Top").connectionDirection(ConnectablePosition.Direction.TOP)
                .connectionPosition(0)
                .terminalConnected(true)
                .build()));
        entities2.add(ModificationEntity.fromDTO(LineCreationInfos.builder().equipmentId("newLine").equipmentName("newLine").r(1.0).x(2.0).g1(3.0).b1(4.0).g2(5.0).b2(6.0).voltageLevelId1("v1").busOrBusbarSectionId1("1.1").voltageLevelId2("v2").busOrBusbarSectionId2("1B").currentLimits1(null).currentLimits2(null).connectionName1("cn101").connectionDirection1(ConnectablePosition.Direction.TOP).connectionName2("cn102").connectionDirection2(ConnectablePosition.Direction.TOP).connected1(true).connected2(true).build()));

        List<TapChangerStepCreationEmbeddable> tapChangerStepCreationEmbeddables = new ArrayList<>();
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 1, 1, 0, 0, 0, 0, 0.));
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 2, 1, 0, 0, 0, 0, 0.));
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 3, 1, 0, 0, 0, 0, 0.));
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 5, 1, 0, 0, 0, 0, null));
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 6, 1, 0, 0, 0, 0, null));
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 7, 1, 0, 0, 0, 0, null));
        tapChangerStepCreationEmbeddables.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 8, 1, 0, 0, 0, 0, null));

        entities2.add(ModificationEntity.fromDTO(EquipmentDeletionInfos.builder().equipmentId("v2shunt").equipmentType(IdentifiableType.SHUNT_COMPENSATOR).build()));
        entities2.add(ModificationEntity.fromDTO(GroovyScriptInfos.builder().script("network.getGenerator('idGenerator').targetP=55\n").build()));
        entities2.add(ModificationEntity.fromDTO(OperatingStatusModificationInfos.builder().equipmentId("line2").action(OperatingStatusModificationInfos.ActionType.TRIP).build()));
        entities2.add(ModificationEntity.fromDTO(VoltageLevelCreationInfos.builder()
                .equipmentId("vl9")
                .equipmentName("vl9")
                .nominalV(225)
                .substationId("s1")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("vl9_1_1").busbarSectionId2("vl9_2_1").build()))
                .build()));
        entities2.add(ModificationEntity.fromDTO(ShuntCompensatorCreationInfos.builder()
            .equipmentId("shunt9")
            .equipmentName("shunt9")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1A")
            .maximumSectionCount(2)
            .sectionCount(1)
            .maxSusceptance(1.)
            .connectionDirection(ConnectablePosition.Direction.UNDEFINED)
            .connectionName("shunt9")
            .terminalConnected(true)
            .build()));
        entities2.add(ModificationEntity.fromDTO(TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("new2wt")
                .equipmentName("new2wt")
                .r(1.)
                .x(2.)
                .g(3.)
                .b(4.)
                .ratedU1(5.)
                .ratedU2(6.)
                .ratedS(1.)
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .connected1(true)
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .connected2(true)
                .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).build())
                .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).build())
                .connectionName1("cn201")
                .connectionDirection1(ConnectablePosition.Direction.TOP)
                .connectionName2("cn202")
                .connectionDirection2(ConnectablePosition.Direction.TOP)
                .phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                        .lowTapPosition(1)
                        .tapPosition(2)
                        .regulatingTerminalId("v1load")
                        .regulatingTerminalVlId("v1")
                        .regulating(false)
                        .regulatingTerminalType("LOAD")
                        .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                        .index(1)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(2)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0.)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(3)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .alpha(0.)
                                        .build()
                        )).build())
                .ratioTapChanger(RatioTapChangerCreationInfos.builder()
                        .lowTapPosition(5)
                        .tapPosition(6)
                        .regulating(true)
                        .targetDeadband(1.)
                        .regulatingTerminalId("v2load")
                        .regulatingTerminalVlId("v2")
                        .regulatingTerminalType("LOAD")
                        .loadTapChangingCapabilities(true)
                        .targetV(5.)
                        .steps(List.of(TapChangerStepCreationInfos.builder()
                                        .index(5)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(6)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(7)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build(),
                                TapChangerStepCreationInfos.builder()
                                        .index(8)
                                        .rho(1)
                                        .r(0)
                                        .x(0)
                                        .g(0)
                                        .b(0)
                                        .build()
                        ))
                        .build())
                .build())
        );
        entities2.add(ModificationEntity.fromDTO(LoadModificationInfos.builder().equipmentId("newLoad")
            .equipmentName(new AttributeModification<>("newLoadName", OperationType.SET)).p0(null).build()));
        entities2.add(ModificationEntity.fromDTO(GeneratorModificationInfos.builder()
                .equipmentId("newGenerator")
                .equipmentName(new AttributeModification<>("newGeneratorName", OperationType.SET))
                .voltageRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET)).build()));

        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);
        modificationRepository.saveModifications(TEST_GROUP_ID_2, entities2);

        testNetworkModificationsCount(TEST_GROUP_ID, entities1.size());
        testNetworkModificationsCount(TEST_GROUP_ID_2, entities2.size());

        // build VARIANT_ID by cloning network initial variant and applying all modifications in all groups
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID, TEST_GROUP_ID_2),
            List.of(new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_1), new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_2)));
        String buildInfosJson = objectWriter.writeValueAsString(buildInfos);
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(buildInfosJson))
            .andExpect(status().isOk());

        Message<byte[]> resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        // 2 : LOAD and SWITCH equipments are reduced to collection impact
        // + 2 substation modifications
        // (newSubstation is created but transformed to modification type (see NetworkStoreListener::reduceNetworkImpacts))
        // + 3 Equipment deletions ( 1 shunt compensator + 2 switch)
        // = 7
        testElementImpacts(mapper, new String(resultMessage.getPayload()), 7, Set.of(IdentifiableType.LOAD, IdentifiableType.SWITCH), Set.of("newSubstation", "s1"));
        Message<byte[]> buildMessage = output.receive(TIMEOUT, consumeBuildDestination);
        assertNotNull(buildMessage);
        assertEquals("me", buildMessage.getHeaders().get("receiver"));

        // test all modifications have been made on variant VARIANT_ID
        network.getVariantManager().setWorkingVariant(NetworkCreation.VARIANT_ID);
        assertTrue(network.getSwitch("v1d1").isOpen());
        OperatingStatus<Line> operatingStatus = network.getLine("line1").getExtension(OperatingStatus.class);
        assertNotNull(operatingStatus);
        assertEquals(OperatingStatus.Status.PLANNED_OUTAGE, operatingStatus.getStatus());
        operatingStatus = network.getLine("line2").getExtension(OperatingStatus.class);
        assertNotNull(operatingStatus);
        assertEquals(OperatingStatus.Status.FORCED_OUTAGE, operatingStatus.getStatus());

        assertEquals(55., network.getGenerator("idGenerator").getTargetP(), 0.1);
        assertEquals(2, network.getTwoWindingsTransformer("trf1").getRatioTapChanger().getTapPosition());
        assertEquals(0, network.getThreeWindingsTransformer("trf6").getLeg1().getPhaseTapChanger().getTapPosition());
        assertEquals(LoadType.AUXILIARY, network.getLoad("newLoad").getLoadType());
        assertEquals(10., network.getLoad("newLoad").getP0(), 0.1);
        assertEquals(20., network.getLoad("newLoad").getQ0(), 0.1);
        assertEquals(10., network.getLoad("newLoad1").getP0(), 0.1);
        assertEquals(20., network.getLoad("newLoad1").getQ0(), 0.1);
        assertEquals(10., network.getLoad("newLoad2").getP0(), 0.1);
        assertEquals(20., network.getLoad("newLoad2").getQ0(), 0.1);
        assertEquals(EnergySource.HYDRO, network.getGenerator(NEW_GENERATOR_ID).getEnergySource());
        assertEquals("v2", network.getGenerator(NEW_GENERATOR_ID).getTerminal().getVoltageLevel().getId());
        assertEquals(500., network.getGenerator(NEW_GENERATOR_ID).getMaxP(), 0.1);
        assertEquals(100., network.getGenerator(NEW_GENERATOR_ID).getTargetP(), 0.1);
        assertEquals(80., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getPlannedActivePowerSetpoint(), 0);
        assertEquals(82., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getMarginalCost(), 0);
        assertEquals(83., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getPlannedOutageRate(), 0);
        assertEquals(84., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getForcedOutageRate(), 0);
        assertTrue(network.getGenerator(NEW_GENERATOR_ID).getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(9F, network.getGenerator(NEW_GENERATOR_ID).getExtension(ActivePowerControl.class).getDroop(), 0);
        assertEquals(35., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorShortCircuit.class).getDirectTransX(), 0);
        assertEquals(25., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorShortCircuit.class).getStepUpTransformerX(), 0);
        assertEquals(MIN_MAX, network.getGenerator(NEW_GENERATOR_ID).getReactiveLimits().getKind());
        assertTrue(network.getGenerator(NEW_GENERATOR_ID).isVoltageRegulatorOn());
        assertEquals(225., network.getGenerator(NEW_GENERATOR_ID).getTargetV(), 0.1);
        assertEquals("v1", network.getLine("newLine").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", network.getLine("newLine").getTerminal2().getVoltageLevel().getId());
        assertEquals(4., network.getLine("newLine").getB1(), 0.1);
        assertEquals("v1", network.getTwoWindingsTransformer("new2wt").getTerminal1().getVoltageLevel().getId());
        assertEquals("v2", network.getTwoWindingsTransformer("new2wt").getTerminal2().getVoltageLevel().getId());
        assertEquals(2., network.getTwoWindingsTransformer("new2wt").getX(), 0.1);
        assertEquals(5., network.getTwoWindingsTransformer("new2wt").getRatedU1(), 0.1);
        assertEquals(1, network.getTwoWindingsTransformer("new2wt").getRatedS(), 0.1);
        assertEquals(4, network.getTwoWindingsTransformer("new2wt").getRatioTapChanger().getStepCount());
        assertEquals(3, network.getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getStepCount());
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER, network.getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getRegulationMode());
        assertNull(network.getShuntCompensator("v2shunt"));
        Substation newSubstation = network.getSubstation("newSubstation");
        assertEquals(Country.FR, newSubstation.getCountry().orElse(Country.AF));
        assertEquals(Set.of("DEMO"), network.getSubstation("newSubstation").getPropertyNames());
        assertEquals("Demo1", network.getSubstation("newSubstation").getProperty("DEMO"));
        assertNotNull(network.getVoltageLevel("vl9"));
        assertNotNull(network.getShuntCompensator("shunt9"));

        // Test that no modifications have been made on initial variant
        network.getVariantManager().setWorkingVariant(VariantManagerConstants.INITIAL_VARIANT_ID);
        assertFalse(network.getSwitch("v1d1").isOpen());
        assertNull(network.getLine("line1").getExtension(OperatingStatus.class));
        assertNull(network.getLine("line2").getExtension(OperatingStatus.class));
        assertEquals(42.1, network.getGenerator("idGenerator").getTargetP(), 0.1);
        assertEquals(1, network.getTwoWindingsTransformer("trf1").getRatioTapChanger().getTapPosition());
        assertEquals(1, network.getThreeWindingsTransformer("trf6").getLeg1().getPhaseTapChanger().getTapPosition());
        assertNull(network.getLoad("newLoad"));
        assertNull(network.getGenerator(NEW_GENERATOR_ID));
        assertNull(network.getGenerator("newLine"));
        assertNull(network.getGenerator("new2wt"));
        assertNotNull(network.getShuntCompensator("v2shunt"));
        assertNull(network.getSubstation("newSubstation"));
        assertNull(network.getVoltageLevel("vl9"));
        assertNull(network.getShuntCompensator("shunt9"));

        // No new modification entity should have been added to the database
        testNetworkModificationsCount(TEST_GROUP_ID, entities1.size());
        testNetworkModificationsCount(TEST_GROUP_ID_2, entities2.size());

        // Execute another build starting from variant VARIANT_ID to variant VARIANT_ID_2
        // to check
        BuildInfos newBuildInfos = new BuildInfos(NetworkCreation.VARIANT_ID,
            VARIANT_ID_2,
            Collections.emptyList(),
            Collections.emptyList());
        buildInfosJson = objectWriter.writeValueAsString(newBuildInfos);
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(buildInfosJson)).andExpect(status().isOk());

        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        testEmptyImpacts(mapper, new String(resultMessage.getPayload()));
        buildMessage = output.receive(TIMEOUT, consumeBuildDestination);
        assertNotNull(buildMessage);
        assertEquals("me", buildMessage.getHeaders().get("receiver"));

        List<EquipmentInfos> eqVariant1 = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        List<EquipmentInfos> eqVariant2 = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VARIANT_ID_2);
        assertTrue(eqVariant2.size() > 0);
        assertEquals(eqVariant1.size(), eqVariant2.size());

        List<TombstonedEquipmentInfos> tbseqVariant1 = tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        List<TombstonedEquipmentInfos> tbseqVariant2 = tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VARIANT_ID_2);
        // v2shunt was deleted from initial variant => v2shunt and the cell switches (breaker and disconnector) have been added as TombstonedEquipmentInfos in ElasticSearch
        // switch equipments are not indexed in elasticsearch
        assertEquals(1, tbseqVariant1.size());
        assertEquals(tbseqVariant1.size(), tbseqVariant2.size());

        TestUtils.purgeRequests(server);
    }

    @Test
    void runBuildWithStashedModificationsTest(final MockWebServer server) {
        // create modification entities in the database
        List<ModificationEntity> entities1 = new ArrayList<>();
        entities1.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("v1d1").equipmentAttributeName("open").equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build()));
        entities1.add(ModificationEntity.fromDTO(LoadCreationInfos.builder().equipmentId("willBeStashedLoad").equipmentName("willBeStashedLoad").loadType(LoadType.AUXILIARY).voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).connectionName("vn").connectionDirection(ConnectablePosition.Direction.TOP).terminalConnected(true).stashed(true).build()));

        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);

        testNetworkModificationsCount(TEST_GROUP_ID, entities1.size());

        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID),
            List.of(new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_1)));
        networkModificationService.buildVariant(TEST_NETWORK_ID, buildInfos);

        // test that only non stashed modifications have been made on variant VARIANT_ID
        network.getVariantManager().setWorkingVariant(NetworkCreation.VARIANT_ID);
        assertTrue(network.getSwitch("v1d1").isOpen());
        assertNull(network.getLoad("willBeStashedLoad"));
        assertTrue(TestUtils.getRequestsDone(1, server).stream().anyMatch(r -> r.matches("/v1/reports/.*")));
    }

    @Test
    void stopBuildTest() throws Exception {
        List<ModificationEntity> entities = List.of(
            ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("v1d1").equipmentAttributeName("open").equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build()),
            ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("line1").equipmentAttributeName("operatingStatus").equipmentAttributeValue(OperatingStatus.Status.PLANNED_OUTAGE).equipmentType(IdentifiableType.LINE).build())
        );

        modificationRepository.saveModifications(TEST_GROUP_ID, entities);  // save all modification entities in group TEST_GROUP_ID
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        // Build VARIANT_ID by cloning network initial variant and applying all modifications in group uuid TEST_GROUP_ID
        // Because TestChannelBinder implementation is synchronous the build is made in a different thread
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID),
            List.of(new ReportInfos(UUID.randomUUID(), TEST_SUB_REPORTER_ID_1)));
        String buildInfosJson = mapper.writeValueAsString(buildInfos);
        CompletableFuture.runAsync(() -> {
            try {
                mockMvc.perform(post("/v1/networks/{networkUuid}/build?receiver=me", TEST_NETWORK_STOP_BUILD_ID)
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(buildInfosJson))
                    .andExpect(status().isOk());
            } catch (Exception e) {
                LOGGER.error("mock mvc perform error", e);
            }
        }, executorService);

        // stop build
        waitStartBuild.await();
        mockMvc.perform(put("/v1/build/stop?receiver=me")).andExpect(status().isOk());

        Message<byte[]> message = output.receive(TIMEOUT, buildStoppedDestination);
        assertNotNull(message);
        assertEquals("me", message.getHeaders().get("receiver"));
        assertEquals(CANCEL_MESSAGE, message.getHeaders().get("message"));
        Message<byte[]> buildMessage = output.receive(TIMEOUT, consumeBuildDestination);
        assertNotNull(buildMessage);
        assertEquals("me", buildMessage.getHeaders().get("receiver"));
        Message<byte[]> cancelMessage = output.receive(TIMEOUT, cancelBuildDestination);
        assertNotNull(cancelMessage);
        assertEquals("me", cancelMessage.getHeaders().get("receiver"));
    }

    @Test
    void runBuildWithReportErrorTest(final MockWebServer server) throws Exception {
        List<ModificationEntity> entities = new ArrayList<>();
        entities.add(ModificationEntity.fromDTO(EquipmentAttributeModificationInfos.builder().equipmentId("v1d1").equipmentAttributeName("open").equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build()));
        modificationRepository.saveModifications(TEST_GROUP_ID, entities);

        // build VARIANT_ID by cloning network initial variant and applying all modifications in all groups
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID),
            List.of(new ReportInfos(TEST_ERROR_REPORT_ID, TEST_SUB_REPORTER_ID_1)));
        mockMvc.perform(post(uriString, TEST_NETWORK_ID)
            .contentType(MediaType.APPLICATION_JSON)
            .content(mapper.writeValueAsString(buildInfos)))
            .andExpect(status().isOk());

        assertTrue(TestUtils.getRequestsDone(1, server).stream().anyMatch(r -> r.matches("/v1/reports/.*")));

        assertNull(output.receive(TIMEOUT, buildResultDestination));
        Message<byte[]> message = output.receive(TIMEOUT * 3, buildFailedDestination);
        assertEquals("me", message.getHeaders().get("receiver"));
        assertThat((String) message.getHeaders().get("message"), startsWith(FAIL_MESSAGE));
        Message<byte[]> buildMessage = output.receive(TIMEOUT, consumeBuildDestination);
        assertNotNull(buildMessage);
        assertEquals("me", buildMessage.getHeaders().get("receiver"));
    }

    @Test
    void testApplyModificationWithErrors(final MockWebServer server) {
        Network network = NetworkCreation.create(TEST_NETWORK_ID, true);
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().voltageLevelId("unknownVoltageLevelId").equipmentId("loadId").build();
        UUID groupUuid = UUID.randomUUID();
        UUID reportUuid = UUID.randomUUID();
        UUID reporterId = UUID.randomUUID();
        String variantId = network.getVariantManager().getWorkingVariantId();

        // Building mode : No error send with exception
        NetworkModificationResult networkModificationResult = networkModificationApplicator.applyModifications(List.of(loadCreationInfos), new NetworkInfos(network, TEST_NETWORK_ID, true), new ReportInfos(reportUuid, reporterId));
        assertNotNull(networkModificationResult);
        testEmptyImpactsWithErrors(networkModificationResult);
        assertTrue(TestUtils.getRequestsDone(1, server).stream().anyMatch(r -> r.matches(String.format("/v1/reports/%s", reportUuid))));

        // Incremental mode : No error send with exception
        ModificationApplicationContext applicationContext = new ModificationApplicationContext(TEST_NETWORK_ID, variantId, reportUuid, reporterId);
        List<Optional<NetworkModificationResult>> networkModificationResult2 = networkModificationService.createNetworkModification(groupUuid, loadCreationInfos, List.of(applicationContext));
        assertEquals(1, networkModificationResult2.size());
        assertTrue(networkModificationResult2.get(0).isPresent());
        testEmptyImpactsWithErrors(networkModificationResult);
        assertTrue(TestUtils.getRequestsDone(1, server).stream().anyMatch(r -> r.matches(String.format("/v1/reports/%s", reportUuid))));
        testNetworkModificationsCount(groupUuid, 1);

        // Save mode only (variant does not exist) : No log and no error send with exception
        applicationContext = new ModificationApplicationContext(TEST_NETWORK_ID, UUID.randomUUID().toString(), reportUuid, reporterId);
        networkModificationResult2 = networkModificationService.createNetworkModification(groupUuid, loadCreationInfos, List.of(applicationContext));
        assertEquals(1, networkModificationResult2.size());
        assertTrue(networkModificationResult2.get(0).isEmpty());
        testNetworkModificationsCount(groupUuid, 2);
    }

    @Test
    void testLastGroupModificationStatus(final MockWebServer server) {
        Network network = NetworkCreation.create(TEST_NETWORK_ID, true);
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().voltageLevelId("unknownVoltageLevelId").equipmentId("loadId").build();
        UUID reportUuid = UUID.randomUUID();
        UUID nodeUuid1 = UUID.randomUUID();
        UUID nodeUuid2 = UUID.randomUUID();

        List<Pair<ReportInfos, List<ModificationInfos>>> modificationInfosGroups = new ArrayList<>();
        modificationInfosGroups.add(Pair.of(new ReportInfos(reportUuid, nodeUuid1), List.of(loadCreationInfos)));
        modificationInfosGroups.add(Pair.of(new ReportInfos(UUID.randomUUID(), nodeUuid2), List.of()));

        //Global application status should be in error and last application status should be OK
        NetworkModificationResult networkModificationResult = networkModificationApplicator.applyModifications(modificationInfosGroups, new NetworkInfos(network, TEST_NETWORK_ID, true));
        assertNotNull(networkModificationResult);
        testEmptyImpactsWithErrorsLastOK(networkModificationResult);
        assertTrue(TestUtils.getRequestsDone(2, server).stream().anyMatch(r -> r.matches(String.format("/v1/reports/%s", reportUuid))));
    }

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) {
        assertEquals(actualSize, modificationRepository.getModifications(groupUuid, true, true).size());
    }

    @AfterEach
    void tearDown(final MockWebServer server) {
        List<String> destinations = List.of(consumeBuildDestination, cancelBuildDestination, buildResultDestination, buildStoppedDestination, buildFailedDestination);
        TestUtils.assertQueuesEmptyThenClear(destinations, output);
        try {
            TestUtils.assertServerRequestsEmptyThenShutdown(server);
        } catch (UncheckedInterruptedException e) {
            LOGGER.error("Error while attempting to get the request done : ", e);
        } catch (IOException e) {
            // Ignoring
        }
    }
}
