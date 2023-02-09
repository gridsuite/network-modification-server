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
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition.Direction;
import com.powsybl.network.store.client.NetworkStoreService;

import lombok.SneakyThrows;
import okhttp3.HttpUrl;
import okhttp3.mockwebserver.Dispatcher;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.gridsuite.modification.server.NetworkModificationApplication;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.TapChangerType;
import org.gridsuite.modification.server.dto.*;
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
import org.jetbrains.annotations.NotNull;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.stubbing.Answer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cloud.stream.binder.test.OutputDestination;
import org.springframework.cloud.stream.binder.test.TestChannelBinderConfiguration;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.messaging.Message;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.ContextHierarchy;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;

import static com.powsybl.iidm.network.ReactiveLimitsKind.MIN_MAX;
import static org.gridsuite.modification.server.service.BuildWorkerService.CANCEL_MESSAGE;
import static org.gridsuite.modification.server.service.BuildWorkerService.FAIL_MESSAGE;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest(properties = { "test.elasticsearch.enabled=true" })
@ContextHierarchy({ @ContextConfiguration(classes = { NetworkModificationApplication.class,
        TestChannelBinderConfiguration.class }) })
public class BuildTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(BuildTest.class);

    @Autowired
    private MockMvc mockMvc;
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_STOP_BUILD_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID_2 = UUID.randomUUID();
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private static final UUID TEST_ERROR_REPORT_ID = UUID.randomUUID();
    private static final String TEST_SUB_REPORTER_ID_1 = UUID.randomUUID().toString();
    private static final String TEST_SUB_REPORTER_ID_2 = UUID.randomUUID().toString();

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
    ModificationGroupRepository modificationGroupRepository;

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

    private MockWebServer server;

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

        cleanDB();

        initMockWebServer();
    }

    private void cleanDB() {
        modificationRepository.deleteAll();
        equipmentInfosService.deleteVariants(TEST_NETWORK_ID,
                List.of(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID, VARIANT_ID_2));
    }

    @SneakyThrows
    private void initMockWebServer() {
        server = new MockWebServer();
        server.start();

        // Ask the server for its URL. You'll need this to make HTTP requests.
        HttpUrl baseHttpUrl = server.url("");
        String baseUrl = baseHttpUrl.toString().substring(0, baseHttpUrl.toString().length() - 1);
        reportService.setReportServerBaseUri(baseUrl);

        final Dispatcher dispatcher = new Dispatcher() {
            @SneakyThrows
            @Override
            @NotNull
            public MockResponse dispatch(RecordedRequest request) {
                String path = Objects.requireNonNull(request.getPath());
                if (path.matches("/v1/reports/.*") && Objects.equals(request.getMethod(), HttpMethod.PUT.name())) {
                    String reportUuid = Objects.requireNonNull(request.getRequestUrl()).pathSegments().get(2);
                    if (TEST_ERROR_REPORT_ID.toString().equals(reportUuid)) {
                        return new MockResponse().setResponseCode(HttpStatus.INTERNAL_SERVER_ERROR.value());
                    }
                    return new MockResponse().setResponseCode(HttpStatus.OK.value());
                } else {
                    LOGGER.error("Unhandled method+path: " + request.getMethod() + " " + request.getPath());
                    return new MockResponse().setResponseCode(HttpStatus.I_AM_A_TEAPOT.value())
                            .setBody("Unhandled method+path: " + request.getMethod() + " " + request.getPath());
                }
            }
        };

        server.setDispatcher(dispatcher);
    }

    @Test
    public void runBuildForLineSplits() throws Exception {
        List<ModificationEntity> entities1 = List.of(
            LineCreationInfos.builder().id("newLine").name("newLine").r(1.0)
                .x(2.0).shuntConductance1(3.0).shuntSusceptance1(4.0).shuntConductance2(5.0)
                .shuntSusceptance2(6.0).voltageLevelId1("v1").busOrBusbarSectionId1("1.1").voltageLevelId2("v2")
                .busOrBusbarSectionId2("1B")
                .position1(ConnectablePositionInfos.builder()
                    .label("cn11")
                    .direction(Direction.TOP).build())
                .position2(ConnectablePositionInfos.builder()
                    .label("cn22")
                    .direction(Direction.TOP).build())
                .build().toEntity(),
            LineSplitWithVoltageLevelInfos.builder().lineToSplitId("line3").percent(0.32)
                .mayNewVoltageLevelInfos(null).existingVoltageLevelId("vl1").bbsOrBusId("sjb1").newLine1Id("un")
                .newLine1Name("One").newLine2Id("deux").newLine2Name("Two").build().toEntity());
        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);

        List<ModificationEntity> entities2 = new ArrayList<>();
        entities2.add(VoltageLevelCreationInfos.builder().id("vl9").name("vl9").nominalVoltage(225)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("1.1", "1.1", 1, 1),
                        new BusbarSectionCreationInfos("1.2", "1.2", 1, 2)))
                .busbarConnections(List.of(new BusbarConnectionCreationInfos("1.1", "1.2", SwitchKind.BREAKER))).build()
                .toEntity());
        modificationRepository.saveModifications(TEST_GROUP_ID_2, entities2);

        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID,
                TEST_REPORT_ID, List.of(TEST_GROUP_ID, TEST_GROUP_ID_2),
                List.of(TEST_SUB_REPORTER_ID_1, TEST_SUB_REPORTER_ID_2), new HashSet<>());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(buildInfos))).andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        Message<byte[]> resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));

        BuildInfos newBuildInfos = new BuildInfos(NetworkCreation.VARIANT_ID, VARIANT_ID_2, TEST_REPORT_ID, List.of(),
                List.of(), new HashSet<>());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(newBuildInfos))).andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("", new String(resultMessage.getPayload()));

        TestUtils.purgeRequests(server);
    }

    @Test
    public void runBuildWithEmptyGroupTest() throws Exception {
        Network network = NetworkCreation.create(TEST_NETWORK_ID, false);
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID,
                TEST_REPORT_ID, List.of(TEST_GROUP_ID), List.of(TEST_SUB_REPORTER_ID_1), new HashSet<>());
        String expectedBody = mapper
                .writeValueAsString(new ReporterModel(TEST_SUB_REPORTER_ID_1, TEST_SUB_REPORTER_ID_1));

        // Group does not exist
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(buildInfos))).andExpect(status().isOk());
        RecordedRequest request = server.takeRequest(TIMEOUT, TimeUnit.MILLISECONDS);
        assertNotNull(request);
        assertEquals(expectedBody, request.getBody().readUtf8());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        assertNotNull(output.receive(TIMEOUT, buildResultDestination));

        // Group is empty
        modificationGroupRepository.save(new ModificationGroupEntity(TEST_GROUP_ID));
        networkModificationService.buildVariant(new NetworkInfos(network, TEST_NETWORK_ID, true), buildInfos);
        request = server.takeRequest(TIMEOUT, TimeUnit.MILLISECONDS);
        assertNotNull(request);
        assertEquals(expectedBody, request.getBody().readUtf8());
    }

    @Test
    public void runBuildTest() throws Exception {
        // create modification entities in the database
        List<ModificationEntity> entities1 = new ArrayList<>();
        entities1.add(EquipmentAttributeModificationInfos.builder().id("v1d1").equipmentAttributeName("open")
                .equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build().toEntity());
        entities1.add(EquipmentAttributeModificationInfos.builder().id("line1")
                .equipmentAttributeName("branchStatus").equipmentAttributeValue(BranchStatus.Status.PLANNED_OUTAGE)
                .equipmentType(IdentifiableType.LINE).build().toEntity());
        entities1.add(EquipmentAttributeModificationInfos.builder().id("idGenerator")
                .equipmentAttributeName("targetP").equipmentAttributeValue(50.)
                .equipmentType(IdentifiableType.GENERATOR).build().toEntity());
        entities1.add(EquipmentAttributeModificationInfos.builder().id("trf1")
                .equipmentAttributeName("ratioTapChanger.tapPosition").equipmentAttributeValue(2)
                .equipmentType(IdentifiableType.TWO_WINDINGS_TRANSFORMER).build().toEntity());
        entities1.add(EquipmentAttributeModificationInfos.builder().id("trf6")
                .equipmentAttributeName("phaseTapChanger1.tapPosition").equipmentAttributeValue(0)
                .equipmentType(IdentifiableType.THREE_WINDINGS_TRANSFORMER).build().toEntity());

        entities1.add(LoadCreationInfos.builder().id("newLoad").name("newLoad").loadType(LoadType.AUXILIARY)
                .voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).name("vn")
                .position(ConnectablePositionInfos.builder().direction(Direction.TOP).build()).build().toEntity());
        entities1.add(LoadCreationInfos.builder().id("newLoad1").name("newLoad1").loadType(LoadType.AUXILIARY)
                .voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).name("cn1")
                .position(ConnectablePositionInfos.builder().direction(Direction.BOTTOM).build()).build().toEntity());
        entities1.add(LoadCreationInfos.builder().id("newLoad2").name("newLoad2").loadType(LoadType.AUXILIARY)
                .voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).name("cn2")
                .position(ConnectablePositionInfos.builder().direction(Direction.UNDEFINED).build()).build()
                .toEntity());
        entities1.add(LoadCreationInfos.builder().id("newLoad2").name("newLoad2").loadType(LoadType.AUXILIARY)
                .voltageLevelId("v1").busOrBusbarSectionId("1.1").p0(10.).q0(20.).name(null)
                .position(ConnectablePositionInfos.builder().direction(Direction.UNDEFINED).build()).build()
                .toEntity());

        Map<String, String> properties = Map.of("DEMO", "Demo1");
        entities1.add(SubstationCreationInfos.builder().id("newSubstation").name("newSubstation")
                .substationCountry(Country.FR).properties(properties).build().toEntity());

        List<ModificationEntity> entities2 = new ArrayList<>();
        entities2.add(GeneratorCreationInfos.builder().id(NEW_GENERATOR_ID).name(NEW_GENERATOR_ID)
                .energySource(EnergySource.HYDRO).voltageLevelId("v2").busOrBusbarSectionId("1A").minActivePower(0)
                .maxActivePower(500).ratedNominalPower(1.).activePowerSetpoint(100).reactivePowerSetpoint(50.)
                .voltageRegulationOn(true).voltageSetpoint(225.).plannedActivePowerSetPoint(80.).startupCost(81.)
                .marginalCost(82.).plannedOutageRate(83.).forcedOutageRate(84.).minimumReactivePower(20.)
                .maximumReactivePower(50.).participate(true).droop(9F).transientReactance(35.)
                .stepUpTransformerReactance(25.).regulatingTerminalId("v2load").regulatingTerminalType("LOAD")
                .regulatingTerminalVlId("v2").qPercent(25.).reactiveCapabilityCurve(false)
                .reactiveCapabilityCurvePoints(List.of()).connectionName("Top")
                .connectionDirection(ConnectablePosition.Direction.TOP).connectionPosition(0).build().toEntity());
        entities2.add(LineCreationInfos.builder().id("newLine").name("newLine").r(1.0)
            .x(2.0).shuntConductance1(3.0).shuntSusceptance1(4.0).shuntConductance2(5.0)
            .shuntSusceptance2(6.0).voltageLevelId1("v1").busOrBusbarSectionId1("1.1").voltageLevelId2("v2")
            .busOrBusbarSectionId2("1B").currentLimits1(null).currentLimits2(null)
            .position1(ConnectablePositionInfos.builder()
                .label("cn101")
                .direction(Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn102")
                .direction(Direction.TOP).build())
            .build().toEntity());

        List<TapChangerStepCreationEmbeddable> tapChangerStepCreationEmbeddables = new ArrayList<>();
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 1, 1, 0, 0, 0, 0, 0.));
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 2, 1, 0, 0, 0, 0, 0.));
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, 3, 1, 0, 0, 0, 0, 0.));
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 5, 1, 0, 0, 0, 0, null));
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 6, 1, 0, 0, 0, 0, null));
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 7, 1, 0, 0, 0, 0, null));
        tapChangerStepCreationEmbeddables
                .add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, 8, 1, 0, 0, 0, 0, null));

        entities2.add(EquipmentDeletionInfos.builder().id("v2shunt").equipmentType("SHUNT_COMPENSATOR").build()
                .toEntity());
        entities2.add(GroovyScriptInfos.builder().script("network.getGenerator('idGenerator').targetP=55\n").build()
                .toEntity());
        entities2.add(BranchStatusModificationInfos.builder().id("line2")
                .action(BranchStatusModificationInfos.ActionType.TRIP).build().toEntity());
        entities2.add(VoltageLevelCreationInfos.builder().id("vl9").name("vl9").nominalVoltage(225)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("1.1", "1.1", 1, 1),
                        new BusbarSectionCreationInfos("1.2", "1.2", 1, 2)))
                .busbarConnections(List.of(new BusbarConnectionCreationInfos("1.1", "1.2", SwitchKind.BREAKER))).build()
                .toEntity());
        entities2.add(ShuntCompensatorCreationInfos.builder().id("shunt9").name("shunt9").voltageLevelId("v2")
                .busOrBusbarSectionId("1A").maximumNumberOfSections(2).currentNumberOfSections(2)
                .susceptancePerSection(1.).isIdenticalSection(true)
                .connectionDirection(ConnectablePosition.Direction.UNDEFINED).connectionName("shunt9").build()
                .toEntity());
        entities2.add(TwoWindingsTransformerCreationInfos.builder().id("new2wt").name("new2wt")
            .r(1.).x(2.).g(3.).b(4.)
            .ratedU1(5.).ratedU2(6.).ratedS(1.).voltageLevelId1("v1")
            .busOrBusbarSectionId1("1.1").voltageLevelId2("v2").busOrBusbarSectionId2("1A")
            .currentLimits1(CurrentLimitsInfos.builder().permanentLimit(3.).build())
            .currentLimits2(CurrentLimitsInfos.builder().permanentLimit(2.).build())
            .position1(ConnectablePositionInfos.builder()
                .label("cn201")
                .direction(Direction.TOP).build())
            .position2(ConnectablePositionInfos.builder()
                .label("cn202")
                .direction(Direction.TOP).build())
            .phaseTapChanger(
                PhaseTapChangerCreationInfos.builder().lowTapPosition(1).tapPosition(2)
                    .regulating(false)
                    .regulatingTerminal(RegulatingTerminalInfos.builder()
                        .id("v1load")
                        .vlId("v1")
                        .type("LOAD").build())
                    .regulationMode(PhaseTapChanger.RegulationMode.CURRENT_LIMITER)
                    .steps(List
                        .of(TapChangerStepCreationInfos.builder().index(1).rho(1).r(0).x(0).g(0)
                                .b(0).alpha(0).build(),
                            TapChangerStepCreationInfos.builder().index(2).rho(1).r(0).x(0)
                                .g(0).b(0).alpha(0.).build(),
                            TapChangerStepCreationInfos.builder().index(3).rho(1).r(0).x(0)
                                .g(0).b(0).alpha(0.).build()))
                    .build())
            .ratioTapChanger(
                RatioTapChangerCreationInfos.builder().lowTapPosition(5).tapPosition(6).regulating(true)
                    .targetDeadband(1.)
                    .regulatingTerminal(RegulatingTerminalInfos.builder()
                        .id("v2load")
                        .vlId("v2")
                        .type("LOAD").build())
                    .loadTapChangingCapabilities(true).targetV(5.)
                    .steps(List.of(
                        TapChangerStepCreationInfos.builder().index(5).rho(1).r(0).x(0).g(0)
                            .b(0).build(),
                        TapChangerStepCreationInfos.builder().index(6).rho(1).r(0).x(0).g(0)
                            .b(0).build(),
                        TapChangerStepCreationInfos.builder().index(7).rho(1).r(0).x(0).g(0)
                            .b(0).build(),
                        TapChangerStepCreationInfos.builder().index(8).rho(1).r(0).x(0).g(0)
                            .b(0).build()))
                    .build())
            .build().toEntity());
        entities2.add(LoadModificationInfos.builder().id("newLoad")
                .name(new AttributeModification<>("newLoadName", OperationType.SET)).p0(null).build()
                .toEntity());
        entities2.add(GeneratorModificationInfos.builder().id("newGenerator")
                .name(new AttributeModification<>("newGeneratorName", OperationType.SET))
                .voltageRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET)).build().toEntity());

        modificationRepository.saveModifications(TEST_GROUP_ID, entities1);
        modificationRepository.saveModifications(TEST_GROUP_ID_2, entities2);

        testNetworkModificationsCount(TEST_GROUP_ID, entities1.size());
        testNetworkModificationsCount(TEST_GROUP_ID_2, entities2.size());

        // build VARIANT_ID by cloning network initial variant and applying all
        // modifications in all groups
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID,
                TEST_REPORT_ID, List.of(TEST_GROUP_ID, TEST_GROUP_ID_2),
                List.of(TEST_SUB_REPORTER_ID_1, TEST_SUB_REPORTER_ID_2), new HashSet<>());
        String buildInfosJson = objectWriter.writeValueAsString(buildInfos);
        mockMvc.perform(
                post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(buildInfosJson))
                .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        Message<byte[]> resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
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
        assertEquals(10., network.getLoad("newLoad1").getP0(), 0.1);
        assertEquals(20., network.getLoad("newLoad1").getQ0(), 0.1);
        assertEquals(10., network.getLoad("newLoad2").getP0(), 0.1);
        assertEquals(20., network.getLoad("newLoad2").getQ0(), 0.1);
        assertEquals(EnergySource.HYDRO, network.getGenerator(NEW_GENERATOR_ID).getEnergySource());
        assertEquals("v2", network.getGenerator(NEW_GENERATOR_ID).getTerminal().getVoltageLevel().getId());
        assertEquals(500., network.getGenerator(NEW_GENERATOR_ID).getMaxP(), 0.1);
        assertEquals(100., network.getGenerator(NEW_GENERATOR_ID).getTargetP(), 0.1);
        assertEquals(80., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class)
                .getPlannedActivePowerSetpoint(), 0);
        assertEquals(81., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getStartupCost(),
                0);
        assertEquals(82., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getMarginalCost(),
                0);
        assertEquals(83.,
                network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getPlannedOutageRate(), 0);
        assertEquals(84.,
                network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorStartup.class).getForcedOutageRate(), 0);
        assertTrue(network.getGenerator(NEW_GENERATOR_ID).getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(9F, network.getGenerator(NEW_GENERATOR_ID).getExtension(ActivePowerControl.class).getDroop(), 0);
        assertEquals(35.,
                network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorShortCircuit.class).getDirectTransX(), 0);
        assertEquals(25., network.getGenerator(NEW_GENERATOR_ID).getExtension(GeneratorShortCircuit.class)
                .getStepUpTransformerX(), 0);
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
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER,
                network.getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getRegulationMode());
        assertNull(network.getShuntCompensator("v2shunt"));
        Substation newSubstation = network.getSubstation("newSubstation");
        assertEquals(Country.FR, newSubstation.getCountry().orElse(Country.AF));
        assertEquals("Demo1", network.getSubstation("newSubstation").getProperty("DEMO"));
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

        // Execute another build starting from variant VARIANT_ID to variant
        // VARIANT_ID_2
        // to check
        BuildInfos newBuildInfos = new BuildInfos(NetworkCreation.VARIANT_ID, VARIANT_ID_2, TEST_REPORT_ID,
                Collections.emptyList(), Collections.emptyList(), new HashSet<>());
        buildInfosJson = objectWriter.writeValueAsString(newBuildInfos);
        mockMvc.perform(
                post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON).content(buildInfosJson))
                .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("", new String(resultMessage.getPayload()));

        List<EquipmentInfos> eqVariant1 = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID,
                NetworkCreation.VARIANT_ID);
        List<EquipmentInfos> eqVariant2 = equipmentInfosRepository.findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID,
                VARIANT_ID_2);
        assertTrue(eqVariant2.size() > 0);
        assertEquals(eqVariant1.size(), eqVariant2.size());

        List<TombstonedEquipmentInfos> tbseqVariant1 = tombstonedEquipmentInfosRepository
                .findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, NetworkCreation.VARIANT_ID);
        List<TombstonedEquipmentInfos> tbseqVariant2 = tombstonedEquipmentInfosRepository
                .findAllByNetworkUuidAndVariantId(TEST_NETWORK_ID, VARIANT_ID_2);
        // v2shunt was deleted from initial variant => v2shunt and the cell switches
        // (breaker and disconnector) have been added as TombstonedEquipmentInfos in
        // ElasticSearch
        assertEquals(3, tbseqVariant1.size());
        assertEquals(tbseqVariant1.size(), tbseqVariant2.size());
        // deactivate some modifications and rebuild VARIANT_ID
        network.getVariantManager().cloneVariant(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID,
                true);

        AtomicReference<UUID> lineModificationEntityUuid = new AtomicReference<>();
        AtomicReference<UUID> loadCreationEntityUuid = new AtomicReference<>();
        AtomicReference<UUID> equipmentDeletionEntityUuid = new AtomicReference<>();
        List<ModificationInfos> modificationsInfos = networkModificationService.getNetworkModifications(TEST_GROUP_ID,
                false, true);
        modificationsInfos.addAll(networkModificationService.getNetworkModifications(TEST_GROUP_ID_2, false, true));
        modificationsInfos.forEach(modificationInfos -> {
            if (modificationInfos.getClass().equals(EquipmentAttributeModificationInfos.class)) {
                if (((EquipmentAttributeModificationInfos) modificationInfos).getId().equals("line1")) {
                    lineModificationEntityUuid.set(modificationInfos.getUuid());
                }
            } else if (modificationInfos.getClass().equals(LoadCreationInfos.class)) {
                if (((LoadCreationInfos) modificationInfos).getId().equals("newLoad")) {
                    loadCreationEntityUuid.set(modificationInfos.getUuid());
                }
            } else if (modificationInfos.getClass().equals(EquipmentDeletionInfos.class)) {
                if (((EquipmentDeletionInfos) modificationInfos).getId().equals("v2shunt")) {
                    equipmentDeletionEntityUuid.set(modificationInfos.getUuid());
                }
            }
        });

        buildInfos.addModificationToExclude(lineModificationEntityUuid.get());
        buildInfos.addModificationToExclude(loadCreationEntityUuid.get());
        buildInfos.addModificationToExclude(equipmentDeletionEntityUuid.get());
        buildInfosJson = objectWriter.writeValueAsString(buildInfos);

        mockMvc.perform(
                post(uriString, TEST_NETWORK_ID).content(buildInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        resultMessage = output.receive(TIMEOUT, buildResultDestination);
        assertNotNull(resultMessage);
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
        assertEquals(EnergySource.HYDRO, network.getGenerator(NEW_GENERATOR_ID).getEnergySource());
        assertEquals("v2", network.getGenerator(NEW_GENERATOR_ID).getTerminal().getVoltageLevel().getId());
        assertEquals(500., network.getGenerator(NEW_GENERATOR_ID).getMaxP(), 0.1);
        assertEquals(100., network.getGenerator(NEW_GENERATOR_ID).getTargetP(), 0.1);
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
        assertEquals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER,
                network.getTwoWindingsTransformer("new2wt").getPhaseTapChanger().getRegulationMode());
        assertNotNull(network.getShuntCompensator("v2shunt"));
        assertEquals(Country.FR, network.getSubstation("newSubstation").getCountry().orElse(Country.AF));
        assertNotNull(network.getVoltageLevel("vl9"));
        assertNotNull(network.getShuntCompensator("shunt9"));

        TestUtils.purgeRequests(server);
    }

    @Test
    public void stopBuildTest() throws Exception {
        List<ModificationEntity> entities = List.of(
                EquipmentAttributeModificationInfos.builder().id("v1d1").equipmentAttributeName("open")
                        .equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build().toEntity(),
                EquipmentAttributeModificationInfos.builder().id("line1")
                        .equipmentAttributeName("branchStatus")
                        .equipmentAttributeValue(BranchStatus.Status.PLANNED_OUTAGE)
                        .equipmentType(IdentifiableType.LINE).build().toEntity());

        modificationRepository.saveModifications(TEST_GROUP_ID, entities); // save all modification entities in group
                                                                           // TEST_GROUP_ID
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        // Build VARIANT_ID by cloning network initial variant and applying all
        // modifications in group uuid TEST_GROUP_ID
        // Because TestChannelBinder implementation is synchronous the build is made in
        // a different thread
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID,
                TEST_REPORT_ID, List.of(TEST_GROUP_ID), List.of(TEST_SUB_REPORTER_ID_1), Set.of());
        String buildInfosJson = mapper.writeValueAsString(buildInfos);
        CompletableFuture.runAsync(() -> {
            try {
                mockMvc.perform(post("/v1/networks/{networkUuid}/build?receiver=me", TEST_NETWORK_STOP_BUILD_ID)
                        .contentType(MediaType.APPLICATION_JSON).content(buildInfosJson)).andExpect(status().isOk());
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
        assertEquals(CANCEL_MESSAGE, message.getHeaders().get("message"));
    }

    @Test
    public void runBuildWithReportErrorTest() throws Exception {
        modificationRepository.saveModifications(TEST_GROUP_ID,
                List.of(EquipmentAttributeModificationInfos.builder().id("v1d1").equipmentAttributeName("open")
                        .equipmentAttributeValue(true).equipmentType(IdentifiableType.SWITCH).build().toEntity()));

        // build VARIANT_ID by cloning network initial variant and applying all
        // modifications in all groups
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID, NetworkCreation.VARIANT_ID,
                TEST_ERROR_REPORT_ID, List.of(TEST_GROUP_ID), List.of(TEST_SUB_REPORTER_ID_1), Set.of());
        mockMvc.perform(post(uriString, TEST_NETWORK_ID).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(buildInfos))).andExpect(status().isOk());

        assertTrue(TestUtils.getRequestsDone(1, server).stream().anyMatch(r -> r.matches("/v1/reports/.*")));

        assertNotNull(output.receive(TIMEOUT, consumeBuildDestination));
        assertNull(output.receive(TIMEOUT, buildResultDestination));
        Message<byte[]> message = output.receive(TIMEOUT * 3, buildFailedDestination);
        assertEquals("me", message.getHeaders().get("receiver"));
        assertThat((String) message.getHeaders().get("message"), startsWith(FAIL_MESSAGE));
    }

    @Test
    public void testApplyModificationWithErrors() {
        Network network = NetworkCreation.create(TEST_NETWORK_ID, true);
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder().voltageLevelId("unknownVoltageLevelId")
                .id("loadId").build();
        UUID groupUuid = UUID.randomUUID();
        UUID reportUuid = UUID.randomUUID();
        String reporterId = UUID.randomUUID().toString();
        String variantId = network.getVariantManager().getWorkingVariantId();

        // Building mode : No error send with exception
        List<ModificationInfos> modificationInfos = networkModificationApplicator.applyModifications(
                List.of(loadCreationInfos), new NetworkInfos(network, TEST_NETWORK_ID, true),
                new ReportInfos(reportUuid, reporterId));
        assertEquals(List.of(), modificationInfos);
        assertTrue(TestUtils.getRequestsDone(1, server).stream()
                .anyMatch(r -> r.matches(String.format("/v1/reports/%s", reportUuid))));

        // Incremental mode : Error send with exception
        NetworkInfos networkInfos = networkModificationService.getNetworkInfos(TEST_NETWORK_ID, variantId);
        ReportInfos reportInfos = new ReportInfos(reportUuid, reporterId);
        assertEquals("VOLTAGE_LEVEL_NOT_FOUND : unknownVoltageLevelId",
                assertThrows(NetworkModificationException.class, () -> networkModificationService
                        .createNetworkModification(networkInfos, groupUuid, reportInfos, loadCreationInfos))
                        .getMessage());
        assertTrue(TestUtils.getRequestsDone(1, server).stream()
                .anyMatch(r -> r.matches(String.format("/v1/reports/%s", reportUuid))));
        testNetworkModificationsCount(groupUuid, 1);

        // Save mode only (variant does not exist) : No log and no error send with
        // exception
        assertEquals(List.of(),
                networkModificationService.createNetworkModification(
                        networkModificationService.getNetworkInfos(TEST_NETWORK_ID, UUID.randomUUID().toString()),
                        groupUuid, new ReportInfos(reportUuid, reporterId), loadCreationInfos));
        testNetworkModificationsCount(groupUuid, 2);
    }

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) {
        assertEquals(actualSize, modificationRepository.getModifications(groupUuid, true, true).size());
    }

    @After
    public void tearDown() {
        List<String> destinations = List.of(consumeBuildDestination, cancelBuildDestination, buildResultDestination,
                buildStoppedDestination, buildFailedDestination);
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
