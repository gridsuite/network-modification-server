/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
@SpringBootTest
@DisableElasticsearch
@AutoConfigureMockMvc
class ParameterizedNetworkModificationControllerTest {
    private static final UUID TEST_NETWORK_ID = UUID.randomUUID();
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();

    private static final String URI_NETWORK_MODIFICATION_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIFICATION_WITH_GROUP = URI_NETWORK_MODIFICATION_BASE + "?groupUuid=" + TEST_GROUP_ID;
    private static final String URI_NETWORK_MODIFICATION_BY_UUID = URI_NETWORK_MODIFICATION_BASE + "/";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper mapper;

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    @MockitoBean
    private ReportService reportService;

    private Network network;

    @BeforeEach
    void setUp() {
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        networkModificationRepository.deleteAll();

        when(networkStoreService.getNetwork(eq(NOT_FOUND_NETWORK_ID), any(PreloadingStrategy.class))).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), any(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);
        when(networkStoreService.networkExists(TEST_NETWORK_ID)).thenReturn(true);
    }

    @AfterEach
    void tearDown() {
        networkModificationRepository.deleteAll();
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldCreateReadUpdateDeleteModificationThroughServerApi(String caseName,
                                                                  ModificationInfos modificationToCreate,
                                                                  ModificationInfos modificationToUpdate) throws Exception {
        UUID createdModificationUuid = createModification(modificationToCreate);

        if (modificationToCreate instanceof EquipmentCreationInfos creation) {
            assertNotNull(network.getIdentifiable(creation.getEquipmentId()));
        }

        assertGroupModificationCount(1);

        ModificationInfos createdModification = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).getFirst();
        assertThat(createdModification).recursivelyEquals(modificationToCreate);

        MvcResult readResult = mockMvc.perform(get(URI_NETWORK_MODIFICATION_BY_UUID + createdModificationUuid))
            .andExpect(status().isOk())
            .andReturn();
        ModificationInfos readModification = mapper.readValue(readResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(readModification).recursivelyEquals(modificationToCreate);

        mockMvc.perform(put(URI_NETWORK_MODIFICATION_BY_UUID + createdModificationUuid)
                .content(mapper.writeValueAsString(modificationToUpdate))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        ModificationInfos updatedModification = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).getFirst();
        assertThat(updatedModification).recursivelyEquals(modificationToUpdate);

        mockMvc.perform(delete(URI_NETWORK_MODIFICATION_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", createdModificationUuid.toString()))
            .andExpect(status().isOk());

        assertTrue(networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).isEmpty());
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldCreateDisabledModificationWithoutApplyingIt(String caseName,
                                                           ModificationInfos modificationToCreate,
                                                           ModificationInfos modificationToUpdate) throws Exception {
        modificationToCreate.setActivated(false);

        createModification(modificationToCreate);

        if (modificationToCreate instanceof EquipmentCreationInfos creation) {
            assertNull(network.getIdentifiable(creation.getEquipmentId()));
        }

        List<ModificationInfos> completeModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, completeModifications.size());
        assertThat(completeModifications.getFirst()).recursivelyEquals(modificationToCreate);

        List<ModificationInfos> metadataModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, metadataModifications.size());
        assertEquals(false, metadataModifications.getFirst().getActivated());
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldCopyModificationThroughServerApi(String caseName,
                                                ModificationInfos modificationToCreate,
                                                ModificationInfos modificationToUpdate) throws Exception {
        UUID modificationUuid = saveModification(modificationToCreate);

        String body = TestUtils.getJsonBody(List.of(modificationUuid), TEST_NETWORK_ID, null);

        ResultActions copyAction = mockMvc.perform(put("/v1/containers/{groupUuid}?action=COPY", TEST_GROUP_ID)
                .content(body)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(request().asyncStarted());

        mockMvc.perform(asyncDispatch(copyAction.andReturn()))
            .andExpect(status().isOk());

        List<ModificationInfos> copiedModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, copiedModifications.size());
        assertThat(copiedModifications.get(0)).recursivelyEquals(modificationToCreate);
        assertThat(copiedModifications.get(1)).recursivelyEquals(modificationToCreate);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldStashAndUnstashModificationThroughServerApi(String caseName,
                                                           ModificationInfos modificationToCreate,
                                                           ModificationInfos modificationToUpdate) throws Exception {
        UUID modificationUuid = saveModification(modificationToCreate);

        mockMvc.perform(put(URI_NETWORK_MODIFICATION_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", modificationUuid.toString())
                .queryParam("stashed", "true"))
            .andExpect(status().isOk());

        List<ModificationInfos> stashedModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true, true);
        assertEquals(1, stashedModifications.size());
        assertEquals(Boolean.TRUE, stashedModifications.getFirst().getStashed());

        mockMvc.perform(put(URI_NETWORK_MODIFICATION_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", modificationUuid.toString())
                .queryParam("stashed", "false"))
            .andExpect(status().isOk());

        List<ModificationInfos> restoredModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, restoredModifications.size());
        assertFalse(Boolean.TRUE.equals(restoredModifications.getFirst().getStashed()));
    }

    private UUID createModification(ModificationInfos modificationToCreate) throws Exception {
        prepareNetwork(modificationToCreate);
        String bodyJson = TestUtils.getJsonBody(modificationToCreate, TEST_NETWORK_ID, null);

        ResultActions createAction = mockMvc.perform(post(URI_NETWORK_MODIFICATION_WITH_GROUP)
                .content(bodyJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(request().asyncStarted());

        MvcResult createResult = mockMvc.perform(asyncDispatch(createAction.andReturn()))
            .andExpect(status().isOk())
            .andReturn();

        NetworkModificationsResult networkModificationsResult = mapper.readValue(createResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().getFirst().isPresent());
        assertNotEquals(
            NetworkModificationResult.ApplicationStatus.WITH_ERRORS,
            networkModificationsResult.modificationResults().getFirst().get().getApplicationStatus()
        );

        List<ModificationInfos> storedModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, storedModifications.size());
        return storedModifications.getFirst().getUuid();
    }

    private UUID saveModification(ModificationInfos modificationInfos) {
        prepareNetwork(modificationInfos);
        ModificationEntity entity = ModificationEntity.fromDTO(modificationInfos);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(entity));
        return entity.getId();
    }

    private void prepareNetwork(ModificationInfos modificationInfos) {
        // HVDC modifications require two converter stations of the same technology.
        if (modificationInfos instanceof VscModificationInfos) {
            network = NetworkCreation.createWithVSC(TEST_NETWORK_ID, true);
        } else if (modificationInfos instanceof LccModificationInfos) {
            network = NetworkCreation.createWithLcc(TEST_NETWORK_ID);
        }
    }

    private void assertGroupModificationCount(int expectedSize) throws Exception {
        MvcResult result = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true", TEST_GROUP_ID)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();

        List<ModificationInfos> modifications = mapper.readValue(result.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(expectedSize, modifications.size());
    }

    private static Stream<Arguments> serverModificationCases() {
        return Stream.concat(equipmentModificationCases(), equipmentCreationCases());
    }

    private static Stream<Arguments> equipmentCreationCases() {
        return Stream.of(
            Arguments.of("VSC HVDC creation", vscCreation("VSC from create", 100.0), vscCreation("VSC from update", 200.0)),
            Arguments.of("LCC HVDC creation", lccCreation("LCC from create", 100.0), lccCreation("LCC from update", 200.0))
        );
    }

    private static Stream<Arguments> equipmentModificationCases() {
        return Stream.of(
            Arguments.of(
                "equipment attribute modification",
                switchOpenModification(true),
                switchOpenModification(false)
            ),
            Arguments.of(
                "generator modification",
                generatorModification("idGenerator", "generator name from create"),
                generatorModification("idGenerator", "generator name from update")
            ),
            Arguments.of(
                "composite modification",
                compositeModification("initial description"),
                compositeModification("updated description")
            ),
            Arguments.of(
                "tabular generator modification",
                tabularModification(generatorModification("idGenerator", "generator name from tabular create")),
                tabularModification(generatorModification("idGenerator", "generator name from tabular update"))
            ),
            Arguments.of(
                "tabular load modification",
                tabularModification(loadModification("load from tabular create", 100.0, 20.0)),
                tabularModification(loadModification("load from tabular update", 150.0, 30.0))
            ),
            Arguments.of(
                "tabular battery modification",
                tabularModification(batteryModification("battery from tabular create", 10.0)),
                tabularModification(batteryModification("battery from tabular update", 20.0))
            ),
            Arguments.of(
                "tabular line modification",
                tabularModification(lineModification("line from tabular create", 5.0)),
                tabularModification(lineModification("line from tabular update", 10.0))
            ),
            Arguments.of(
                "tabular two windings transformer modification",
                tabularModification(transformerModification("transformer from tabular create", 1)),
                tabularModification(transformerModification("transformer from tabular update", 2))
            ),
            Arguments.of(
                "load modification",
                loadModification("load from create", 100.0, 20.0),
                loadModification("load from update", 150.0, 30.0)
            ),
            Arguments.of(
                "battery modification",
                batteryModification("battery from create", 10.0),
                batteryModification("battery from update", 20.0)
            ),
            Arguments.of(
                "substation modification",
                substationModification("substation from create", Country.FR),
                substationModification("substation from update", Country.BE)
            ),
            Arguments.of(
                "voltage level modification",
                voltageLevelModification("voltage level from create", 380.0),
                voltageLevelModification("voltage level from update", 400.0)
            ),
            Arguments.of(
                "line modification",
                lineModification("line from create", 5.0),
                lineModification("line from update", 10.0)
            ),
            Arguments.of(
                "two windings transformer modification",
                transformerModification("transformer from create", 1),
                transformerModification("transformer from update", 2)
            ),
            Arguments.of(
                "shunt compensator modification",
                shuntModification("shunt from create", 1),
                shuntModification("shunt from update", 2)
            ),
            Arguments.of(
                "VSC HVDC modification",
                vscModification("VSC from create", 100.0),
                vscModification("VSC from update", 200.0)
            ),
            Arguments.of(
                "LCC HVDC modification",
                lccModification("LCC from create", 100.0),
                lccModification("LCC from update", 200.0)
            )
        );
    }

    private static EquipmentAttributeModificationInfos switchOpenModification(boolean open) {
        return EquipmentAttributeModificationInfos.builder()
            .equipmentId("v1b1")
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(open)
            .stashed(false)
            .build();
    }

    private static GeneratorModificationInfos generatorModification(String generatorId, String generatorName) {
        return GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .equipmentName(new AttributeModification<>(generatorName, OperationType.SET))
            .stashed(false)
            .build();
    }

    private static CompositeModificationInfos compositeModification(String description) {
        // Names and children are edited through the dedicated composite endpoints.
        return CompositeModificationInfos.builder()
            .name("composite")
            .description(description)
            .modificationsInfos(List.of(generatorModification("idGenerator", "generator name from composite")))
            .stashed(false)
            .build();
    }

    private static TabularModificationInfos tabularModification(EquipmentModificationInfos modification) {
        return TabularModificationInfos.builder()
            .modificationType(modification.getType())
            .modifications(List.of(modification))
            .stashed(false)
            .build();
    }

    private static LoadModificationInfos loadModification(String name, double p0, double q0) {
        return LoadModificationInfos.builder()
            .equipmentId("v1load")
            .equipmentName(set(name))
            .loadType(set(LoadType.AUXILIARY))
            .p0(set(p0))
            .q0(set(q0))
            .pMeasurementValue(set(p0))
            .pMeasurementValidity(set(true))
            .qMeasurementValue(set(q0))
            .qMeasurementValidity(set(true))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static BatteryModificationInfos batteryModification(String name, double targetP) {
        return BatteryModificationInfos.builder()
            .equipmentId("v1Battery")
            .equipmentName(set(name))
            .minP(set(-100.0))
            .maxP(set(100.0))
            .targetP(set(targetP))
            .targetQ(set(5.0))
            .participate(set(true))
            .droop(set(4.0f))
            .directTransX(set(10.0))
            .stepUpTransformerX(set(5.0))
            .reactiveCapabilityCurve(set(false))
            .minQ(set(-20.0))
            .maxQ(set(20.0))
            .voltageRegulationOn(set(false))
            .targetV(set(380.0))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static SubstationModificationInfos substationModification(String name, Country country) {
        return SubstationModificationInfos.builder()
            .equipmentId("s1")
            .equipmentName(set(name))
            .country(set(country))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static VoltageLevelModificationInfos voltageLevelModification(String name, double nominalV) {
        return VoltageLevelModificationInfos.builder()
            .equipmentId("v1")
            .equipmentName(set(name))
            .nominalV(set(nominalV))
            .lowVoltageLimit(set(350.0))
            .highVoltageLimit(set(420.0))
            .ipMin(set(10.0))
            .ipMax(set(20.0))
            .busbarSectionVMeasurements(List.of(BusbarSectionVMeasurementInfos.builder()
                .busbarSectionId("1.1")
                .vMeasurementValue(set(nominalV))
                .vMeasurementValidity(set(true))
                .build()))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static LineModificationInfos lineModification(String name, double resistance) {
        return LineModificationInfos.builder()
            .equipmentId("line1")
            .enableOLGModification(true)
            .operationalLimitsGroupsModificationType(OperationalLimitsGroupsModificationType.REPLACE)
            .operationalLimitsGroups(List.of(operationalLimits(name, 100.0 * resistance)))
            .selectedOperationalLimitsGroupId1(set("test-limits"))
            .selectedOperationalLimitsGroupId2(set("test-limits"))
            .equipmentName(set(name))
            .r(set(resistance))
            .x(set(20.0))
            .g1(set(0.01))
            .g2(set(0.02))
            .b1(set(0.03))
            .b2(set(0.04))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static OperationalLimitsGroupModificationInfos operationalLimits(String propertyValue, double permanentLimit) {
        return OperationalLimitsGroupModificationInfos.builder()
            .id("test-limits")
            .modificationType(OperationalLimitsGroupModificationType.ADD)
            .temporaryLimitsModificationType(TemporaryLimitModificationType.REPLACE)
            .applicability(OperationalLimitsGroupInfos.Applicability.EQUIPMENT)
            .limitsProperties(List.of(new LimitsPropertyInfos("season", propertyValue)))
            .currentLimits(CurrentLimitsModificationInfos.builder()
                .permanentLimit(permanentLimit)
                .temporaryLimits(List.of(CurrentTemporaryLimitModificationInfos.builder()
                    .name(set("temporary-limit"))
                    .value(set(permanentLimit + 100.0))
                    .acceptableDuration(set(600))
                    .modificationType(TemporaryLimitModificationType.ADD)
                    .build()))
                .build())
            .build();
    }

    private static TwoWindingsTransformerModificationInfos transformerModification(String name, int tapPosition) {
        return TwoWindingsTransformerModificationInfos.builder()
            .equipmentId("trf1")
            .enableOLGModification(true)
            .operationalLimitsGroupsModificationType(OperationalLimitsGroupsModificationType.REPLACE)
            .operationalLimitsGroups(List.of(operationalLimits(name, 500.0 * tapPosition)))
            .selectedOperationalLimitsGroupId1(set("test-limits"))
            .selectedOperationalLimitsGroupId2(set("test-limits"))
            .equipmentName(set(name))
            .r(set(3.0))
            .x(set(15.0))
            .g(set(0.0))
            .b(set(0.00004))
            .ratedU1(set(400.0))
            .ratedU2(set(225.0))
            .ratedS(set(500.0))
            .ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .tapPosition(set(tapPosition))
                .targetV(set(225.0))
                .targetDeadband(set(2.0))
                .regulating(set(true))
                .loadTapChangingCapabilities(set(true))
                .build())
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static ShuntCompensatorModificationInfos shuntModification(String name, int sectionCount) {
        return ShuntCompensatorModificationInfos.builder()
            .equipmentId("v2shunt")
            .equipmentName(set(name))
            .maximumSectionCount(set(4))
            .sectionCount(set(sectionCount))
            .maxQAtNominalV(set(40.0))
            .shuntCompensatorType(set(ShuntCompensatorType.CAPACITOR))
            .qMeasurementValue(set(10.0 * sectionCount))
            .qMeasurementValidity(set(true))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static VscModificationInfos vscModification(String name, double activePower) {
        return VscModificationInfos.builder()
            .equipmentId("hvdcLine")
            .equipmentName(set(name))
            .nominalV(set(225.0))
            .r(set(2.0))
            .maxP(set(500.0))
            .activePowerSetpoint(set(activePower))
            .convertersMode(set(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER))
            .operatorActivePowerLimitFromSide1ToSide2(set(400.0f))
            .operatorActivePowerLimitFromSide2ToSide1(set(300.0f))
            .angleDroopActivePowerControl(set(true))
            .p0(set((float) activePower))
            .droop(set(10.0f))
            .converterStation1(ConverterStationModificationInfos.builder()
                .equipmentId("v1vsc")
                .lossFactor(set(2.0f))
                .voltageRegulationOn(set(false))
                .reactivePowerSetpoint(set(10.0))
                .voltageSetpoint(set(380.0))
                .reactiveCapabilityCurve(set(false))
                .minQ(set(-50.0))
                .maxQ(set(50.0))
                .build())
            .converterStation2(ConverterStationModificationInfos.builder()
                .equipmentId("v2vsc")
                .lossFactor(set(3.0f))
                .voltageRegulationOn(set(true))
                .voltageSetpoint(set(225.0))
                .build())
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static LccModificationInfos lccModification(String name, double activePower) {
        return LccModificationInfos.builder()
            .equipmentId("hvdcLine")
            .equipmentName(set(name))
            .nominalV(set(225.0))
            .r(set(2.0))
            .maxP(set(500.0))
            .activePowerSetpoint(set(activePower))
            .convertersMode(set(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER))
            .converterStation1(LccConverterStationModificationInfos.builder()
                .equipmentId("v1lcc")
                .lossFactor(set(2.0f))
                .powerFactor(set(0.9f))
                .shuntCompensatorsOnSide(List.of())
                .build())
            .converterStation2(LccConverterStationModificationInfos.builder()
                .equipmentId("v2lcc")
                .lossFactor(set(3.0f))
                .powerFactor(set(0.95f))
                .shuntCompensatorsOnSide(List.of())
                .build())
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static List<FreePropertyInfos> properties(String value) {
        return List.of(FreePropertyInfos.builder().name("test-property").value(value).build());
    }

    private static VscCreationInfos vscCreation(String name, double activePower) {
        return VscCreationInfos.builder()
            .equipmentId("created-vsc-line")
            .equipmentName(name)
            .nominalV(225.0)
            .r(2.0)
            .maxP(500.0)
            .activePowerSetpoint(activePower)
            .convertersMode(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER)
            .operatorActivePowerLimitFromSide1ToSide2(400.0f)
            .operatorActivePowerLimitFromSide2ToSide1(300.0f)
            .angleDroopActivePowerControl(true)
            .p0((float) activePower)
            .droop(10.0f)
            .converterStation1(vscStationCreation("created-vsc-1", "v1", "1.1", 380.0))
            .converterStation2(vscStationCreation("created-vsc-2", "v2", "1A", 225.0))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static ConverterStationCreationInfos vscStationCreation(String id, String voltageLevelId, String busbarId, double voltage) {
        return ConverterStationCreationInfos.builder()
            .equipmentId(id)
            .equipmentName(id)
            .voltageLevelId(voltageLevelId)
            .busOrBusbarSectionId(busbarId)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .lossFactor(2.0f)
            .voltageRegulationOn(true)
            .voltageSetpoint(voltage)
            .reactivePowerSetpoint(10.0)
            .reactiveCapabilityCurve(false)
            .minQ(-50.0)
            .maxQ(50.0)
            .build();
    }

    private static LccCreationInfos lccCreation(String name, double activePower) {
        return LccCreationInfos.builder()
            .equipmentId("created-lcc-line")
            .equipmentName(name)
            .nominalV(225.0)
            .r(2.0)
            .maxP(500.0)
            .activePowerSetpoint(activePower)
            .convertersMode(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER)
            .converterStation1(lccStationCreation("created-lcc-1", "v1", "1.1"))
            .converterStation2(lccStationCreation("created-lcc-2", "v2", "1A"))
            .properties(properties(name))
            .stashed(false)
            .build();
    }

    private static LccConverterStationCreationInfos lccStationCreation(String id, String voltageLevelId, String busbarId) {
        return LccConverterStationCreationInfos.builder()
            .equipmentId(id)
            .equipmentName(id)
            .voltageLevelId(voltageLevelId)
            .busOrBusbarSectionId(busbarId)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .lossFactor(2.0f)
            .powerFactor(0.9f)
            .shuntCompensatorsOnSide(List.of())
            .build();
    }

    private static <T> AttributeModification<T> set(T value) {
        return new AttributeModification<>(value, OperationType.SET);
    }
}
