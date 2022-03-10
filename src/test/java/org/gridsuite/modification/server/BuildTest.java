/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VariantManagerConstants;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.BusbarConnectionCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.creation.BusbarSectionCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.service.BuildStoppedPublisherService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.cloud.stream.binder.test.OutputDestination;
import org.springframework.cloud.stream.binder.test.TestChannelBinderConfiguration;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.Message;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.ContextHierarchy;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.reactive.config.EnableWebFlux;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@RunWith(SpringRunner.class)
@EnableWebFlux
@AutoConfigureWebTestClient
@SpringBootTest(properties = {"spring.data.elasticsearch.enabled=true"})
@ContextHierarchy({@ContextConfiguration(classes = {NetworkModificationApplication.class, TestChannelBinderConfiguration.class})})
public class BuildTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_STOP_BUILD_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID_2 = UUID.randomUUID();
    private static final String VARIANT_ID_2 = "variant_2";

    @Autowired
    private OutputDestination output;

    @Autowired
    private WebTestClient webTestClient;

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

    private Network network;

    @Before
    public void setUp() {
        // create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });

        when(networkStoreService.getNetwork(TEST_NETWORK_STOP_BUILD_ID)).then((Answer<Network>) invocation -> {
            // Needed so the stop call doesn't arrive too late
            Thread.sleep(2000);
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

        // purge messages
        while (output.receive(1000, "build.result") != null) {
        }
        while (output.receive(1000, "build.run") != null) {
        }
        while (output.receive(1000, "build.cancel") != null) {
        }
        while (output.receive(1000, "build.stopped") != null) {
        }
    }

    @Test
    public void runBuildTest() throws InterruptedException {
        // create modification entities in the database
        List<ModificationEntity> entities1 = new ArrayList<>();
        entities1.add(modificationRepository.createEquipmentAttributeModification("v1d1", "open", true));
        entities1.add(modificationRepository.createEquipmentAttributeModification("line1", "branchStatus", BranchStatus.Status.PLANNED_OUTAGE));
        entities1.add(modificationRepository.createEquipmentAttributeModification("idGenerator", "targetP", 50.));
        entities1.add(modificationRepository.createEquipmentAttributeModification("trf1", "ratioTapChanger.tapPosition", 2));
        entities1.add(modificationRepository.createEquipmentAttributeModification("trf6", "phaseTapChanger1.tapPosition", 0));
        entities1.add(modificationRepository.createLoadEntity("newLoad", "newLoad", LoadType.AUXILIARY, "v1", "1.1", 10., 20.));
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
                                                                 List.of(TEST_GROUP_ID, TEST_GROUP_ID_2),
                                                                 new HashSet<>());
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .bodyValue(buildInfos)
            .exchange()
            .expectStatus().isOk();

        Thread.sleep(3000);  // Needed to be sure that build result message has been sent
        Message<byte[]> resultMessage = output.receive(1000, "build.result");
        assertEquals("me", resultMessage.getHeaders().get("receiver"));
        assertEquals("newSubstation,s1,s2", new String(resultMessage.getPayload()));

        // test all modifications have been made on variant VARIANT_ID
        network.getVariantManager().setWorkingVariant(NetworkCreation.VARIANT_ID);
        assertTrue(network.getSwitch("v1d1").isOpen());
        BranchStatus branchStatus = network.getLine("line1").getExtension(BranchStatus.class);
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
            Collections.emptyList(),
            new HashSet<>());
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .bodyValue(newBuildInfos)
            .exchange()
            .expectStatus().isOk();

        Thread.sleep(3000);  // Needed to be sure that build result message has been sent and data have been written in ES
        resultMessage = output.receive(1000, "build.result");
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
                if (((EquipmentAttributeModificationEntity) entity).getEquipmentId().equals("line1")) {
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

        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .bodyValue(buildInfos)
            .exchange()
            .expectStatus().isOk();

        Thread.sleep(3000);  // Needed to be sure that build result message has been sent
        resultMessage = output.receive(1000, "build.result");
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
    public void stopBuildTest() {
        List<ModificationEntity> entities = new ArrayList<>();
        entities.add(modificationRepository.createEquipmentAttributeModification("v1d1", "open", true));
        entities.add(modificationRepository.createEquipmentAttributeModification("line1", "branchStatus", BranchStatus.Status.PLANNED_OUTAGE));

        modificationRepository.saveModifications(TEST_GROUP_ID, entities);  // save all modification entities in group TEST_GROUP_ID
        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        // build VARIANT_ID by cloning network initial variant and applying all modifications in group uuid TEST_GROUP_ID
        String uriString = "/v1/networks/{networkUuid}/build?receiver=me";
        BuildInfos buildInfos = new BuildInfos(VariantManagerConstants.INITIAL_VARIANT_ID,
            NetworkCreation.VARIANT_ID,
            List.of(TEST_GROUP_ID),
            new HashSet<>());
        webTestClient.post().uri(uriString, TEST_NETWORK_STOP_BUILD_ID)
            .bodyValue(buildInfos)
            .exchange()
            .expectStatus().isOk();

        // stop build
        uriString = "/v1/build/stop?receiver=me";
        webTestClient.put()
            .uri(uriString)
            .exchange()
            .expectStatus().isOk();

        Message<byte[]> message = output.receive(3000, "build.stopped");
        assertEquals("me", message.getHeaders().get("receiver"));
        assertEquals(BuildStoppedPublisherService.CANCEL_MESSAGE, message.getHeaders().get("message"));
    }

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) {
        // get all modifications for the given group of a network
        assertEquals(actualSize, Objects.requireNonNull(webTestClient.get().uri("/v1/groups/{groupUuid}/modifications", groupUuid)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(ModificationInfos.class)
            .returnResult().getResponseBody()).size());
    }
}
