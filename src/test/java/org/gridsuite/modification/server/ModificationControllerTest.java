/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.utils.*;
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
import org.springframework.http.*;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.reactive.function.BodyInserters;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.when;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@EnableWebFlux
@AutoConfigureWebTestClient
@SpringBootTest
public class ModificationControllerTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_ID_2 = UUID.fromString("7928181e-7977-4592-ba19-88027e4254e4");
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");
    private static final UUID TEST_NETWORK_WITH_FLUSH_ERROR_ID = UUID.fromString("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_NETWORK_BUS_BREAKER_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_MIXED_TOPOLOGY_ID = UUID.fromString("22222222-7977-4592-ba19-88027e4254e4");
    public static final String VARIANT_NOT_EXISTING_ID = "variant_not_existing";

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
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });
        when(networkStoreService.getNetwork(TEST_NETWORK_ID_2)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_ID_2, false));
        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_FLUSH_ERROR_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_WITH_FLUSH_ERROR_ID, true));
        when(networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID)).then((Answer<Network>) invocation -> NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID));
        when(networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID)).then((Answer<Network>) invocation -> NetworkCreation.createMixedTopology(TEST_NETWORK_MIXED_TOPOLOGY_ID));

        doThrow(new PowsyblException()).when(networkStoreService).flush(argThat(n -> TEST_NETWORK_WITH_FLUSH_ERROR_ID.toString().equals(n.getId())));

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_NETWORK_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
            .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
    }

    @Test
    public void testModificationException() {
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR).getMessage(), MODIFICATION_ERROR.name());
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, "Error message").getMessage(), MODIFICATION_ERROR.name() + " : Error message");
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, new IllegalArgumentException("Error message")).getMessage(), MODIFICATION_ERROR.name() +  " : Error message");
    }

    @Test
    public void testEquipmentAttributeModificationInfos() {
        EquipmenAttributeModificationInfos modificationInfos = EquipmenAttributeModificationInfos.builder()
                .uuid(TEST_NETWORK_ID)
                .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentId("equipmentId")
                .substationIds(Set.of("substationId"))
                .equipmentAttributeName("equipmentAttributeName")
                .equipmentAttributeValue("equipmentAttributeValue")
                .build();

        // switch opening
        EquipmenAttributeModificationInfos modificationSwitchInfos =
                Objects.requireNonNull(webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true", TEST_NETWORK_ID, "v1b1")
                        .exchange()
                        .expectStatus().isOk()
                        .expectHeader().contentType(MediaType.APPLICATION_JSON)
                        .expectBodyList(EquipmenAttributeModificationInfos.class)
                        .returnResult()
                        .getResponseBody()).get(0);

        assertTrue(MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("v1b1", Set.of("s1"), "open", true).matchesSafely(modificationSwitchInfos));

        // switch in variant VARIANT_ID opening
        modificationSwitchInfos =
            Objects.requireNonNull(webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&variantId=" + NetworkCreation.VARIANT_ID, TEST_NETWORK_ID, "break1Variant")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmenAttributeModificationInfos.class)
                .returnResult()
                .getResponseBody()).get(0);

        assertTrue(MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("break1Variant", Set.of("s1Variant"), "open", true).matchesSafely(modificationSwitchInfos));
    }

    @Test
    public void testNetworkListener() {
        Network network = NetworkCreation.create(TEST_NETWORK_ID, true);
        NetworkStoreListener listener = NetworkStoreListener.create(network, TEST_NETWORK_ID, null, modificationRepository, equipmentInfosService, false, true);
        Generator generator = network.getGenerator("idGenerator");
        Object invalidValue = new Object();
        assertTrue(assertThrows(PowsyblException.class, () ->
            listener.storeEquipmentAttributeModification(generator, "targetP", invalidValue)).getMessage().contains("Value type invalid : Object"));
    }

    @Test
    public void testModificationGroups() {
        // no groups
        webTestClient.get().uri("/v1/groups")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(UUID.class)
                .isEqualTo(List.of());

        // switch opening to create the default group
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true", TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmenAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("v1b1", Set.of("s1"), "open", true));

        webTestClient.get().uri("/v1/groups")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(UUID.class)
                .isEqualTo(List.of(TEST_GROUP_ID));

        // delete the default modification group of a network
        webTestClient.delete().uri("/v1/groups/{groupUuid}", TEST_GROUP_ID)
                .exchange()
                .expectStatus().isOk();

        webTestClient.get().uri("/v1/groups/{groupUuid}/modifications/metadata", TEST_GROUP_ID)
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    private void switchModifications(String uriString, String switchId1, String switchNotFoundId, String switchId2, String switchId3,
                                     Set<String> substationsIds, Set<String> otherSubstationsIds,
                                     int modificationsCount) {
        // network not existing
        webTestClient.put().uri(uriString + "&open=true", NOT_FOUND_NETWORK_ID, switchId1)
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        // switch not existing
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, switchNotFoundId)
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(SWITCH_NOT_FOUND, switchNotFoundId).getMessage());

        // switch closing when already closed
        webTestClient.put().uri(uriString + "&open=false", TEST_NETWORK_ID, switchId1)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId1, Set.of(), "open", false));

        // switch opening
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, switchId1)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId1, substationsIds, "open", true));

        // switch closing
        webTestClient.put().uri(uriString + "&open=false", TEST_NETWORK_ID, switchId2)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId2, substationsIds, "open", false));

        // switch opening on another substation
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, switchId3)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId3, otherSubstationsIds, "open", true));

        testNetworkModificationsCount(TEST_GROUP_ID, modificationsCount);
    }

    @Test
    public void testSwitch() {
        String uriString = "/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID;

        // switches modifications on initial variant
        switchModifications(uriString, "v1b1", "disc1Variant", "v2b1", "v3b1", Set.of("s1"), Set.of("s2"), 4);

        // switches modifications on variant VARIANT_ID
        switchModifications(uriString + "&variantId=" + NetworkCreation.VARIANT_ID, "break1Variant", "notFound", "disc1Variant", "break2Variant", Set.of("s1Variant"), Set.of("s2Variant"), 8);
    }

    @Test
    public void testNetworkOrVariantNotFound() {
        String uriString = "/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID;

        // network not existing
        webTestClient.put().uri(uriString + "&open=true", NOT_FOUND_NETWORK_ID, "v1b1")
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());
    }

    @Test
    public void testLineStatusMofification() {
        String uriString = "/v1/networks/{networkUuid}/lines/{lineId}/status?group=" + TEST_GROUP_ID;

        // line lockout
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("lockout")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(BranchStatusModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.LOCKOUT, Set.of("s1", "s2")));

        // line switch on (already switched on)
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("switch_on")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(BranchStatusModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.SWITCH_ON, Set.of()));

        // line trip
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("trip")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(BranchStatusModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.TRIP, Set.of("s1", "s2")));

        // line energise on one end
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("energise_end_one")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(BranchStatusModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.ENERGISE_END_ONE, Set.of("s2")));

        // line energise on other end
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("energise_end_two")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(BranchStatusModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos("line2", BranchStatusModificationInfos.ActionType.ENERGISE_END_TWO, Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 5);
    }

    @Test
    public void testLineStatusMofificationWithErrors() {
        String uriString = "/v1/networks/{networkUuid}/lines/{lineId}/status?group=" + TEST_GROUP_ID;

        // network not existing
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID, "line2")
            .bodyValue("lockout")
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        // line not existing
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "notFound")
            .bodyValue("lockout")
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(LINE_NOT_FOUND, "notFound").getMessage());

        // modification action empty
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BRANCH_ACTION_TYPE_EMPTY).getMessage());

        // modification action not existing
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
            .bodyValue("foo")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(NetworkModificationException.createBranchActionTypeBad("foo").getMessage());

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
            .bodyValue("lockout")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends").getMessage());

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
            .bodyValue("trip")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends").getMessage());

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
            .bodyValue("energise_end_one")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise line end").getMessage());

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
            .bodyValue("energise_end_two")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise line end").getMessage());
    }

    @Test
    public void testGroovyWithErrors() {
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID;

        // apply null groovy script
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .exchange()
                .expectStatus().isEqualTo(HttpStatus.UNSUPPORTED_MEDIA_TYPE);

        // apply empty groovy script
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage());

        // apply empty groovy script
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("      ")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage());

        // apply groovy script with unknown generator
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('there is no generator').targetP=12\n")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());
    }

    @Test
    public void testGroovy() {
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID;

        // apply groovy script with generator target P modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=12\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with load type modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getLoad('v1load').loadType=com.powsybl.iidm.network.LoadType.FICTITIOUS\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with lcc converter station power factor modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getLccConverterStation('v1lcc').powerFactor=1\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with line R modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getLine('line1').r=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1", "s2")));

        // apply groovy script with two windings transformer ratio tap modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        // apply groovy script with three windings transformer phase tap modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherModificationInfos.createMatcherModificationInfos(ModificationType.GROOVY_SCRIPT, Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 6);
    }

    @Test
    public void testUndoModificationsOnNetworkFlushError() {
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID;

        // apply groovy script with 2 modifications with network flush error
        webTestClient.put().uri(uriString, TEST_NETWORK_WITH_FLUSH_ERROR_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .exchange()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_ERROR, PowsyblException.class.getName()).getMessage());

        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
    }

    @Test
    public void testMultipleModificationsWithError() {
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID;

        // apply groovy script without error
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .exchange()
                .expectStatus().isOk();

        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true).size());

        // apply groovy script with error ont the second
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .bodyValue("network.getGenerator('idGenerator').targetP=30\nnetwork.getGenerator('there is no generator').targetP=40\n")
            .exchange()
            .expectStatus().isBadRequest()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());

        // no modifications have been saved
        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, true).size());
    }

    @Test
    public void testCreateLoadInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/loads?group=" + TEST_GROUP_ID;

        // create new load in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .loadType(LoadType.AUXILIARY)
            .activePower(100.0)
            .reactivePower(60.0)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        assertNotNull(network.getLoad("idLoad1"));  // load was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database

        // create load with errors
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        loadCreationInfos.setEquipmentId(null);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LOAD_ERROR, "Load id is not set").getMessage());

        loadCreationInfos.setEquipmentId("idLoad1");
        loadCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage());

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage());

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setActivePower(Double.NaN);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LOAD_ERROR, "Load 'idLoad1': p0 is invalid").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be created
        uriString = "/v1/networks/{networkUuid}/loads?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID;
        loadCreationInfos.setEquipmentId("idLoad3");
        loadCreationInfos.setEquipmentName("nameLoad3");
        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("1B");
        List<EquipmenModificationInfos> modifications = webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getLoad("idLoad3"));  // load was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateLoadInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/loads?group=" + TEST_GROUP_ID;

        // create new load in voltage level with bus/breaker topology (in voltage level "VLGEN" and bus "NGEN")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v1")
            .busOrBusbarSectionId("bus1")
            .loadType(LoadType.FICTITIOUS)
            .activePower(200.0)
            .reactivePower(30.0)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create load with errors
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateGeneratorInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/generators?group=" + TEST_GROUP_ID;

        // create new generator in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        GeneratorCreationInfos generatorCreationInfos = GeneratorCreationInfos.builder()
            .equipmentId("idGenerator1")
            .equipmentName("nameGenerator1")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .energySource(EnergySource.HYDRO)
            .minActivePower(100.0)
            .maxActivePower(600.0)
            .ratedNominalPower(10.)
            .activePowerSetpoint(400.)
            .reactivePowerSetpoint(50.)
            .voltageRegulationOn(true)
            .voltageSetpoint(225.)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_CREATION, "idGenerator1", Set.of("s1")));

        assertNotNull(network.getGenerator("idGenerator1"));  // generator was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create generator with errors
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        generatorCreationInfos.setEquipmentId(null);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator id is not set").getMessage());

        generatorCreationInfos.setEquipmentId("idGenerator1");
        generatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage());

        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage());

        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfos.setMinActivePower(Double.NaN);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1': invalid value (NaN) for minimum P").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create generator on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the generator cannot be created
        uriString = "/v1/networks/{networkUuid}/generators?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID;
        generatorCreationInfos.setEquipmentId("idGenerator3");
        generatorCreationInfos.setEquipmentName("nameGenerator3");
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        List<EquipmenModificationInfos> modifications = webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getGenerator("idGenerator3"));  // generator was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateGeneratorInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/generators?group=" + TEST_GROUP_ID;

        // create new generator in voltage level with bus/breaker topology (in voltage level "VLGEN" and bus "NGEN")
        GeneratorCreationInfos generatorCreationInfos = GeneratorCreationInfos.builder()
            .equipmentId("idGenerator2")
            .equipmentName("nameGenerator2")
            .voltageLevelId("v1")
            .busOrBusbarSectionId("bus1")
            .energySource(EnergySource.HYDRO)
            .minActivePower(100.0)
            .maxActivePower(600.0)
            .ratedNominalPower(10.)
            .activePowerSetpoint(400.)
            .reactivePowerSetpoint(50.)
            .voltageRegulationOn(true)
            .voltageSetpoint(225.)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_CREATION, "idGenerator2", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create generator with errors
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateTwoWindingsTransformerInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/two-windings-transformer?group=" + TEST_GROUP_ID;

        // create new 2wt in voltage level with bus/breaker topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("bus1")
                .voltageLevelId2("v12")
                .busOrBusbarSectionId2("bus12")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmenModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create 2wt with errors
        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().is4xxClientError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateTwoWindingsTransformerInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/two-windings-transformer?group=" + TEST_GROUP_ID;

        // create new 2wt in voltage level with Node/breaker topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v2")
                .busOrBusbarSectionId2("1A")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmenModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        assertNotNull(network.getTwoWindingsTransformer("id2wt1"));  // transformer was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create transformer on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the transformer cannot be created
        uriString = "/v1/networks/{networkUuid}/two-windings-transformer?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID;
        twoWindingsTransformerCreationInfos.setEquipmentId("id2wt3");
        twoWindingsTransformerCreationInfos.setEquipmentName("name2wt3");
        List<EquipmenModificationInfos> modifications = webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getTwoWindingsTransformer("id2wt3"));  // transformer was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateTwoWindingsTransformerInMixedTopology() {
        String uriString = "/v1/networks/{networkUuid}/two-windings-transformer?group=" + TEST_NETWORK_MIXED_TOPOLOGY_ID;

        // create new 2wt in voltage level with mixed topology
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = TwoWindingsTransformerCreationInfos.builder()
                .equipmentId("id2wt1")
                .equipmentName("2wtName")
                .voltageLevelId1("v1")
                .busOrBusbarSectionId1("1.1")
                .voltageLevelId2("v3")
                .busOrBusbarSectionId2("bus3")
                .magnetizingConductance(100.0)
                .magnetizingSusceptance(200.0)
                .ratedVoltage1(1000)
                .ratedVoltage2(1010)
                .seriesReactance(300)
                .seriesResistance(400)
                .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmenModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        testNetworkModificationsCount(TEST_NETWORK_MIXED_TOPOLOGY_ID, 1);
    }

    @Test
    public void testDeleteEquipment() {
        String uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?group=" + TEST_GROUP_ID;

        // delete load
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LOAD", "v1load")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1load", "LOAD", Set.of("s1")));

        assertNull(network.getLoad("v1load"));  // load was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test delete load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be deleted
        uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID;
        List<EquipmentDeletionInfos> deletions = webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LOAD", "v3load")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .returnResult().getResponseBody();

        assertTrue(deletions.isEmpty());  // no modifications returned
        assertNotNull(network.getLoad("v3load"));  // load was not deleted
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?group=" + TEST_GROUP_ID;

        // delete equipment with errors
        webTestClient.delete().uri(uriString, NOT_FOUND_NETWORK_ID, "LOAD", "v1load")
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LOAD", "notFoundLoad")
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=notFoundLoad not found or of bad type").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        // delete shunt compensator
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "SHUNT_COMPENSATOR", "v2shunt")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2shunt", "SHUNT_COMPENSATOR", Set.of("s1")));

        assertNull(network.getShuntCompensator("v2shunt"));  // shunt compensator was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        // delete generator
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "GENERATOR", "idGenerator")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "idGenerator", "GENERATOR", Set.of("s1")));

        assertNull(network.getGenerator("idGenerator"));  // generator was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        // delete line
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LINE", "line2")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "line2", "LINE", Set.of("s1", "s2")));

        assertNull(network.getLine("line2"));  // line was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        // delete two windings transformer
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "TWO_WINDINGS_TRANSFORMER", "trf1")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf1", "TWO_WINDINGS_TRANSFORMER", Set.of("s1")));

        assertNull(network.getTwoWindingsTransformer("trf1"));  // two windings transformer was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 6);

        // delete three windings transformer
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "THREE_WINDINGS_TRANSFORMER", "trf6")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf6", "THREE_WINDINGS_TRANSFORMER", Set.of("s1")));

        assertNull(network.getThreeWindingsTransformer("trf6"));  // three windings transformer was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 7);

        // delete static var compensator
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "STATIC_VAR_COMPENSATOR", "v3Compensator")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Compensator", "STATIC_VAR_COMPENSATOR", Set.of("s2")));

        assertNull(network.getStaticVarCompensator("v3Compensator"));  // static var compensator was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 8);

        // delete battery
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "BATTERY", "v3Battery")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Battery", "BATTERY", Set.of("s2")));

        assertNull(network.getBattery("v3Battery"));  // battery was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 9);

        // delete dangling line
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "DANGLING_LINE", "v2Dangling")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2Dangling", "DANGLING_LINE", Set.of("s1")));

        assertNull(network.getDanglingLine("v2Dangling"));  // dangling line was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 10);

        // delete hvdc line
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "HVDC_LINE", "hvdcLine")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "hvdcLine", "HVDC_LINE", Set.of("s1")));

        assertNull(network.getHvdcLine("hvdcLine"));  // hvdc line was removed
        testNetworkModificationsCount(TEST_GROUP_ID, 11);

        // delete vsc converter station
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID_2, "HVDC_CONVERTER_STATION", "v2vsc")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2vsc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 12);

        // delete lcc converter station
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID_2, "HVDC_CONVERTER_STATION", "v1lcc")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1lcc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 13);
    }

    @Test
    public void testCreateLineInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID;

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "1.1" and
        //         voltage level "v2" and busbar section "1.1"
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
            .equipmentId("idLine4")
            .equipmentName("nameLine4")
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
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine4", Set.of("s1")));

        assertNotNull(network.getLine("idLine4"));  // line was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create line with errors
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        lineCreationInfos.setEquipmentId(null);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line id is not set").getMessage());

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setSeriesResistance(Double.NaN);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': r is invalid").getMessage());

        lineCreationInfos.setSeriesResistance(100.0);
        lineCreationInfos.setSeriesReactance(Double.NaN);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': x is invalid").getMessage());
        lineCreationInfos.setSeriesReactance(100.0);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        uriString = "/v1/networks/{networkUuid}/lines?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID;
        lineCreationInfos.setEquipmentId("idLine5");
        lineCreationInfos.setEquipmentName("nameLine5");
        List<EquipmenModificationInfos> modifications = webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getLine("idLine5"));  // line was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateLineInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID;

        // create new line in voltage levels with node/breaker topology
        // between voltage level "v1" and busbar section "bus1" and
        //         voltage level "v2" and busbar section "bus2"
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
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

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create line with errors
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineInMixedTypology() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID;

        // create new line in voltage levels with node breaker topology and bus breaker topology
        // between voltage level "v1" and busbar section "1.1" type NODE_BREAKER and
        //         voltage level "v2" and busbar section "bus2 type BUS_BREAKER"
        LineCreationInfos lineCreationInfos = LineCreationInfos.builder()
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
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        //create line with errors
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setBusOrBusbarSectionId2("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID;

        // create new line without shunt conductance or reactance
        LineCreationInfos lineCreationInfosNoShunt = LineCreationInfos.builder()
            .equipmentId("idLine1")
            .equipmentName("nameLine1")
            .seriesResistance(100.0)
            .seriesReactance(100.0)
            .voltageLevelId1("v1")
            .busOrBusbarSectionId1("bus1")
            .voltageLevelId2("v2")
            .busOrBusbarSectionId2("bus2")
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosNoShunt))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfosNoShunt.setShuntConductance1(50.0);
        lineCreationInfosNoShunt.setShuntConductance2(null);
        lineCreationInfosNoShunt.setShuntSusceptance1(null);
        lineCreationInfosNoShunt.setShuntSusceptance2(60.0);

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosNoShunt))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 2);

        LineCreationInfos lineCreationInfosPermanentLimitOK = LineCreationInfos.builder()
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

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosPermanentLimitOK))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine2", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        lineCreationInfosPermanentLimitOK.setCurrentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).build());
        lineCreationInfosPermanentLimitOK.setCurrentLimits2(CurrentLimitsInfos.builder().permanentLimit(null).build());

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosPermanentLimitOK))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmenModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine2", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        LineCreationInfos lineCreationInfosPermanentLimitNOK = LineCreationInfos.builder()
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

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosPermanentLimitNOK))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine2': permanent limit must be defined and be > 0").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 4);
    }

    @Test
    public void testCreateSubstation() {
        String uriString = "/v1/networks/{networkUuid}/substations?group=" + TEST_GROUP_ID;

        // create new substation
        SubstationCreationInfos substationCreationInfos = SubstationCreationInfos.builder()
                .equipmentId("SubstationId")
                .equipmentName("SubstationName")
                .substationCountry(Country.AF)
                .build();
        assertEquals("SubstationCreationInfos(super=EquipmentCreationInfos(super=EquipmenModificationInfos(super=ModificationInfos(uuid=null, date=null, type=null, substationIds=[]), equipmentId=SubstationId), equipmentName=SubstationName), substationCountry=AF)", substationCreationInfos.toString());

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(substationCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmenModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.SUBSTATION_CREATION, "SubstationId", Set.of("SubstationId")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create substation with errors
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID)
                .body(BodyInserters.fromValue(substationCreationInfos))
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        substationCreationInfos.setEquipmentId(null);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(substationCreationInfos))
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Substation id is not set").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    private void testNetworkModificationsCount(UUID groupUuid, int actualSize) {
        // get all modifications for the given group of a network
        assertEquals(actualSize, Objects.requireNonNull(webTestClient.get().uri("/v1/groups/{groupUuid}/modifications/metadata", groupUuid)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(ModificationInfos.class)
            .returnResult().getResponseBody()).size());
    }
}
