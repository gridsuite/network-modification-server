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
import nl.jqno.equalsverifier.EqualsVerifier;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkModificationService;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.utils.*;
import org.junit.After;
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.hamcrest.Matchers.containsString;
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
@SpringBootTest(properties = {"spring.data.elasticsearch.enabled=true"})
public class ModificationControllerTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_ID_2 = UUID.fromString("7928181e-7977-4592-ba19-88027e4254e4");
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");
    private static final UUID TEST_NETWORK_WITH_FLUSH_ERROR_ID = UUID.fromString("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_NETWORK_BUS_BREAKER_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_NETWORK_MIXED_TOPOLOGY_ID = UUID.fromString("22222222-7977-4592-ba19-88027e4254e4");
    public static final String VARIANT_NOT_EXISTING_ID = "variant_not_existing";
    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

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

    private Network network2;

    @Before
    public void setUp() {
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = NetworkCreation.create(TEST_NETWORK_ID, true);
            return network;
        });
        when(networkStoreService.getNetwork(TEST_NETWORK_ID_2)).then((Answer<Network>) invocation -> {
            network2 = NetworkCreation.create(TEST_NETWORK_ID_2, false);
            return network2;
        });
        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_FLUSH_ERROR_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_WITH_FLUSH_ERROR_ID, true));
        when(networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID)).then((Answer<Network>) invocation -> NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID));
        when(networkStoreService.getNetwork(TEST_NETWORK_MIXED_TOPOLOGY_ID)).then((Answer<Network>) invocation -> NetworkCreation.createMixedTopology(TEST_NETWORK_MIXED_TOPOLOGY_ID));

        doThrow(new PowsyblException()).when(networkStoreService).flush(argThat(n -> TEST_NETWORK_WITH_FLUSH_ERROR_ID.toString().equals(n.getId())));

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
            .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @After
    public void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
        equipmentInfosService.deleteAll();
    }

    @Test
    public void testModificationException() {
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR).getMessage(), MODIFICATION_ERROR.name());
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, "Error message").getMessage(), MODIFICATION_ERROR.name() + " : Error message");
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, new IllegalArgumentException("Error message")).getMessage(), MODIFICATION_ERROR.name() +  " : Error message");
    }

    @Test
    public void testEquipmentAttributeModificationInfos() {
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder()
                .uuid(TEST_NETWORK_ID)
                .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
                .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
                .equipmentId("equipmentId")
                .substationIds(Set.of("substationId"))
                .equipmentAttributeName("equipmentAttributeName")
                .equipmentAttributeValue("equipmentAttributeValue")
                .build();
        assertEquals("EquipmentAttributeModificationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=7928181c-7977-4592-ba19-88027e4254e4, date=2021-02-19T00:00Z, type=EQUIPMENT_ATTRIBUTE_MODIFICATION, substationIds=[substationId]), equipmentId=equipmentId), equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue)", modificationInfos.toString());

        // switch opening
        EquipmentAttributeModificationInfos modificationSwitchInfos =
                Objects.requireNonNull(webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&reportUuid=" + TEST_REPORT_ID, TEST_NETWORK_ID, "v1b1")
                        .exchange()
                        .expectStatus().isOk()
                        .expectHeader().contentType(MediaType.APPLICATION_JSON)
                        .expectBodyList(EquipmentAttributeModificationInfos.class)
                        .returnResult()
                        .getResponseBody()).get(0);

        assertTrue(MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("v1b1", Set.of("s1"), "open", true).matchesSafely(modificationSwitchInfos));

        // switch in variant VARIANT_ID opening
        modificationSwitchInfos =
            Objects.requireNonNull(webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&variantId=" + NetworkCreation.VARIANT_ID + "&reportUuid=" + TEST_REPORT_ID, TEST_NETWORK_ID, "break1Variant")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentAttributeModificationInfos.class)
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
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&reportUuid=" + TEST_REPORT_ID, TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentAttributeModificationInfos.class)
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
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId1, Set.of(), "open", false));

        // switch opening
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, switchId1)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId1, substationsIds, "open", true));

        // switch closing
        webTestClient.put().uri(uriString + "&open=false", TEST_NETWORK_ID, switchId2)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId2, substationsIds, "open", false));

        // switch opening on another substation
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, switchId3)
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos(switchId3, otherSubstationsIds, "open", true));

        testNetworkModificationsCount(TEST_GROUP_ID, modificationsCount);
    }

    @Test
    public void testSwitch() {
        String uriString = "/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        // switches modifications on initial variant
        switchModifications(uriString, "v1b1", "disc1Variant", "v2b1", "v3b1", Set.of("s1"), Set.of("s2"), 4);

        // switches modifications on variant VARIANT_ID
        switchModifications(uriString + "&variantId=" + NetworkCreation.VARIANT_ID, "break1Variant", "notFound", "disc1Variant", "break2Variant", Set.of("s1Variant"), Set.of("s2Variant"), 8);
    }

    @Test
    public void testDeleteModification() {
        var res = webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&reportUuid=" + TEST_REPORT_ID, TEST_NETWORK_ID, "v1b1")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .returnResult().getResponseBody();
        assertNotNull(res);
        assertEquals(1, res.size());

        assertEquals(1, modificationRepository.getModifications(TEST_GROUP_ID, false).size());
        String deleteStrWrongGroup = "/v1/groups/" + UUID.randomUUID() + "/modifications?modificationsUuids=" + res.get(0).getUuid();
        String deleteStr = "/v1/groups/" + TEST_GROUP_ID + "/modifications";

        webTestClient.delete().uri(deleteStrWrongGroup)
            .exchange()
            .expectStatus().isNotFound();

        webTestClient.delete()
            .uri(uriBuilder -> uriBuilder.path(deleteStr).queryParam("modificationsUuids", List.of(res.get(0).getUuid())).build())
            .exchange()
            .expectStatus().isOk();

        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID, false).size());

        /* non existing modification */
        webTestClient.delete()
            .uri(uriBuilder -> uriBuilder.path(deleteStr).queryParam("modificationsUuids", List.of(res.get(0).getUuid())).build())
            .exchange()
            .expectStatus().isNotFound();

    }

    @Test
    public void testNetworkOrVariantNotFound() {
        String uriString = "/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        // network not existing
        webTestClient.put().uri(uriString + "&open=true", NOT_FOUND_NETWORK_ID, "v1b1")
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());
    }

    @Test
    public void testLineStatusModification() {
        String uriString = "/v1/networks/{networkUuid}/lines/{lineId}/status?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        var res = webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
            .bodyValue("trip")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(BranchStatusModificationInfos.class)
            .returnResult().getResponseBody();
        assertEquals(1, res.size());
        var matcher = MatcherBranchStatusModificationInfos.createMatcherBranchStatusModificationInfos("line3", BranchStatusModificationInfos.ActionType.TRIP, Set.of("s1", "s2"));
        assertTrue(res.stream().anyMatch(matcher::matchesSafely));

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

        testNetworkModificationsCount(TEST_GROUP_ID, 6);
    }

    @Test
    public void testLineStatusModificationWithErrors() {
        String uriString = "/v1/networks/{networkUuid}/lines/{lineId}/status?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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
            .isEqualTo(NetworkModificationException.createBranchActionTypeUnknown("foo").getMessage());

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
            .bodyValue("lockout")
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
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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
        String uriString = "/v1/networks/{networkUuid}/loads?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        assertNotNull(network.getLoad("idLoad1"));  // load was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database

        // create load with errors
        webTestClient.post().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        loadCreationInfos.setEquipmentId(null);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LOAD_ERROR, "Load id is not set").getMessage());

        loadCreationInfos.setEquipmentId("idLoad1");
        loadCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage());

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage());

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setActivePower(Double.NaN);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LOAD_ERROR, "Load 'idLoad1': p0 is invalid").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be created
        uriString = "/v1/networks/{networkUuid}/loads?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        loadCreationInfos.setEquipmentId("idLoad3");
        loadCreationInfos.setEquipmentName("nameLoad3");
        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("1B");
        List<EquipmentModificationInfos> modifications = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getLoad("idLoad3"));  // load was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateLoadInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/loads?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1"))).returnResult().getResponseBody().get(0);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Update load creation
        loadCreationInfos = new LoadCreationEntity(
                "idLoad1edited",
                "nameLoad1edited",
                LoadType.AUXILIARY,
                "v12",
                "bus12",
                175.0,
                60.0)
                .toModificationInfos();
        loadCreationInfos.setUuid(result.getUuid());

        LoadCreationInfos loadCreationUpdate = new LoadCreationEntity(
                "idLoad1edited",
                "nameLoad1edited",
                LoadType.AUXILIARY,
                "v12",
                "bus12",
                175.0,
                60.0)
                .toModificationInfos();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/loads-creation";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(loadCreationUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(LoadCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherLoadCreationInfos.createMatcherLoadCreationInfos(loadCreationInfos));

        // create load with errors
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testModifyLoad() {
        EqualsVerifier.simple().forClass(AttributeModification.class).verify();

        String uriString = "/v1/networks/{networkUuid}/loads-modification?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(LoadType.AUXILIARY, OperationType.SET))
                .activePower(new AttributeModification<>(100.0, OperationType.SET))
                .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(loadModificationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_MODIFICATION, "v1load", Set.of("s1")));

        assertNotNull(network.getLoad("v1load"));  // load was modified
        assertEquals(network.getLoad("v1load").getLoadType(), LoadType.AUXILIARY);
        assertEquals(network.getLoad("v1load").getP0(), 100.0, 0.1);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database

        // modify load with errors
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID)
                .body(BodyInserters.fromValue(loadModificationInfos))
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        loadModificationInfos.setEquipmentId(null);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(loadModificationInfos))
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFY_LOAD_ERROR, "Missing required attributes to modify the equipment").getMessage());

        loadModificationInfos.setEquipmentId("unknownLoadId");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(loadModificationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_MODIFICATION, "unknownLoadId", Set.of()));
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // Modify all attributes of the load
        loadModificationInfos = LoadModificationInfos.builder()
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1Load", OperationType.SET))
                .activePower(new AttributeModification<>(80.0, OperationType.SET))
                .reactivePower(new AttributeModification<>(40.0, OperationType.SET))
                .voltageLevelId(new AttributeModification<>("newVlId", OperationType.SET))
                .busOrBusbarSectionId(new AttributeModification<>("newBusbarId", OperationType.SET))
                .build();

        EquipmentModificationInfos result = webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(loadModificationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LOAD_MODIFICATION, "v1load", Set.of("s1"))).returnResult().getResponseBody().get(0);

        assertNotNull(network.getLoad("v1load"));  // load was modified
        // TODO uncomment when load name modification will be enabled in Powsybl
        //assertEquals(network.getLoad("v1load").getNameOrId(), "newV1Load");
        assertEquals(network.getLoad("v1load").getLoadType(), LoadType.FICTITIOUS);
        assertEquals(network.getLoad("v1load").getP0(), 80.0, 0.1);
        assertEquals(network.getLoad("v1load").getQ0(), 40.0, 0.1);
        // TODO check connectivity when it will be implemented
        testNetworkModificationsCount(TEST_GROUP_ID, 3);  // new modification stored in the database

        // Unset an attribute that should not be null
        loadModificationInfos = LoadModificationInfos.builder()
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(null, OperationType.UNSET))
                .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(loadModificationInfos))
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFY_LOAD_ERROR, "Load 'v1load': load type is null").getMessage());

        // Update load modification
        loadModificationInfos = new LoadModificationEntity(
                "v1load",
                null,
                new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET),
                null,
                null,
                null,
                new AttributeModification<>(70.0, OperationType.SET))
                .toModificationInfos();
        loadModificationInfos.setUuid(result.getUuid());

        LoadModificationInfos loadModificationUpdate = LoadModificationInfos.builder()
                .equipmentId("v1load")
                .loadType(new AttributeModification<>(LoadType.FICTITIOUS, OperationType.SET))
                .reactivePower(new AttributeModification<>(70.0, OperationType.SET))
                .build();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/loads-modification";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(loadModificationUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(LoadModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherLoadModificationInfos.createMatcherLoadModificationInfos(loadModificationInfos));

    }

    @Test
    public void testModifyGenerator() {
        String uriString = "/v1/networks/{networkUuid}/generators-modification?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        String generatorId = "idGenerator";
        GeneratorModificationInfos generatorModificationInfos = GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
            .maxActivePower(new AttributeModification<>(100.0, OperationType.SET))
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, generatorId, Set.of("s1")));

        assertNotNull(network.getGenerator(generatorId));  // generator was modified
        assertEquals(EnergySource.HYDRO, network.getGenerator(generatorId).getEnergySource());
        assertEquals(100.0, network.getGenerator(generatorId).getMaxP(), 0.1);
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database

        // modify generator with errors
        webTestClient.put().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        generatorModificationInfos.setEquipmentId(null);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Missing required attributes to modify the equipment").getMessage());

        String anotherId = "unknownGeneratorId";
        generatorModificationInfos.setEquipmentId(anotherId);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, anotherId, Set.of()));
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        // Modify all attributes of the generator
        generatorModificationInfos = GeneratorModificationInfos.builder()
            .type(ModificationType.GENERATOR_MODIFICATION)
            .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
            .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
            .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
            .voltageSetpoint(new AttributeModification<>(48.0, OperationType.SET))
            .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
            .minActivePower(new AttributeModification<>(0., OperationType.SET))
            .maxActivePower(new AttributeModification<>(100., OperationType.SET))
            .ratedNominalPower(new AttributeModification<>(220., OperationType.SET))
            .equipmentId(generatorId)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, generatorId, Set.of("s1")));

        assertNotNull(network.getGenerator(generatorId));  // generator was modified
        // TODO test name change when it will be implemented
        var equipment = network.getGenerator(generatorId);
        assertEquals(EnergySource.SOLAR, equipment.getEnergySource());
        assertEquals(80.0, equipment.getTargetP(), .1);
        assertEquals(40.0, equipment.getTargetQ(), .1);
        assertEquals(48.0, equipment.getTargetV(), .1);
        assertTrue(equipment.isVoltageRegulatorOn());
        assertEquals(0.0, equipment.getMinP(), .1);
        assertEquals(100.0, equipment.getMaxP(), .1);
        assertEquals(220.0, equipment.getRatedS(), .1);

        // TODO check connectivity when it will be implemented
        testNetworkModificationsCount(TEST_GROUP_ID, 3);  // new modification stored in the database

        // Unset an attribute that should not be null
        generatorModificationInfos = GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(null, OperationType.UNSET))
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Generator '" + generatorId + "': energy source is not set").getMessage());

    }

    @Test
    public void testUpdateModifyGenerator() {
        String uriString = "/v1/networks/{networkUuid}/generators-modification?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        String generatorId = "idGenerator";
        GeneratorModificationInfos generatorModificationInfos = GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
            .maxActivePower(new AttributeModification<>(100.0, OperationType.SET))
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_MODIFICATION, generatorId, Set.of("s1")));

        var listModifications = modificationRepository.getModifications(TEST_GROUP_ID, true);
        assertEquals(1, listModifications.size());

        generatorModificationInfos = GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
            .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
            .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
            .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
            .voltageSetpoint(new AttributeModification<>(48.0, OperationType.SET))
            .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
            .minActivePower(new AttributeModification<>(0., OperationType.SET))
            .maxActivePower(new AttributeModification<>(100., OperationType.SET))
            .ratedNominalPower(new AttributeModification<>(220., OperationType.SET))
            .uuid(listModifications.get(0).getUuid())
            .type(ModificationType.GENERATOR_MODIFICATION)
            .build();

        uriString = "/v1/modifications/{modificationUUID}/generators-modification";

        webTestClient.put().uri(uriString, listModifications.get(0).getUuid())
            .body(BodyInserters.fromValue(generatorModificationInfos))
            .exchange()
            .expectStatus().isOk();

        var modifications = modificationRepository.getModifications(TEST_GROUP_ID, false);

        assertEquals(1, modifications.size());
        modifications.get(0).setDate(listModifications.get(0).getDate()); // this one is modified by sql database
        assertEquals(generatorModificationInfos, modifications.get(0));
    }

    @Test
    public void testCreateShuntCompensator() {
        String uriString = "/v1/networks/{networkUuid}/shunt-compensators?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        ShuntCompensatorCreationInfos shunt1 = ShuntCompensatorCreationInfos.builder()
            .equipmentId("shuntOneId").equipmentName("hop")
            .currentNumberOfSections(4).maximumNumberOfSections(9)
            .susceptancePerSection(1.).isIdenticalSection(true)
            .voltageLevelId("v2").busOrBusbarSectionId("1B")
            .build();

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(shunt1))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.SHUNT_COMPENSATOR_CREATION, "shuntOneId", Set.of("s1")))
                .returnResult().getResponseBody().get(0);

        assertNotNull(network.getShuntCompensator("shuntOneId"));  // shunt compensator was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        shunt1 = new ShuntCompensatorCreationEntity(shunt1).toModificationInfos();
        shunt1.setEquipmentId("shuntOneIdEdited");
        shunt1.setEquipmentName("hopEdited");
        shunt1.setIsIdenticalSection(false);
        shunt1.setCurrentNumberOfSections(6);
        shunt1.setMaximumNumberOfSections(12);
        shunt1.setSusceptancePerSection(2.);
        shunt1.setVoltageLevelId("v4");
        shunt1.setBusOrBusbarSectionId("1.A");
        shunt1.setUuid(result.getUuid());
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/shunt-compensators-creation";

        // Update shunt compansator creation
        ShuntCompensatorCreationInfos shuntUpdate = new ShuntCompensatorCreationEntity(shunt1).toModificationInfos();
        shuntUpdate.setEquipmentId("shuntOneIdEdited");
        shuntUpdate.setEquipmentName("hopEdited");
        shuntUpdate.setIsIdenticalSection(false);
        shuntUpdate.setCurrentNumberOfSections(6);
        shuntUpdate.setMaximumNumberOfSections(12);
        shuntUpdate.setSusceptancePerSection(2.);
        shuntUpdate.setVoltageLevelId("v4");
        shuntUpdate.setBusOrBusbarSectionId("1.A");
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(shuntUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ShuntCompensatorCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherShuntCompensatorCreationInfos.createMatcher(shunt1));

        shunt1.setMaximumNumberOfSections(2);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(shunt1))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class);
    }

    @Test
    public void testCreateGeneratorInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/generators?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_CREATION, "idGenerator1", Set.of("s1")))
                .returnResult().getResponseBody().get(0);

        assertNotNull(network.getGenerator("idGenerator1"));  // generator was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        generatorCreationInfos = new GeneratorCreationEntity(
                "idGenerator1Edited",
                "nameGenerator1Edited",
                EnergySource.SOLAR,
                "v1",
                "1A",
                150.,
                300.,
                15.,
                450.,
                55.,
                false,
                235.)
                .toModificationInfos();
        generatorCreationInfos.setUuid(result.getUuid());

        // Update generator creation
        GeneratorCreationInfos generatorCreationUpdate = new GeneratorCreationEntity(
                "idGenerator1Edited",
                "nameGenerator1Edited",
                EnergySource.SOLAR,
                "v1",
                "1A",
                150.,
                300.,
                15.,
                450.,
                55.,
                false,
                235.)
                .toModificationInfos();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/generators-creation";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(generatorCreationUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(GeneratorCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherGeneratorCreationInfos.createMatcherGeneratorCreationInfos(generatorCreationInfos));

        // create generator with errors
        webTestClient.post().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        generatorCreationInfos.setEquipmentId(null);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator id is not set").getMessage());

        generatorCreationInfos.setEquipmentId("idGenerator1");
        generatorCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage());

        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage());

        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        generatorCreationInfos.setMinActivePower(Double.NaN);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator 'idGenerator1': invalid value (NaN) for minimum P").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create generator on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the generator cannot be created
        uriString = "/v1/networks/{networkUuid}/generators?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        generatorCreationInfos.setEquipmentId("idGenerator3");
        generatorCreationInfos.setEquipmentName("nameGenerator3");
        generatorCreationInfos.setVoltageLevelId("v2");
        generatorCreationInfos.setBusOrBusbarSectionId("1B");
        List<EquipmentModificationInfos> modifications = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getGenerator("idGenerator3"));  // generator was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateGeneratorInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/generators?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.GENERATOR_CREATION, "idGenerator2", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create generator with errors
        generatorCreationInfos.setBusOrBusbarSectionId("notFoundBus");
        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(generatorCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateTwoWindingsTransformerInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/two-windings-transformers?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")))
                .returnResult().getResponseBody().get(0);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        twoWindingsTransformerCreationInfos = new TwoWindingsTransformerCreationEntity(
                "id2wt1Edited",
                "2wtNameEdited",
                150.,
                250.,
                1005.,
                1015.,
                350.,
                450.,
                "v12",
                "bus12",
                "v1",
                "bus1",
                50.,
                55.
                )
                .toModificationInfos();
        twoWindingsTransformerCreationInfos.setUuid(result.getUuid());

        // Update 2wt creation
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationUpdate = new TwoWindingsTransformerCreationEntity(
                "id2wt1Edited",
                "2wtNameEdited",
                150.,
                250.,
                1005.,
                1015.,
                350.,
                450.,
                "v12",
                "bus12",
                "v1",
                "bus1",
                50.,
                55.
        ).toModificationInfos();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/two-windings-transformers-creation";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(TwoWindingsTransformerCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherTwoWindingsTransformerCreationInfos.createMatcherTwoWindingsTransformerCreationInfos(twoWindingsTransformerCreationInfos));

        // create 2wt with errors
        twoWindingsTransformerCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().is4xxClientError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateTwoWindingsTransformerInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/two-windings-transformers?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        assertNotNull(network.getTwoWindingsTransformer("id2wt1"));  // transformer was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create transformer on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the transformer cannot be created
        uriString = "/v1/networks/{networkUuid}/two-windings-transformers?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        twoWindingsTransformerCreationInfos.setEquipmentId("id2wt3");
        twoWindingsTransformerCreationInfos.setEquipmentName("name2wt3");
        List<EquipmentModificationInfos> modifications = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getTwoWindingsTransformer("id2wt3"));  // transformer was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateTwoWindingsTransformerInMixedTopology() {
        String uriString = "/v1/networks/{networkUuid}/two-windings-transformers?group=" + TEST_NETWORK_MIXED_TOPOLOGY_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
                .body(BodyInserters.fromValue(twoWindingsTransformerCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, "id2wt1", Set.of("s1")));

        testNetworkModificationsCount(TEST_NETWORK_MIXED_TOPOLOGY_ID, 1);
    }

    @Test
    public void testDeleteEquipment() {
        String uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());
        assertTrue(equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());

        // delete load
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LOAD", "v1load")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1load", "LOAD", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // load and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getLoad("v1load"));
        assertNull(network.getSwitch("v1d1"));
        assertNull(network.getSwitch("v1b1"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1load", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1d1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1b1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // Test delete load on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the load cannot be deleted
        uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        List<EquipmentDeletionInfos> deletions = webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LOAD", "v3load")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .returnResult().getResponseBody();

        assertTrue(deletions.isEmpty());  // no modifications returned
        assertNotNull(network.getLoad("v3load"));  // load was not deleted
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database

        uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        // shunt compensator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getShuntCompensator("v2shunt"));
        assertNull(network.getSwitch("v2bshunt"));
        assertNull(network.getSwitch("v2dshunt"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2shunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bshunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dshunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete generator
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "GENERATOR", "idGenerator")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "idGenerator", "GENERATOR", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 4);

        // generator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getGenerator("idGenerator"));
        assertNull(network.getSwitch("v2bgenerator"));
        assertNull(network.getSwitch("v2dgenerator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("idGenerator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bgenerator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dgenerator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete line
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LINE", "line2")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "line2", "LINE", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 5);

        // line and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getLine("line2"));
        assertNull(network.getSwitch("v1dl2"));
        assertNull(network.getSwitch("v1bl2"));
        assertNull(network.getSwitch("v3dl2"));
        assertNull(network.getSwitch("v3bl2"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("line2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1bl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bl2", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete two windings transformer
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "TWO_WINDINGS_TRANSFORMER", "trf1")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf1", "TWO_WINDINGS_TRANSFORMER", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 6);

        // 2 windings transformer and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getTwoWindingsTransformer("trf1"));
        assertNull(network.getSwitch("v1btrf1"));
        // disconnector 'v1dtrf1' was not removed (2wt 'trf1' in double feeder with 3wt 'trf6' in voltage level 'v1')
        assertNotNull(network.getSwitch("v1dtrf1"));
        assertNull(network.getSwitch("v2btrf1"));
        assertNull(network.getSwitch("v2dtrf1"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("trf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1btrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(equipmentInfosService.existTombstonedEquipmentInfos("v1dtrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2btrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dtrf1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete three windings transformer
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "THREE_WINDINGS_TRANSFORMER", "trf6")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "trf6", "THREE_WINDINGS_TRANSFORMER", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 7);

        // 3 windings transformer and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getThreeWindingsTransformer("trf6"));
        // breaker 'v1btrf6' was not removed (special connection of 3wt 'trf6' in voltage level 'v1')
        // disconnector 'v1dtrf6' was not removed (special connection of 3wt 'trf6' in voltage level 'v1')
        assertNotNull(network.getSwitch("v1btrf6"));
        assertNotNull(network.getSwitch("v1dtrf6"));
        assertNull(network.getSwitch("v2btrf6"));
        assertNull(network.getSwitch("v2dtrf6"));
        assertNull(network.getSwitch("v4btrf6"));
        assertNull(network.getSwitch("v4dtrf6"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("trf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(equipmentInfosService.existTombstonedEquipmentInfos("v1btrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertFalse(equipmentInfosService.existTombstonedEquipmentInfos("v1dtrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2btrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dtrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v4btrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v4dtrf6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete static var compensator
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "STATIC_VAR_COMPENSATOR", "v3Compensator")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Compensator", "STATIC_VAR_COMPENSATOR", Set.of("s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 8);

        // static var compensator and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getStaticVarCompensator("v3Compensator"));
        assertNull(network.getSwitch("v3dCompensator"));
        assertNull(network.getSwitch("v3bCompensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3Compensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dCompensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bCompensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete battery
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "BATTERY", "v3Battery")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v3Battery", "BATTERY", Set.of("s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 9);

        // battery and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getBattery("v3Battery"));
        assertNull(network.getSwitch("v3dBattery"));
        assertNull(network.getSwitch("v3bBattery"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3Battery", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3dBattery", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v3bBattery", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete dangling line
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "DANGLING_LINE", "v2Dangling")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2Dangling", "DANGLING_LINE", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 10);

        // dangling line and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getDanglingLine("v2Dangling"));
        assertNull(network.getSwitch("v2bdangling"));
        assertNull(network.getSwitch("v2ddangling"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2Dangling", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bdangling", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2ddangling", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete hvdc line
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "HVDC_LINE", "hvdcLine")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "hvdcLine", "HVDC_LINE", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 11);

        // hvdc line has been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getHvdcLine("hvdcLine"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("hvdcLine", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete vsc converter station
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID_2, "HVDC_CONVERTER_STATION", "v2vsc")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v2vsc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 12);

        // vsc converter station and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network2.getVscConverterStation("v2vsc"));
        assertNull(network2.getSwitch("v2bvsc"));
        assertNull(network2.getSwitch("v2dvsc"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2vsc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2bvsc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v2dvsc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete lcc converter station
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID_2, "HVDC_CONVERTER_STATION", "v1lcc")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v1lcc", "HVDC_CONVERTER_STATION", Set.of("s1")));

        testNetworkModificationsCount(TEST_GROUP_ID, 13);

        // lcc converter station and switches have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network2.getLccConverterStation("v1lcc"));
        assertNull(network2.getSwitch("v1dlcc"));
        assertNull(network2.getSwitch("v1blcc"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1lcc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1dlcc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v1blcc", TEST_NETWORK_ID_2, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete voltage level
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "VOLTAGE_LEVEL", "v5")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "v5", "VOLTAGE_LEVEL", Set.of("s3")));

        testNetworkModificationsCount(TEST_GROUP_ID, 14);

        // voltage level and equipments have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getVoltageLevel("v5"));
        assertNull(network.getBusbarSection("1A1"));
        assertNull(network.getLoad("v5load"));
        assertNull(network.getGenerator("v5generator"));
        assertNull(network.getShuntCompensator("v5shunt"));
        assertNull(network.getStaticVarCompensator("v5Compensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("1A1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5load", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5generator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5shunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v5Compensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete voltage level (fail because the vl is connected)
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "VOLTAGE_LEVEL", "v4")
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .value(exception -> exception, containsString("\"status\":500,\"error\":\"Internal Server Error\",\"message\":\"The voltage level 'v4' cannot be removed because of a remaining THREE_WINDINGS_TRANSFORMER"));
        assertNotNull(network.getVoltageLevel("v4"));

        // delete substation
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "SUBSTATION", "s3")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentDeletionInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentDeletionInfos.createMatcherEquipmentDeletionInfos(ModificationType.EQUIPMENT_DELETION, "s3", "SUBSTATION", Set.of("s3")));

        testNetworkModificationsCount(TEST_GROUP_ID, 15);

        // substation and equipments have been removed from network and added as TombstonedEquipmentInfos in ElasticSearch
        assertNull(network.getSubstation("s3"));
        assertNull(network.getVoltageLevel("v6"));
        assertNull(network.getBusbarSection("1B1"));
        assertNull(network.getLoad("v6load"));
        assertNull(network.getGenerator("v6generator"));
        assertNull(network.getShuntCompensator("v6shunt"));
        assertNull(network.getStaticVarCompensator("v6Compensator"));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("s3", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("1B1", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6load", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6generator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6shunt", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));
        assertTrue(equipmentInfosService.existTombstonedEquipmentInfos("v6Compensator", TEST_NETWORK_ID, VariantManagerConstants.INITIAL_VARIANT_ID));

        // delete substation (fail because the substations is connected)
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "SUBSTATION", "s2")
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .value(exception -> exception, containsString("DELETE_EQUIPMENT_ERROR : The substation s2 is still connected to another substation"));
        assertNotNull(network.getSubstation("s2"));

        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID).isEmpty());
        assertTrue(equipmentInfosService.findAllEquipmentInfos(TEST_NETWORK_ID_2).isEmpty());
        assertEquals(52, equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID).size());
        assertEquals(6, equipmentInfosService.findAllTombstonedEquipmentInfos(TEST_NETWORK_ID_2).size());
    }

    @Test
    public void testErrorRemovingDanglingSwitches() {
        String uriString = "/v1/networks/{networkUuid}/equipments/type/{equipmentType}/id/{equipmentId}?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        // delete load with error removing dangling switches, because the load connection node is not linked to any other node
        webTestClient.delete().uri(uriString, TEST_NETWORK_ID, "LOAD", "v5load")
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .value(exception -> exception, containsString("DELETE_EQUIPMENT_ERROR : no such vertex in graph: 2"));

        // no modifications added
        webTestClient.get().uri("/v1/groups")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(UUID.class)
            .isEqualTo(List.of());
    }

    @Test
    public void testMoveModification() {
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&reportUuid=" + TEST_REPORT_ID, TEST_NETWORK_ID, "v1b1")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .returnResult().getResponseBody();
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true" + "&reportUuid=" + TEST_REPORT_ID, TEST_NETWORK_ID, "v1b1")
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentAttributeModificationInfos.class)
            .returnResult().getResponseBody();

        var modificationList = networkModificationService.getModifications(TEST_GROUP_ID, true).map(ModificationInfos::getUuid).collectList().block();
        assertNotNull(modificationList);
        assertEquals(2, modificationList.size());
        webTestClient.put().uri("/v1/groups/" + TEST_GROUP_ID
                    + "/modifications/move?before=" + modificationList.get(0)
                    + "&modificationsToMove=" + modificationList.get(1))
            .exchange()
            .expectStatus().isOk();

        var newModificationList = networkModificationService.getModifications(TEST_GROUP_ID, true).map(ModificationInfos::getUuid).collectList().block();
        assertNotNull(newModificationList);
        Collections.reverse(newModificationList);

        assertEquals(modificationList, newModificationList);
    }

    @Test
    public void testCreateLineInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        assertEquals("LineCreationInfos(super=BranchCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, date=null, type=null, substationIds=[]), equipmentId=idLine4), equipmentName=nameLine4), seriesResistance=100.0, seriesReactance=100.0, voltageLevelId1=v1, voltageLevelId2=v2, busOrBusbarSectionId1=1.1, busOrBusbarSectionId2=1A, currentLimits1=null, currentLimits2=null), shuntConductance1=10.0, shuntSusceptance1=10.0, shuntConductance2=20.0, shuntSusceptance2=20.0)", lineCreationInfos.toString());

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine4", Set.of("s1")))
                .returnResult().getResponseBody().get(0);

        assertNotNull(network.getLine("idLine4"));  // line was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfos = new LineCreationEntity(
                "idLine4edited",
                "nameLine4edited",
                110.0,
                110.0,
                15.0,
                15.,
                25.,
                25.,
                "v2",
                "1A",
                "v1",
                "1.1",
                5.,
                5.)
                .toModificationInfos();
        lineCreationInfos.setUuid(result.getUuid());

        // Update load creation
        LineCreationInfos lineCreationUpdate = new LineCreationEntity(
                "idLine4edited",
                "nameLine4edited",
                110.0,
                110.0,
                15.0,
                15.,
                25.,
                25.,
                "v2",
                "1A",
                "v1",
                "1.1",
                5.,
                5.).toModificationInfos();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/lines-creation";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(lineCreationUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(LineCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherLineCreationInfos.createMatcherLineCreationInfos(lineCreationInfos));

        // create line with errors
        webTestClient.post().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        lineCreationInfos.setEquipmentId(null);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line id is not set").getMessage());

        lineCreationInfos.setEquipmentId("idLine4");
        lineCreationInfos.setVoltageLevelId1("notFoundVoltageLevelId1");
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBusbarSection1");
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection1").getMessage());

        lineCreationInfos.setVoltageLevelId1("v1");
        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setSeriesResistance(Double.NaN);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': r is invalid").getMessage());

        lineCreationInfos.setSeriesResistance(100.0);
        lineCreationInfos.setSeriesReactance(Double.NaN);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine4': x is invalid").getMessage());
        lineCreationInfos.setSeriesReactance(100.0);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // Test create line on not yet existing variant VARIANT_NOT_EXISTING_ID :
        // Only the modification should be added in the database but the line cannot be created
        uriString = "/v1/networks/{networkUuid}/lines?variantId=" + VARIANT_NOT_EXISTING_ID + "&group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;
        lineCreationInfos.setEquipmentId("idLine5");
        lineCreationInfos.setEquipmentName("nameLine5");
        List<EquipmentModificationInfos> modifications = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .returnResult().getResponseBody();

        assertTrue(modifications.isEmpty());  // no modifications returned
        assertNull(network.getLine("idLine5"));  // line was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 2);  // new modification stored in the database
    }

    @Test
    public void testCreateLineInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        // create line with errors
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineInMixedTypology() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        //create line with errors
        lineCreationInfos.setBusOrBusbarSectionId1("notFoundBus");
        webTestClient.post().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfos.setBusOrBusbarSectionId1("1.1");
        lineCreationInfos.setBusOrBusbarSectionId2("notFoundBus");
        webTestClient.post().uri(uriString, TEST_NETWORK_MIXED_TOPOLOGY_ID)
            .body(BodyInserters.fromValue(lineCreationInfos))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLineOptionalParameters() {
        String uriString = "/v1/networks/{networkUuid}/lines?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

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

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosNoShunt))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine1", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        lineCreationInfosNoShunt.setShuntConductance1(50.0);
        lineCreationInfosNoShunt.setShuntConductance2(null);
        lineCreationInfosNoShunt.setShuntSusceptance1(null);
        lineCreationInfosNoShunt.setShuntSusceptance2(60.0);

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosNoShunt))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
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

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosPermanentLimitOK))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.LINE_CREATION, "idLine2", Set.of("s1", "s2")));

        testNetworkModificationsCount(TEST_GROUP_ID, 3);

        lineCreationInfosPermanentLimitOK.setCurrentLimits1(CurrentLimitsInfos.builder().permanentLimit(5.0).build());
        lineCreationInfosPermanentLimitOK.setCurrentLimits2(CurrentLimitsInfos.builder().permanentLimit(null).build());

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosPermanentLimitOK))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
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

        webTestClient.post().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(lineCreationInfosPermanentLimitNOK))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LINE_ERROR, "AC Line 'idLine2': permanent limit must be defined and be > 0").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 4);
    }

    @Test
    public void testCreateSubstation() {
        String uriString = "/v1/networks/{networkUuid}/substations?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        // create new substation
        SubstationCreationInfos substationCreationInfos = SubstationCreationInfos.builder()
                .equipmentId("SubstationId")
                .equipmentName("SubstationName")
                .substationCountry(Country.AF)
                .build();
        assertEquals("SubstationCreationInfos(super=EquipmentCreationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=null, date=null, type=null, substationIds=[]), equipmentId=SubstationId), equipmentName=SubstationName), substationCountry=AF)", substationCreationInfos.toString());

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(substationCreationInfos))
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(EquipmentModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.SUBSTATION_CREATION, "SubstationId", Set.of("SubstationId")))
                .returnResult().getResponseBody().get(0);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        substationCreationInfos = new SubstationCreationEntity(
                "SubstationIdEdited",
                "SubstationNameEdited",
                Country.CI)
                .toModificationInfos();
        substationCreationInfos.setUuid(result.getUuid());

        // Update substation creation
        SubstationCreationInfos substationCreationUpdate = new SubstationCreationEntity(
                "SubstationIdEdited",
                "SubstationNameEdited",
                Country.CI).toModificationInfos();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/substations-creation";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(substationCreationUpdate))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(SubstationCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherSubstationCreationInfos.createMatcherSubstationCreationInfos(substationCreationInfos));

        // create substation with errors
        webTestClient.post().uri(uriString, NOT_FOUND_NETWORK_ID)
                .body(BodyInserters.fromValue(substationCreationInfos))
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        substationCreationInfos.setEquipmentId(null);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(substationCreationInfos))
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(CREATE_SUBSTATION_ERROR, "Substation id is not set").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateVoltageLevel() {
        String uriString = "/v1/networks/{networkUuid}/voltage-levels?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        List<BusbarSectionCreationInfos> busbarSectionInfos = new ArrayList<>();
        busbarSectionInfos.add(BusbarSectionCreationInfos.builder().id("bbs.nw").name("SJB NO").vertPos(1).horizPos(1).build());
        busbarSectionInfos.add(BusbarSectionCreationInfos.builder().id("bbs.ne").name("SJB NE").vertPos(1).horizPos(2).build());
        busbarSectionInfos.add(BusbarSectionCreationInfos.builder().id("bbs.sw").name("SJB SW").vertPos(2).horizPos(1).build());

        List<BusbarConnectionCreationInfos> busbarConnectionInfos = new ArrayList<>();
        busbarConnectionInfos.add(
            BusbarConnectionCreationInfos.builder().fromBBS("bbs.nw").toBBS("bbs.ne").switchKind(SwitchKind.BREAKER).build());
        busbarConnectionInfos.add(
            BusbarConnectionCreationInfos.builder().fromBBS("bbs.nw").toBBS("bbs.sw").switchKind(SwitchKind.DISCONNECTOR).build());
        busbarConnectionInfos.add(
            BusbarConnectionCreationInfos.builder().fromBBS("bbs.ne").toBBS("bbs.ne").switchKind(SwitchKind.DISCONNECTOR).build());

        VoltageLevelCreationInfos vli;

        vli = VoltageLevelCreationInfos.builder()
            .equipmentId("VoltageLevelId")
            .equipmentName("VoltageLevelName")
            .nominalVoltage(379.1)
            .substationId("absent_station")
            .busbarSections(busbarSectionInfos)
            .busbarConnections(busbarConnectionInfos)
            .build();

        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(vli))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(SUBSTATION_NOT_FOUND, "absent_station").getMessage());

        vli = VoltageLevelCreationInfos.builder()
            .equipmentId("VoltageLevelId")
            .equipmentName("VoltageLevelName")
            .nominalVoltage(379.1)
            .substationId("s1")
            .busbarSections(busbarSectionInfos)
            .busbarConnections(busbarConnectionInfos)
            .build();

        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(vli))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Disconnector between same bus bar section 'bbs.ne'").getMessage());

        // then success
        busbarConnectionInfos.remove(2);
        vli = VoltageLevelCreationInfos.builder()
            .equipmentId("VoltageLevelId")
            .equipmentName("VoltageLevelName")
            .nominalVoltage(379.1)
            .substationId("s1")
            .busbarSections(busbarSectionInfos)
            .busbarConnections(busbarConnectionInfos)
            .build();

        EquipmentModificationInfos result = webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(vli))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos(ModificationType.VOLTAGE_LEVEL_CREATION, "VoltageLevelId", Set.of("s1")))
                .returnResult().getResponseBody().get(0);

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        vli = new VoltageLevelCreationEntity(
                "VoltageLevelIdEdited",
                "VoltageLevelEdited",
                385.,
                "s2",
                List.of(),
                List.of())
                .toModificationInfos();
        vli.setUuid(result.getUuid());

        // Update voltage level creation
        VoltageLevelCreationInfos vlu = new VoltageLevelCreationEntity(
                "VoltageLevelIdEdited",
                "VoltageLevelEdited",
                385.,
                "s2",
                List.of(),
                List.of())
                .toModificationInfos();
        String uriStringForUpdate = "/v1/modifications/" + result.getUuid() + "/voltage-levels-creation";
        webTestClient.put().uri(uriStringForUpdate)
                .body(BodyInserters.fromValue(vlu))
                .exchange()
                .expectStatus().isOk();

        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        webTestClient.get().uri("/v1/modifications/" + result.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(VoltageLevelCreationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherVoltageLevelCreationInfos.createMatcherVoltageLevelCreationInfos(vli));

        // create substation with errors
        webTestClient.post().uri(uriString, NOT_FOUND_NETWORK_ID)
            .body(BodyInserters.fromValue(vli))
            .exchange()
            .expectStatus().isNotFound()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        vli.setEquipmentId(null);
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(vli))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Voltage level id is not set").getMessage());

        testNetworkModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testGroupDuplication() {
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
        webTestClient.post().uri(uriString, TEST_NETWORK_ID)
                .body(BodyInserters.fromValue(loadCreationInfos))
                .exchange()
                .expectStatus().isOk();
        assertNotNull(network.getLoad("idLoad1"));  // load was created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);

        UUID duplicatedGroupUuid = UUID.randomUUID();
        uriString = "/v1/groups?duplicateFrom=" + TEST_GROUP_ID + "&groupUuid=" + duplicatedGroupUuid;
        webTestClient.post().uri(uriString)
                .exchange()
                .expectStatus().isOk();
        testNetworkModificationsCount(duplicatedGroupUuid, 1);

        uriString = "/v1/groups?duplicateFrom=" + UUID.randomUUID() + "&groupUuid=" + UUID.randomUUID();
        webTestClient.post().uri(uriString)
                .exchange()
                .expectStatus().isNotFound();
    }
    
    public void testLineSplitWithVoltageLevel() {
        String lineSplitUriString = "/v1/networks/{networkUuid}/line-splits?group=" + TEST_GROUP_ID + "&reportUuid=" + TEST_REPORT_ID;

        VoltageLevelCreationInfos vl1 = VoltageLevelCreationInfos.builder()
            .equipmentId("vl1")
            .equipmentName("NewVoltageLevel")
            .nominalVoltage(379.3)
            .substationId("s1")
            .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
            .busbarConnections(List.of())
            .build();

        LineSplitWithVoltageLevelInfos lineSplitAbsentLine = new LineSplitWithVoltageLevelInfos("absent_line_id", 10.0, vl1, null, "v1bbs",
            "nl1", "NewLine1", "nl2", "NewLine2");

        webTestClient.post().uri(lineSplitUriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineSplitAbsentLine))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage());

        LineSplitWithVoltageLevelInfos lineSplitWoVL = new LineSplitWithVoltageLevelInfos("line3", 10.0, null, "v4", "1.A",
            "nl1", "NewLine1", "nl2", "NewLine2");

        webTestClient.post().uri(lineSplitUriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineSplitWoVL))
            .exchange()
            .expectStatus().isOk();

        LineSplitWithVoltageLevelInfos lineSplitWithNewVL = new LineSplitWithVoltageLevelInfos("line2", 10.0, vl1, null, "v1bbs",
            "nl1v", "NewLine1", "nl2v", "NewLine2");

        List<EquipmentModificationInfos> result = webTestClient.post().uri(lineSplitUriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(lineSplitWithNewVL))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(EquipmentModificationInfos.class)
            .returnResult().getResponseBody();

        assertNotNull(result);
        Optional<EquipmentModificationInfos> lineSplitProper = result.stream().filter(r -> r.getType() == ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL).findFirst();
        assertTrue(lineSplitProper.isPresent());
        assertEquals(2, lineSplitProper.get().getSubstationIds().size());

        Optional<EquipmentModificationInfos> lineDeletion = result.stream().filter(r -> r.getType() == ModificationType.EQUIPMENT_DELETION).findFirst();
        assertTrue(lineDeletion.isPresent());
        // EquipmentDeletionInfos is erased down to a EquipmentModificationInfos by ->json->
        //   but handled ok in StudyServer where we added equipementType to EquipementModificationInfos.
        //assertTrue(lineDeletion.get() instanceof EquipmentDeletionInfos);
        //assertEquals("LINE", ((EquipmentDeletionInfos)lineDeletion.get()).getEquipmentType());

        LineSplitWithVoltageLevelInfos lineSplitWithNewVLUpd = new LineSplitWithVoltageLevelInfos("line2", 20.0, vl1, null, "v1bbs",
            "nl1v", "NewLine1", "nl2v", "NewLine2");

        webTestClient.put().uri("/v1/modifications/" + new UUID(128, 16) + "/line-splits")
            .body(BodyInserters.fromValue(lineSplitWithNewVLUpd))
            .exchange()
            .expectStatus().is4xxClientError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(LINE_SPLIT_NOT_FOUND, "Line split not found").getMessage());

        webTestClient.put().uri("/v1/modifications/" + lineSplitProper.get().getUuid() + "/line-splits")
            .body(BodyInserters.fromValue(lineSplitWithNewVLUpd))
            .exchange()
            .expectStatus().isOk();
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
