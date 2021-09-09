/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.sld.iidm.extensions.BranchStatus;
import org.gridsuite.modification.server.dto.ElementaryAttributeModificationInfos;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.utils.MatcherElementaryAttributeModificationInfos;
import org.gridsuite.modification.server.utils.MatcherElementaryModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.reactive.function.BodyInserters;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.argThat;
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
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");
    private static final UUID TEST_NETWORK_WITH_FLUSH_ERROR_ID = UUID.fromString("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_NETWORK_BUS_BREAKER_ID = UUID.fromString("11111111-7977-4592-ba19-88027e4254e4");

    private static final String ERROR_MESSAGE = "Error message";

    @Autowired
    private WebTestClient webTestClient;

    @MockBean
    private NetworkStoreService networkStoreService;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Before
    public void setUp() {
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_ID));
        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(TEST_NETWORK_WITH_FLUSH_ERROR_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_WITH_FLUSH_ERROR_ID));
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> NetworkCreation.create(TEST_NETWORK_ID));
        when(networkStoreService.getNetwork(TEST_NETWORK_BUS_BREAKER_ID)).then((Answer<Network>) invocation -> NetworkCreation.createBusBreaker(TEST_NETWORK_BUS_BREAKER_ID));

        doThrow(new PowsyblException()).when(networkStoreService).flush(argThat(n -> TEST_NETWORK_WITH_FLUSH_ERROR_ID.toString().equals(n.getId())));

        // clean DB
        modificationRepository.deleteAll();
    }

    @Test
    public void testModificationException() {
        assertEquals(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage(), GROOVY_SCRIPT_EMPTY.name() + " : " + NetworkModificationException.EMPTY_SCRIPT);
        assertEquals(new NetworkModificationException(GROOVY_SCRIPT_EMPTY, ERROR_MESSAGE).getMessage(), GROOVY_SCRIPT_EMPTY.name() + " : " + ERROR_MESSAGE);
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR).getMessage(), MODIFICATION_ERROR.name());
        assertEquals(new NetworkModificationException(MODIFICATION_ERROR, ERROR_MESSAGE).getMessage(), MODIFICATION_ERROR.name() + " : " + ERROR_MESSAGE);
    }

    @Test
    public void testElementaryModificationInfos() {
        ElementaryAttributeModificationInfos modificationInfos = ElementaryAttributeModificationInfos.builder()
                .uuid(TEST_NETWORK_ID)
                .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
                .type(ModificationType.ELEMENTARY)
                .equipmentId("equipmentId")
                .substationIds(Set.of("substationId"))
                .equipmentAttributeName("equipmentAttributeName")
                .equipmentAttributeValue("equipmentAttributeValue")
                .build();
        assertEquals("ElementaryAttributeModificationInfos(super=ElementaryModificationInfos(super=ModificationInfos(uuid=7928181c-7977-4592-ba19-88027e4254e4, date=2021-02-19T00:00Z, type=ELEMENTARY), equipmentId=equipmentId, substationIds=[substationId]), equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue)", modificationInfos.toString());

        // switch opening
        ElementaryAttributeModificationInfos modificationSwitchInfos =
                Objects.requireNonNull(webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID + "&open=true", TEST_NETWORK_ID, "v1b1")
                        .exchange()
                        .expectStatus().isOk()
                        .expectHeader().contentType(MediaType.APPLICATION_JSON)
                        .expectBodyList(ElementaryAttributeModificationInfos.class)
                        .returnResult()
                        .getResponseBody()).get(0);

        assertTrue(MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1b1", Set.of("s1"), "open", true).matchesSafely(modificationSwitchInfos));

        webTestClient.get().uri("/v1/networks/modifications/group/{groupUuid}/elementarymodifications/{modificationUuid}", TEST_GROUP_ID, modificationSwitchInfos.getUuid())
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBody(ElementaryAttributeModificationInfos.class)
                .value(MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1b1", Set.of(), "open", true));

        webTestClient.get().uri("/v1/networks/modifications/group/{groupUuid}/elementarymodifications/{modificationUuid}", TEST_GROUP_ID, TEST_NETWORK_ID)
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_NOT_FOUND, TEST_NETWORK_ID.toString()).getMessage());
    }

    @Test
    public void testNetworkListener() {
        Network network = NetworkCreation.create(TEST_NETWORK_ID);
        NetworkStoreListener listener = NetworkStoreListener.create(network, TEST_GROUP_ID, modificationRepository);
        Generator generator = network.getGenerator("idGenerator");
        Object invalidValue = new Object();
        assertTrue(assertThrows(PowsyblException.class, () ->
                listener.onUpdate(generator, "targetP", 0, invalidValue)).getMessage().contains("Value type invalid : Object"));
    }

    @Test
    public void testModificationGroups() {
        // no groups
        webTestClient.get().uri("/v1/networks/modificationgroups")
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
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1b1", Set.of("s1"), "open", true));

        webTestClient.get().uri("/v1/networks/modificationgroups")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(UUID.class)
                .isEqualTo(List.of(TEST_GROUP_ID));

        // delete the default modification group of a network
        webTestClient.delete().uri("/v1/networks/modifications/group/{groupUuid}", TEST_GROUP_ID)
                .exchange()
                .expectStatus().isOk();

        webTestClient.get().uri("/v1/networks/modifications/group/{groupUuid}", TEST_GROUP_ID)
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, TEST_GROUP_ID.toString()).getMessage());
    }

    @Test
    public void testSwitch() {
        String uriString = "/v1/networks/{networkUuid}/switches/{switchId}?group=" + TEST_GROUP_ID;

        // network not existing
        webTestClient.put().uri(uriString + "&open=true", NOT_FOUND_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        // switch not existing
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, "notFound")
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(SWITCH_NOT_FOUND, "notFound").getMessage());

        // switch closing when already closed
        webTestClient.put().uri(uriString + "&open=false", TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .isEqualTo(List.of());

        // switch opening
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1b1", Set.of("s1"), "open", true));

        // switch closing
        webTestClient.put().uri(uriString + "&open=false", TEST_NETWORK_ID, "v2b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v2b1", Set.of("s1"), "open", false));

        // switch opening on another substation
        webTestClient.put().uri(uriString + "&open=true", TEST_NETWORK_ID, "v3b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v3b1", Set.of("s2"), "open", true));

        testNetwokModificationsCount(TEST_GROUP_ID, 3);
    }

    @Test
    public void testLine() {
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

        // line lockout
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
                .bodyValue("lockout")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1bl1", Set.of("s1"), "open", true))
                .value(modifications -> modifications.get(1),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v3bl1", Set.of("s2"), "open", true))
                .value(modifications -> modifications.get(2),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("line2", Set.of("s1", "s2"), "branchStatus", BranchStatus.Status.PLANNED_OUTAGE.name()));

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
                .bodyValue("lockout")
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends").getMessage());

        // line switch on
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
                .bodyValue("switchOn")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .isEqualTo(List.of());

        // line trip
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
                .bodyValue("trip")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1bl1", Set.of("s1"), "open", true))
                .value(modifications -> modifications.get(1),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v3bl1", Set.of("s2"), "open", true))
                .value(modifications -> modifications.get(2),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("line2", Set.of("s1", "s2"), "branchStatus", BranchStatus.Status.FORCED_OUTAGE.name()));

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
                .bodyValue("trip")
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_ERROR, "Unable to disconnect both line ends").getMessage());

        // line energise on one end
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
                .bodyValue("energiseEndOne")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v3bl1", Set.of("s2"), "open", true));

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
                .bodyValue("energiseEndOne")
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_ERROR, "Unable to energise line end").getMessage());

        // line energise on other end
        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line2")
                .bodyValue("energiseEndTwo")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1bl1", Set.of("s1"), "open", true));

        webTestClient.put().uri(uriString, TEST_NETWORK_ID, "line3")
                .bodyValue("energiseEndTwo")
                .exchange()
                .expectStatus().is5xxServerError()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_ERROR, "Unable to energise line end").getMessage());

        testNetwokModificationsCount(TEST_GROUP_ID, 8);
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
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("idGenerator", Set.of("s1"), "targetP", 12.0));

        // apply groovy script with load type modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getLoad('v1load').loadType=com.powsybl.iidm.network.LoadType.FICTITIOUS\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1load", Set.of("s1"), "loadType", "FICTITIOUS"));

        // apply groovy script with lcc converter station power factor modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getLccConverterStation('v1lcc').powerFactor=1\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("v1lcc", Set.of("s1"), "powerFactor", 1.0));

        // apply groovy script with line R modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getLine('line1').r=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("line1", Set.of("s1", "s2"), "r", 2.0));

        // apply groovy script with two windings transformer ratio tap modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("trf1", Set.of("s1"), "ratioTapChanger.tapPosition", 2));

        // apply groovy script with three windings transformer phase tap modification
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        MatcherElementaryAttributeModificationInfos.createMatcherElementaryAttributeModificationInfos("trf6", Set.of("s1"), "phaseTapChanger1.tapPosition", 0));

        testNetwokModificationsCount(TEST_GROUP_ID, 6);
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

        assertEquals(0, modificationRepository.getModifications(TEST_GROUP_ID).size());
    }

    @Test
    public void testMultipleModificationsWithError() {
        String uriString = "/v1/networks/{networkUuid}/groovy?group=" + TEST_GROUP_ID;

        // apply groovy script with 2 modifications without error
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=10\nnetwork.getGenerator('idGenerator').targetP=20\n")
                .exchange()
                .expectStatus().isOk();

        assertEquals(2, modificationRepository.getModifications(TEST_GROUP_ID).size());

        // apply groovy script with 2 modifications with error ont the second
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=30\nnetwork.getGenerator('there is no generator').targetP=40\n")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());

        // the last 2 modifications have not been saved
        assertEquals(2, modificationRepository.getModifications(TEST_GROUP_ID).size());
    }

    @Test
    public void testCreateLoadInNodeBreaker() {
        String uriString = "/v1/networks/{networkUuid}/createLoad?group=" + TEST_GROUP_ID;

        // create new load in voltage level with node/breaker topology (in voltage level "v2" and busbar section "1B")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v2")
            .busId("1B")
            .loadType(LoadType.AUXILIARY)
            .activePower(100.0)
            .reactivePower(60.0)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(ElementaryModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        testNetwokModificationsCount(TEST_GROUP_ID, 1);

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
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage());

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusId("notFoundBusbarSection");
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage());

        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusId("1B");
        loadCreationInfos.setActivePower(Double.NaN);
        webTestClient.put().uri(uriString, TEST_NETWORK_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(CREATE_LOAD_ERROR, "Load 'idLoad1': p0 is invalid").getMessage());

        testNetwokModificationsCount(TEST_GROUP_ID, 1);
    }

    @Test
    public void testCreateLoadInBusBreaker() {
        String uriString = "/v1/networks/{networkUuid}/createLoad?group=" + TEST_GROUP_ID;

        // create new load in voltage level with bus/breaker topology (in voltage level "VLGEN" and bus "NGEN")
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v1")
            .busId("bus1")
            .loadType(LoadType.FICTITIOUS)
            .activePower(200.0)
            .reactivePower(30.0)
            .build();

        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().isOk()
            .expectHeader().contentType(MediaType.APPLICATION_JSON)
            .expectBodyList(ElementaryModificationInfos.class)
            .value(modifications -> modifications.get(0),
                MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos(ModificationType.LOAD_CREATION, "idLoad1", Set.of("s1")));

        testNetwokModificationsCount(TEST_GROUP_ID, 1);

        // create load with errors
        loadCreationInfos.setBusId("notFoundBus");
        webTestClient.put().uri(uriString, TEST_NETWORK_BUS_BREAKER_ID)
            .body(BodyInserters.fromValue(loadCreationInfos))
            .exchange()
            .expectStatus().is5xxServerError()
            .expectBody(String.class)
            .isEqualTo(new NetworkModificationException(BUS_NOT_FOUND, "notFoundBus").getMessage());

        testNetwokModificationsCount(TEST_GROUP_ID, 1);
    }

    private void testNetwokModificationsCount(UUID groupUuid, int actualSize) {
        // get all modifications for the given group of a network
        assertEquals(actualSize, Objects.requireNonNull(webTestClient.get().uri("/v1/networks/modifications/group/{groupUuid}", groupUuid)
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryAttributeModificationInfos.class)
                .returnResult().getResponseBody()).size());
    }
}
