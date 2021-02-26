/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.service.NetworkStoreListener;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.json.JSONArray;
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

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos;
import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@RunWith(SpringRunner.class)
@EnableWebFlux
@AutoConfigureWebTestClient
@SpringBootTest
@AutoConfigureEmbeddedDatabase(provider = AutoConfigureEmbeddedDatabase.DatabaseProvider.ZONKY)
public class NetworkModificationTest {

    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa");

    private static final String ERROR_MESSAGE = "Error message";

    @Autowired
    private WebTestClient webTestClient;

    @MockBean
    private NetworkStoreService networkStoreService;

    @Before
    public void setUp() {
        // /!\ create a new network for each invocation (answer)
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> NetworkCreation.create());
        when(networkStoreService.getNetwork(NOT_FOUND_NETWORK_ID)).thenThrow(new PowsyblException());
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
        ElementaryModificationInfos modificationInfos = ElementaryModificationInfos.builder()
                .uuid(TEST_NETWORK_ID)
                .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
                .type(ModificationType.ELEMENTARY)
                .equipmentId("equipmentId")
                .substationIds(Set.of("substationId"))
                .equipmentAttributeName("equipmentAttributeName")
                .equipmentAttributeValue("equipmentAttributeValue")
                .build();
        assertEquals("ElementaryModificationInfos(super=ModificationInfos(uuid=7928181c-7977-4592-ba19-88027e4254e4, date=2021-02-19T00:00Z, type=ELEMENTARY), equipmentId=equipmentId, substationIds=[substationId], equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue)",
                modificationInfos.toString());
    }

    @Test
    public void testNetworkListener() {
        Network network = NetworkCreation.create();
        NetworkStoreListener listener = NetworkStoreListener.create(network, TEST_NETWORK_ID, null);
        Generator generator = network.getGenerator("idGenerator");
        Object invalidValue = new Object();
        assertTrue(assertThrows(PowsyblException.class, () ->
                listener.onUpdate(generator, "targetP", 0, invalidValue)).getMessage().contains("Value type invalid : Object"));
    }

    @Test
    public void testSwitch() {
        // network not existing
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=true", NOT_FOUND_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(NETWORK_NOT_FOUND, NOT_FOUND_NETWORK_ID.toString()).getMessage());

        // switch not existing
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=true", TEST_NETWORK_ID, "notFound")
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(SWITCH_NOT_FOUND, "notFound").getMessage());

        // switch closing when already closed
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=false", TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .isEqualTo(List.of());

        // switch opening
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=true", TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v1b1", Set.of("s1"), "open", true));

        // switch closing
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=false", TEST_NETWORK_ID, "v2b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v2b1", Set.of("s1"), "open", false));

        // switch opening on another substation
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=true", TEST_NETWORK_ID, "v3b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v3b1", Set.of("s2"), "open", true));

        testDeleteNetwokModifications(TEST_NETWORK_ID, 3);
    }

    @Test
    public void testGroovyWithErrors() {
        // apply null groovy script
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .exchange()
                .expectStatus().isEqualTo(HttpStatus.UNSUPPORTED_MEDIA_TYPE);

        // apply empty groovy script
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage());

        // apply empty groovy script
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .bodyValue("      ")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_EMPTY).getMessage());

        // apply groovy script with unknown generator
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('there is no generator').targetP=12\n")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());
    }

    @Test
    public void testGroovy() {

        // apply groovy script with generator target P modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=12\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("idGenerator", Set.of("s1"), "targetP", 12.0));

        // apply groovy script with load type modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("network.getLoad('v1load').loadType=com.powsybl.iidm.network.LoadType.FICTITIOUS\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v1load", Set.of("s1"), "loadType", "FICTITIOUS"));

        // apply groovy script with lcc converter station power factor modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .bodyValue("network.getLccConverterStation('v1lcc').powerFactor=1\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v1lcc", Set.of("s1"), "powerFactor", 1.0));

        // apply groovy script with line R modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("network.getLine('line1').r=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("line1", Set.of("s1", "s2"), "r", 2.0));

        // apply groovy script with two windings transformer ratio tap modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("trf1", Set.of("s1"), "ratioTapChanger.tapPosition", 2));

        // apply groovy script with three windings transformer phase tap modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy", TEST_NETWORK_ID)
                .bodyValue("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("trf6", Set.of("s1"), "phaseTapChanger1.tapPosition", 0));

        testDeleteNetwokModifications(TEST_NETWORK_ID, 6);
    }

    private void testDeleteNetwokModifications(UUID networkUuid, int actualSize) {
        webTestClient.get().uri("/v1/networks/modificationgroups")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBody(String.class)
                .isEqualTo(new JSONArray(List.of(networkUuid)).toString());

        // get all modifications for the default group of a network
        assertEquals(actualSize, Objects.requireNonNull(webTestClient.get().uri("/v1/networks/{networkUuid}/modifications", networkUuid)
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .returnResult().getResponseBody()).size());

        // get all elementary modifications for the default group of a network
        assertEquals(actualSize, Objects.requireNonNull(webTestClient.get().uri("/v1/networks/{networkUuid}/elementarymodifications", networkUuid)
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .returnResult().getResponseBody()).size());

        // delete the default modification group of a network
        webTestClient.delete().uri("/v1/networks/{networkUuid}/modifications", networkUuid)
                .exchange()
                .expectStatus().isOk();
        webTestClient.get().uri("/v1/networks/{networkUuid}/modifications", networkUuid)
                .exchange()
                .expectStatus().isNotFound()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, networkUuid.toString()).getMessage());
    }
}
