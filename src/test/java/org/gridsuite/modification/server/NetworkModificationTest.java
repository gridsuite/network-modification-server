/*
  Copyright (c) 2020, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.network.store.client.NetworkStoreService;
import io.zonky.test.db.AutoConfigureEmbeddedDatabase;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
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
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherElementaryModificationInfos.createMatcherElementaryModificationInfos;
import static org.junit.Assert.assertEquals;
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
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).thenReturn(NetworkCreation.create());
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
                .id(TEST_NETWORK_ID)
                .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
                .type(ModificationType.ELEMENTARY)
                .equipmentId("equipmentId")
                .equipmentName("equipmentName")
                .equipmentAttributeName("equipmentAttributeName")
                .equipmentAttributeValue("equipmentAttributeValue")
                .build();
        assertEquals("ElementaryModificationInfos(super=ModificationInfos(id=7928181c-7977-4592-ba19-88027e4254e4, date=2021-02-19T00:00Z, type=ELEMENTARY), equipmentId=equipmentId, equipmentName=equipmentName, equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue)",
                modificationInfos.toString());
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

        // switch opening
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=true", TEST_NETWORK_ID, "v1b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v1b1", "v1b1", "open", "true"));

        // switch closing
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=false", TEST_NETWORK_ID, "v2b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v2b1", "v2b1", "open", "false"));

        // switch closing when already closed
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=false", TEST_NETWORK_ID, "v2b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .isEqualTo(List.of());

        // switch opening on another substation
        webTestClient.put().uri("/v1/networks/{networkUuid}/switches/{switchId}?open=true", TEST_NETWORK_ID, "v3b1")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("v3b1", "v3b1", "open", "true"));
    }

    @Test
    public void testGroovy() {
        // apply null groovy script
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .exchange()
                .expectStatus().isEqualTo(HttpStatus.UNSUPPORTED_MEDIA_TYPE);

        // apply empty groovy script
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
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
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('there is no generator').targetP=12\n")
                .exchange()
                .expectStatus().isBadRequest()
                .expectBody(String.class)
                .isEqualTo(new NetworkModificationException(GROOVY_SCRIPT_ERROR, "Cannot set property 'targetP' on null object").getMessage());

        // apply groovy script with generator target P modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .bodyValue("network.getGenerator('idGenerator').targetP=12\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("idGenerator", "idGenerator", "targetP", "12.0"));

        // apply groovy script with two windings transformer ratio tap modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .bodyValue("network.getTwoWindingsTransformer('trf1').getRatioTapChanger().tapPosition=2\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("trf1", "trf1", "ratioTapChanger.tapPosition", "2"));

        // apply groovy script with three windings transformer phase tap modification
        webTestClient.put().uri("/v1/networks/{networkUuid}/groovy/", TEST_NETWORK_ID)
                .bodyValue("network.getThreeWindingsTransformer('trf6').getLeg1().getPhaseTapChanger().tapPosition=0\n")
                .exchange()
                .expectStatus().isOk()
                .expectHeader().contentType(MediaType.APPLICATION_JSON)
                .expectBodyList(ElementaryModificationInfos.class)
                .value(modifications -> modifications.get(0),
                        createMatcherElementaryModificationInfos("trf6", "trf6", "phaseTapChanger1.tapPosition", "0"));
    }
}
