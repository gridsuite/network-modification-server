/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.Impacts;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.SneakyThrows;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact.SimpleImpactType;
import org.gridsuite.modification.server.utils.MatcherJson;
import org.hamcrest.MatcherAssert;

import java.util.*;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class TestImpactUtils {

    private TestImpactUtils() {
    }

    @SneakyThrows
    public static void testEmptyImpacts(ObjectMapper mapper, String resultAsString) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());
        testEmptyImpacts(mapper, networkModificationResult.get());
    }

    public static void testEmptyImpacts(ObjectMapper mapper, NetworkModificationResult networkModificationResult) {
        testEmptyImpacts(mapper, ApplicationStatus.ALL_OK, ApplicationStatus.ALL_OK, networkModificationResult);
    }

    public static void testEmptyImpactsWithErrors(ObjectMapper mapper, NetworkModificationResult networkModificationResult) {
        testEmptyImpacts(mapper, ApplicationStatus.WITH_ERRORS, ApplicationStatus.WITH_ERRORS, networkModificationResult);
    }

    public static void testEmptyImpactsWithErrorsLastOK(ObjectMapper mapper, NetworkModificationResult networkModificationResult) {
        testEmptyImpacts(mapper, ApplicationStatus.WITH_ERRORS, ApplicationStatus.ALL_OK, networkModificationResult);
    }

    private static void testEmptyImpacts(ObjectMapper mapper, ApplicationStatus globalApplicationStatusExpected, ApplicationStatus localApplicationStatusExpected, NetworkModificationResult networkModificationResult) {
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(globalApplicationStatusExpected)
            .lastGroupApplicationStatus(localApplicationStatusExpected)
            .networkImpacts(List.of())
            .build();

        MatcherAssert.assertThat(networkModificationResult, new MatcherJson<>(mapper, resultExpected));
    }

    @SneakyThrows
    public static void testElementImpacts(ObjectMapper mapper, String resultAsString, int nbImpacts, Set<String> substationIds) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());

        assertEquals(ApplicationStatus.ALL_OK, networkModificationResult.get().getApplicationStatus());
        assertEquals(new TreeSet<>(substationIds), networkModificationResult.get().getImpactedSubstationsIds());
        assertEquals(nbImpacts, networkModificationResult.get().getNetworkImpacts().size());
    }

    @SneakyThrows
    public static void testElementImpacts(ObjectMapper mapper, String resultAsString, List<SimpleElementImpact> elementImpactsExpected) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(elementImpactsExpected)
            .build();
        assertThat(networkModificationResult.get(), new MatcherJson<>(mapper, resultExpected));
    }


    @SneakyThrows
    public static void testElementImpacts(ObjectMapper mapper, NetworkModificationResult networkModificationResult, ApplicationStatus globalApplicationStatus, ApplicationStatus lastGroupApplicationStatus, List<SimpleElementImpact> elementImpacts) {
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(globalApplicationStatus)
            .lastGroupApplicationStatus(lastGroupApplicationStatus)
            .networkImpacts(elementImpacts)
            .build();
        assertThat(networkModificationResult, new MatcherJson<>(mapper, resultExpected));
    }

    public static void testElementCreationImpact(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        testElementImpact(SimpleImpactType.CREATION, mapper, resultAsString, elementType, elementId, substationIds);
    }

    public static void testElementModificationImpact(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        testElementImpact(SimpleImpactType.MODIFICATION, mapper, resultAsString, elementType, elementId, substationIds);
    }

    public static void testElementDeletionImpact(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        testElementImpact(SimpleImpactType.DELETION, mapper, resultAsString, elementType, elementId, substationIds);
    }

    @SneakyThrows
    public static void testElementImpact(SimpleImpactType impactType, ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(List.of(createElementImpact(impactType, elementType, elementId, new TreeSet<>(substationIds))))
            .build();
        assertThat(networkModificationResult.get(), new MatcherJson<>(mapper, resultExpected));
    }

    @SneakyThrows
    public static void testConnectableDeletionImpacts(ObjectMapper mapper, String resultAsString,
                                                      IdentifiableType connectableType, String connectableId,
                                                      String breakerId, String disconnectorId, String substationId) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(createConnectableDeletionImpacts(connectableType, connectableId, breakerId, disconnectorId, substationId))
            .build();
        assertThat(networkModificationResult.get(), new MatcherJson<>(mapper, resultExpected));
    }

    private static List<SimpleElementImpact> createConnectableDeletionImpacts(IdentifiableType connectableType, String connectableId,
                                                                              String breakerId, String disconnectorId, String substationId) {
        return List.of(
            createDeletionImpactType(IdentifiableType.SWITCH, breakerId, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, disconnectorId, Set.of(substationId)),
            createDeletionImpactType(connectableType, connectableId, Set.of(substationId))
        );
    }

    @SneakyThrows
    public static void testBranchCreationImpacts(ObjectMapper mapper, String resultAsString,
                                                 IdentifiableType branchType, String branchId,
                                                 String breakerId1, String disconnectorId1, String substationId1,
                                                 String breakerId2, String disconnectorId2, String substationId2) {
        testBranchImpacts(mapper, SimpleImpactType.CREATION, resultAsString, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2);
    }

    @SneakyThrows
    public static void testBranchCreationImpacts(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        List<SimpleElementImpact> impacts = List.of(
            createElementImpact(SimpleImpactType.CREATION, elementType, elementId, new TreeSet<>(substationIds)),
            createElementImpact(SimpleImpactType.MODIFICATION, elementType, elementId, new TreeSet<>(substationIds)) // case with newCurrentLimits1
        );
        testElementImpacts(mapper, resultAsString, impacts);
    }

    @SneakyThrows
    public static void testBranchDeletionImpacts(ObjectMapper mapper, String resultAsString,
                                                 IdentifiableType branchType, String branchId,
                                                 String breakerId1, String disconnectorId1, String substationId1,
                                                 String breakerId2, String disconnectorId2, String substationId2) {
        testBranchImpacts(mapper, SimpleImpactType.DELETION, resultAsString, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2);
    }

    @SneakyThrows
    public static void testBranchImpacts(ObjectMapper mapper, SimpleImpactType impactType, String resultAsString,
                                         IdentifiableType branchType, String branchId,
                                         String breakerId1, String disconnectorId1, String substationId1,
                                         String breakerId2, String disconnectorId2, String substationId2) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(createBranchImpacts(impactType, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2))
            .build();
        assertThat(networkModificationResult.get(), new MatcherJson<>(mapper, resultExpected));
    }

    private static List<SimpleElementImpact> createBranchImpacts(SimpleImpactType impactType, IdentifiableType branchType, String branchId,
                                                                 String breakerId1, String disconnectorId1, String substationId1,
                                                                 String breakerId2, String disconnectorId2, String substationId2) {
        LinkedList<SimpleElementImpact> impacts = new LinkedList<>(List.of(createElementImpact(impactType, branchType, branchId, new TreeSet<>(List.of(substationId1, substationId2)))));
        List<SimpleElementImpact> switchImpacts = List.of(
            createElementImpact(impactType, IdentifiableType.SWITCH, breakerId1, Set.of(substationId1)),
            createElementImpact(impactType, IdentifiableType.SWITCH, disconnectorId1, Set.of(substationId1)),
            createElementImpact(impactType, IdentifiableType.SWITCH, breakerId2, Set.of(substationId2)),
            createElementImpact(impactType, IdentifiableType.SWITCH, disconnectorId2, Set.of(substationId2))
        );
        if (impactType == SimpleImpactType.DELETION) {
            impacts.addAll(0, switchImpacts);
        } else {
            impacts.addAll(switchImpacts);
        }

        return impacts;
    }

    @SneakyThrows
    public static void test3WTDeletionImpacts(ObjectMapper mapper, String resultAsString, String w3tId,
                                              String breakerId1, String disconnectorId1,
                                              String breakerId2, String disconnectorId2,
                                              String breakerId3, String disconnectorId3,
                                              String substationId) {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() {
        });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(create3wtDeletionImpacts(w3tId, breakerId1, disconnectorId1, breakerId2, disconnectorId2, breakerId3, disconnectorId3, substationId))
            .build();
        assertThat(networkModificationResult.get(), new MatcherJson<>(mapper, resultExpected));
    }

    private static List<SimpleElementImpact> create3wtDeletionImpacts(String w3tId,
                                                                      String breakerId1, String disconnectorId1,
                                                                      String breakerId2, String disconnectorId2,
                                                                      String breakerId3, String disconnectorId3,
                                                                      String substationId) {
        return List.of(
            createDeletionImpactType(IdentifiableType.SWITCH, breakerId1, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, disconnectorId1, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, breakerId2, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, disconnectorId2, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, breakerId3, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, disconnectorId3, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.THREE_WINDINGS_TRANSFORMER, w3tId, Set.of(substationId))
        );
    }

    public static List<SimpleElementImpact> createVoltageLevelDeletionImpacts(String vlId, List<String> busbarSectionsIds,
                                                                              List<Pair<IdentifiableType, String>> connectablesTypesAndIds, String substationId) {
        List<SimpleElementImpact> impacts = new ArrayList<>(List.of(createDeletionImpactType(IdentifiableType.VOLTAGE_LEVEL, vlId, Set.of(substationId))));
        impacts.addAll(busbarSectionsIds.stream().map(id -> createDeletionImpactType(IdentifiableType.BUSBAR_SECTION, id, Set.of(substationId))).collect(Collectors.toList()));
        impacts.addAll(connectablesTypesAndIds.stream().map(typeAndId -> createDeletionImpactType(typeAndId.getLeft(), typeAndId.getRight(), Set.of(substationId))).collect(Collectors.toList()));
        return impacts;
    }

    public static SimpleElementImpact createCreationImpactType(IdentifiableType elementType, String elementId, Set<String> substationIds) {
        return createElementImpact(SimpleImpactType.CREATION, elementType, elementId, substationIds);
    }

    public static SimpleElementImpact createDeletionImpactType(IdentifiableType elementType, String elementId, Set<String> substationIds) {
        return createElementImpact(SimpleImpactType.DELETION, elementType, elementId, substationIds);
    }

    public static SimpleElementImpact createModificationImpactType(IdentifiableType elementType, String elementId, Set<String> substationIds) {
        return createElementImpact(SimpleImpactType.MODIFICATION, elementType, elementId, substationIds);
    }

    private static SimpleElementImpact createElementImpact(SimpleImpactType impactType, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        return SimpleElementImpact.builder()
            .impactType(impactType)
            .elementType(elementType)
            .elementId(elementId)
            .substationIds(substationIds).build();
    }
}
