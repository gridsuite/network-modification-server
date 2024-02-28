/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.Impacts;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact.SimpleImpactType;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.assertions.Assertions.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class TestImpactUtils {
    private TestImpactUtils() {
    }

    public static void testEmptyImpacts(ObjectMapper mapper, String resultAsString) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        testEmptyImpacts(networkModificationResult.get());
    }

    public static void testEmptyImpacts(NetworkModificationResult networkModificationResult) {
        testEmptyImpacts(ApplicationStatus.ALL_OK, ApplicationStatus.ALL_OK, networkModificationResult);
    }

    public static void testEmptyImpactsWithErrors(NetworkModificationResult networkModificationResult) {
        testEmptyImpacts(ApplicationStatus.WITH_ERRORS, ApplicationStatus.WITH_ERRORS, networkModificationResult);
    }

    public static void testEmptyImpactsWithErrorsLastOK(NetworkModificationResult networkModificationResult) {
        testEmptyImpacts(ApplicationStatus.WITH_ERRORS, ApplicationStatus.ALL_OK, networkModificationResult);
    }

    private static void testEmptyImpacts(ApplicationStatus globalApplicationStatusExpected, ApplicationStatus localApplicationStatusExpected, NetworkModificationResult networkModificationResult) {
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(globalApplicationStatusExpected)
            .lastGroupApplicationStatus(localApplicationStatusExpected)
            .networkImpacts(List.of())
            .build();
        assertThat(networkModificationResult).recursivelyEquals(resultExpected);
    }

    public static void testElementImpacts(ObjectMapper mapper, String resultAsString, int nbImpacts, Set<IdentifiableType> collectionImpactElementTypes, Set<String> substationIds) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());

        assertEquals(ApplicationStatus.ALL_OK, networkModificationResult.get().getApplicationStatus());
        assertEquals(new TreeSet<>(substationIds), networkModificationResult.get().getImpactedSubstationsIds());
        assertEquals(nbImpacts, networkModificationResult.get().getNetworkImpacts().size());
        assertThat(networkModificationResult.get().getNetworkImpacts()).containsAll(collectionImpactElementTypes.stream().map(TestImpactUtils::createCollectionElementImpact).toList());
    }

    public static void testElementImpacts(ObjectMapper mapper, String resultAsString, List<AbstractBaseImpact> elementImpactsExpected) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(elementImpactsExpected)
            .build();
        assertThat(networkModificationResult.get()).recursivelyEquals(resultExpected);
    }

    public static void testElementCreationImpact(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) throws JsonProcessingException {
        testElementImpact(SimpleImpactType.CREATION, mapper, resultAsString, elementType, elementId, substationIds);
    }

    public static void testElementModificationImpact(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) throws JsonProcessingException {
        testElementImpact(SimpleImpactType.MODIFICATION, mapper, resultAsString, elementType, elementId, substationIds);
    }

    public static void testElementImpact(SimpleImpactType type, ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(createSubstationImpacts(substationIds))
            .build();
        assertThat(networkModificationResult.get()).recursivelyEquals(resultExpected);
    }

    public static void testConnectableDeletionImpacts(ObjectMapper mapper, String resultAsString,
                                                      IdentifiableType connectableType, String connectableId,
                                                      String breakerId, String disconnectorId, String substationId) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(createConnectableDeletionImpacts(connectableType, connectableId, breakerId, disconnectorId, substationId))
            .build();
        assertThat(networkModificationResult.get()).recursivelyEquals(resultExpected);
    }

    private static List<AbstractBaseImpact> createConnectableDeletionImpacts(IdentifiableType connectableType, String connectableId,
                                                                              String breakerId, String disconnectorId, String substationId) {
        return List.of(
            createDeletionImpactType(IdentifiableType.SWITCH, breakerId, Set.of(substationId)),
            createDeletionImpactType(IdentifiableType.SWITCH, disconnectorId, Set.of(substationId)),
            createDeletionImpactType(connectableType, connectableId, Set.of(substationId))
        );
    }

    public static void testBranchCreationImpacts(ObjectMapper mapper, String resultAsString,
                                                 IdentifiableType branchType, String branchId,
                                                 String breakerId1, String disconnectorId1, String substationId1,
                                                 String breakerId2, String disconnectorId2, String substationId2) throws JsonProcessingException {
        testBranchImpacts(mapper, SimpleImpactType.CREATION, resultAsString, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2);
    }

    public static void testBranchCreationImpacts(ObjectMapper mapper, String resultAsString, IdentifiableType elementType, String elementId, Set<String> substationIds) throws JsonProcessingException {
        List<AbstractBaseImpact> substationsImpacts = createSubstationImpacts(substationIds);
        testElementImpacts(mapper, resultAsString, substationsImpacts);
    }

    public static void testBranchDeletionImpacts(ObjectMapper mapper, String resultAsString,
                                                 IdentifiableType branchType, String branchId,
                                                 String breakerId1, String disconnectorId1, String substationId1,
                                                 String breakerId2, String disconnectorId2, String substationId2) throws JsonProcessingException {
        testBranchImpacts(mapper, SimpleImpactType.DELETION, resultAsString, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2);
    }

    public static void testBranchImpacts(ObjectMapper mapper, SimpleImpactType type, String resultAsString,
                                         IdentifiableType branchType, String branchId,
                                         String breakerId1, String disconnectorId1, String substationId1,
                                         String breakerId2, String disconnectorId2, String substationId2) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(createBranchImpacts(type, branchType, branchId, breakerId1, disconnectorId1, substationId1, breakerId2, disconnectorId2, substationId2))
            .build();
        assertThat(networkModificationResult.get()).recursivelyEquals(resultExpected);
    }

    private static List<AbstractBaseImpact> createBranchImpacts(SimpleImpactType type, IdentifiableType branchType, String branchId,
                                                                 String breakerId1, String disconnectorId1, String substationId1,
                                                                 String breakerId2, String disconnectorId2, String substationId2) {
        List<AbstractBaseImpact> impacts = new ArrayList<>();
        if (type == SimpleImpactType.DELETION) {
            List<SimpleElementImpact> switchImpacts = List.of(
                createElementImpact(SimpleImpactType.DELETION, branchType, branchId, new HashSet<>(List.of(substationId1, substationId2))),
                createElementImpact(SimpleImpactType.DELETION, IdentifiableType.SWITCH, breakerId1, Set.of(substationId1)),
                createElementImpact(SimpleImpactType.DELETION, IdentifiableType.SWITCH, disconnectorId1, Set.of(substationId1)),
                createElementImpact(SimpleImpactType.DELETION, IdentifiableType.SWITCH, breakerId2, Set.of(substationId2)),
                createElementImpact(SimpleImpactType.DELETION, IdentifiableType.SWITCH, disconnectorId2, Set.of(substationId2))
            );
            impacts.addAll(0, switchImpacts);
        } else {
            impacts.addAll(createSubstationImpacts(Set.copyOf(List.of(substationId1, substationId2))));
        }
        return impacts;
    }

    public static void test3WTDeletionImpacts(ObjectMapper mapper, String resultAsString, String w3tId,
                                              String breakerId1, String disconnectorId1,
                                              String breakerId2, String disconnectorId2,
                                              String breakerId3, String disconnectorId3,
                                              String substationId) throws JsonProcessingException {
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        NetworkModificationResult resultExpected = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(create3wtDeletionImpacts(w3tId, breakerId1, disconnectorId1, breakerId2, disconnectorId2, breakerId3, disconnectorId3, substationId))
            .build();
        assertThat(networkModificationResult.get()).recursivelyEquals(resultExpected);
    }

    private static List<AbstractBaseImpact> create3wtDeletionImpacts(String w3tId,
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

    public static List<AbstractBaseImpact> createMultipleDeletionImpacts(List<Pair<IdentifiableType, String>> deletedIdentifiables, Set<String> impactedSubstationIds) {
        return new ArrayList<>(deletedIdentifiables.stream().map(identifiable -> createDeletionImpactType(identifiable.getLeft(), identifiable.getRight(), impactedSubstationIds)).toList());
    }

    public static List<AbstractBaseImpact> createSubstationImpacts(Set<String> substationIds) {
        return substationIds.stream().map(id -> createElementImpact(SimpleImpactType.MODIFICATION, IdentifiableType.SUBSTATION, id, Set.of(id)))
                                    .collect(Collectors.toList());
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

    private static SimpleElementImpact createElementImpact(SimpleImpactType type, IdentifiableType elementType, String elementId, Set<String> substationIds) {
        return SimpleElementImpact.builder()
            .simpleImpactType(type)
            .elementType(elementType)
            .elementId(elementId)
            .substationIds(substationIds).build();
    }

    public static CollectionElementImpact createCollectionElementImpact(IdentifiableType elementType) {
        return CollectionElementImpact.builder()
            .elementType(elementType)
            .build();
    }
}
