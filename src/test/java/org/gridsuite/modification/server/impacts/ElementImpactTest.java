/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.impacts;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.gridsuite.modification.server.impacts.TestImpactUtils.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Tag("UnitTest")
class ElementImpactTest {
    private final ObjectMapper mapper = new ObjectMapper();

    @Test
    void testElementImpact() throws Exception {
        SimpleElementImpact creationImpact = createCreationImpactType(IdentifiableType.LINE, "lineId", new TreeSet<>(List.of("s1", "s2")));
        SimpleElementImpact modificationImpact = createModificationImpactType(IdentifiableType.LOAD, "loadId", new TreeSet<>(List.of("s3")));
        SimpleElementImpact deletionImpact = createDeletionImpactType(IdentifiableType.GENERATOR, "generatorId", new TreeSet<>(List.of("s4")));

        assertTrue(creationImpact.isSimple());
        assertTrue(creationImpact.isCreation());
        assertTrue(modificationImpact.isModification());
        assertTrue(deletionImpact.isDeletion());

        List<AbstractBaseImpact> impacts = List.of(creationImpact, modificationImpact, deletionImpact);

        assertEquals("{\"type\":\"SIMPLE\",\"elementType\":\"LINE\",\"simpleImpactType\":\"CREATION\",\"elementId\":\"lineId\",\"substationIds\":[\"s1\",\"s2\"]}", mapper.writeValueAsString(creationImpact));
        assertEquals("{\"type\":\"SIMPLE\",\"elementType\":\"LOAD\",\"simpleImpactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"substationIds\":[\"s3\"]}", mapper.writeValueAsString(modificationImpact));
        assertEquals("{\"type\":\"SIMPLE\",\"elementType\":\"GENERATOR\",\"simpleImpactType\":\"DELETION\",\"elementId\":\"generatorId\",\"substationIds\":[\"s4\"]}", mapper.writeValueAsString(deletionImpact));

        NetworkModificationResult result = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(impacts)
            .build();
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-all-ok.json"), mapper.writeValueAsString(result));

        result.setApplicationStatus(ApplicationStatus.WITH_WARNINGS);
        result.setLastGroupApplicationStatus(ApplicationStatus.WITH_WARNINGS);
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-with-warnings.json"), mapper.writeValueAsString(result));

        result.setApplicationStatus(ApplicationStatus.WITH_ERRORS);
        result.setLastGroupApplicationStatus(ApplicationStatus.WITH_ERRORS);
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-with-errors.json"), mapper.writeValueAsString(result));

        assertEquals("[s1, s2, s3, s4]", result.getImpactedSubstationsIds().toString());

        Set<AbstractBaseImpact> impactsSet = Set.copyOf(List.of(creationImpact, creationImpact, creationImpact));

        assertEquals("[{\"type\":\"SIMPLE\",\"elementType\":\"LINE\",\"simpleImpactType\":\"CREATION\",\"elementId\":\"lineId\",\"substationIds\":[\"s1\",\"s2\"]}]", mapper.writeValueAsString(impactsSet));
    }

    @Test
    void testCollectionElementImpact() throws Exception {
        CollectionElementImpact linesCollectionImpact = createCollectionElementImpact(IdentifiableType.LINE);
        CollectionElementImpact loadsCollectionImpact = createCollectionElementImpact(IdentifiableType.LOAD);
        CollectionElementImpact generatorsCollectionImpact = createCollectionElementImpact(IdentifiableType.GENERATOR);

        assertTrue(linesCollectionImpact.isCollection());

        assertEquals("{\"type\":\"COLLECTION\",\"elementType\":\"LINE\"}", mapper.writeValueAsString(linesCollectionImpact));
        assertEquals("{\"type\":\"COLLECTION\",\"elementType\":\"LOAD\"}", mapper.writeValueAsString(loadsCollectionImpact));
        assertEquals("{\"type\":\"COLLECTION\",\"elementType\":\"GENERATOR\"}", mapper.writeValueAsString(generatorsCollectionImpact));

        List<AbstractBaseImpact> impacts = List.of(linesCollectionImpact, loadsCollectionImpact, generatorsCollectionImpact);

        NetworkModificationResult result = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts(impacts)
            .build();
        assertEquals(TestUtils.resourceToString("/network-modification-result-collection-impacts-with-all-ok.json"), mapper.writeValueAsString(result));

        assertEquals(Set.of(), result.getImpactedSubstationsIds());

        Set<AbstractBaseImpact> impactsSet = Set.copyOf(List.of(linesCollectionImpact, linesCollectionImpact, linesCollectionImpact));

        assertEquals("[{\"type\":\"COLLECTION\",\"elementType\":\"LINE\"}]", mapper.writeValueAsString(impactsSet));
    }
}
