/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.Impacts;

import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createCollectionElementImpact;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createCreationImpactType;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createDeletionImpactType;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createModificationImpactType;
import static org.junit.Assert.assertEquals;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.impacts.BaseImpact;
import org.gridsuite.modification.server.impacts.CollectionElementImpact;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;

import lombok.SneakyThrows;
import nl.jqno.equalsverifier.EqualsVerifier;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ElementImpactTest {

    ObjectMapper mapper = new ObjectMapper();

    @Test
    @SneakyThrows
    public void testSimpleElementImpact() {
        EqualsVerifier.simple().forClass(NetworkModificationResult.class).verify();
        EqualsVerifier.simple().forClass(SimpleElementImpact.class).verify();

        SimpleElementImpact creationImpact = createCreationImpactType(IdentifiableType.LINE, "lineId", new TreeSet<>(List.of("s1", "s2")));
        SimpleElementImpact modificationImpact = createModificationImpactType(IdentifiableType.LOAD, "loadId", new TreeSet<>(List.of("s3")));
        SimpleElementImpact deletionImpact = createDeletionImpactType(IdentifiableType.GENERATOR, "generatorId", new TreeSet<>(List.of("s4")));

        Collection<BaseImpact> impacts = List.of(creationImpact, modificationImpact, deletionImpact);

        assertEquals("{\"elementType\":\"LINE\",\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"substationIds\":[\"s1\",\"s2\"]}", mapper.writeValueAsString(creationImpact));
        assertEquals("{\"elementType\":\"LOAD\",\"impactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"substationIds\":[\"s3\"]}", mapper.writeValueAsString(modificationImpact));
        assertEquals("{\"elementType\":\"GENERATOR\",\"impactType\":\"DELETION\",\"elementId\":\"generatorId\",\"substationIds\":[\"s4\"]}", mapper.writeValueAsString(deletionImpact));

        NetworkModificationResult result = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts((List<BaseImpact>) impacts)
            .build();
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-all-ok.json"), mapper.writeValueAsString(result));

        result.setApplicationStatus(ApplicationStatus.WITH_WARNINGS);
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-with-warnings.json"), mapper.writeValueAsString(result));

        result.setApplicationStatus(ApplicationStatus.WITH_ERRORS);
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-with-errors.json"), mapper.writeValueAsString(result));

        assertEquals("[s1, s2, s3, s4]", result.getImpactedSubstationsIds().toString());

        impacts = new HashSet<>(List.of(creationImpact, creationImpact, creationImpact));

        assertEquals("[{\"elementType\":\"LINE\",\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"substationIds\":[\"s1\",\"s2\"]}]", mapper.writeValueAsString(impacts));
    }

    @Test
    @SneakyThrows
    public void testCollectionElementImpact() {
        EqualsVerifier.simple().forClass(NetworkModificationResult.class).verify();
        EqualsVerifier.simple().forClass(CollectionElementImpact.class).verify();

        CollectionElementImpact linesCollectionImpact = createCollectionElementImpact(IdentifiableType.LINE);
        CollectionElementImpact loadsCollectionImpact = createCollectionElementImpact(IdentifiableType.LOAD);
        CollectionElementImpact generatorsCollectionImpact = createCollectionElementImpact(IdentifiableType.GENERATOR);

        assertEquals("{\"elementType\":\"LINE\",\"impactType\":\"COLLECTION\"}", mapper.writeValueAsString(linesCollectionImpact));
        assertEquals("{\"elementType\":\"LOAD\",\"impactType\":\"COLLECTION\"}", mapper.writeValueAsString(loadsCollectionImpact));
        assertEquals("{\"elementType\":\"GENERATOR\",\"impactType\":\"COLLECTION\"}", mapper.writeValueAsString(generatorsCollectionImpact));

        Collection<BaseImpact> impacts = List.of(linesCollectionImpact, loadsCollectionImpact, generatorsCollectionImpact);

        NetworkModificationResult result = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts((List<BaseImpact>) impacts)
            .build();
        assertEquals(TestUtils.resourceToString("/network-modification-result-collection-impacts-with-all-ok.json"), mapper.writeValueAsString(result));

        assertEquals(Set.of(), result.getImpactedSubstationsIds());

        impacts = new HashSet<>(List.of(linesCollectionImpact, loadsCollectionImpact, generatorsCollectionImpact));

        assertEquals("[{\"elementType\":\"GENERATOR\",\"impactType\":\"COLLECTION\"},{\"elementType\":\"LOAD\",\"impactType\":\"COLLECTION\"},{\"elementType\":\"LINE\",\"impactType\":\"COLLECTION\"}]", mapper.writeValueAsString(impacts));
    }
}
