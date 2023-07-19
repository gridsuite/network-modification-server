/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.Impacts;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.Test;
import org.junit.jupiter.api.Tag;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

import static org.gridsuite.modification.server.Impacts.TestImpactUtils.*;
import static org.junit.Assert.assertEquals;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Tag("UnitTest")
public class ElementImpactTest {
    private ObjectMapper mapper = new ObjectMapper();

    @Test
    public void testElementImpact() throws IOException {
        SimpleElementImpact creationImpact = createCreationImpactType(IdentifiableType.LINE, "lineId", new TreeSet<>(List.of("s1", "s2")));
        SimpleElementImpact modificationImpact = createModificationImpactType(IdentifiableType.LOAD, "loadId", new TreeSet<>(List.of("s3")));
        SimpleElementImpact deletionImpact = createDeletionImpactType(IdentifiableType.GENERATOR, "generatorId", new TreeSet<>(List.of("s4")));

        Collection<SimpleElementImpact> impacts = List.of(creationImpact, modificationImpact, deletionImpact);

        assertEquals("{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]}", mapper.writeValueAsString(creationImpact));
        assertEquals("{\"impactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"elementType\":\"LOAD\",\"substationIds\":[\"s3\"]}", mapper.writeValueAsString(modificationImpact));
        assertEquals("{\"impactType\":\"DELETION\",\"elementId\":\"generatorId\",\"elementType\":\"GENERATOR\",\"substationIds\":[\"s4\"]}", mapper.writeValueAsString(deletionImpact));

        NetworkModificationResult result = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .lastGroupApplicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts((List<SimpleElementImpact>) impacts)
            .build();
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-all-ok.json"), mapper.writeValueAsString(result));

        result.setApplicationStatus(ApplicationStatus.WITH_WARNINGS);
        result.setLastGroupApplicationStatus(ApplicationStatus.WITH_WARNINGS);
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-with-warnings.json"), mapper.writeValueAsString(result));

        result.setApplicationStatus(ApplicationStatus.WITH_ERRORS);
        result.setLastGroupApplicationStatus(ApplicationStatus.WITH_ERRORS);
        assertEquals(TestUtils.resourceToString("/network-modification-result-with-with-errors.json"), mapper.writeValueAsString(result));

        assertEquals("[s1, s2, s3, s4]", result.getImpactedSubstationsIds().toString());

        impacts = new HashSet<>(List.of(creationImpact, creationImpact, creationImpact));

        assertEquals("[{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]}]", mapper.writeValueAsString(impacts));
    }
}
