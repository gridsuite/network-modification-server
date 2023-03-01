/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.Impacts;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.SneakyThrows;
import nl.jqno.equalsverifier.EqualsVerifier;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;
import org.junit.Test;

import java.util.*;

import static org.gridsuite.modification.server.utils.ImpactUtils.*;
import static org.junit.Assert.assertEquals;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class ElementImpactTest {

    ObjectMapper mapper = new ObjectMapper();

    @Test
    @SneakyThrows
    public void testElementImpact() {
        EqualsVerifier.simple().forClass(EquipmentInfos.class).verify();

        SimpleElementImpact creationImpact = createCreationImpactType(IdentifiableType.LINE, "lineId", new TreeSet<>(List.of("s1", "s2")));
        SimpleElementImpact modificationImpact = createModificationImpactType(IdentifiableType.LOAD, "loadId", new TreeSet<>(List.of("s3")));
        SimpleElementImpact deletionImpact = createDeletionImpactType(IdentifiableType.GENERATOR, "generatorId", new TreeSet<>(List.of("s4")));

        Collection<SimpleElementImpact> impacts = List.of(creationImpact, modificationImpact, deletionImpact);

        assertEquals("{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]}", mapper.writeValueAsString(creationImpact));
        assertEquals("{\"impactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"elementType\":\"LOAD\",\"substationIds\":[\"s3\"]}", mapper.writeValueAsString(modificationImpact));
        assertEquals("{\"impactType\":\"DELETION\",\"elementId\":\"generatorId\",\"elementType\":\"GENERATOR\",\"substationIds\":[\"s4\"]}", mapper.writeValueAsString(deletionImpact));

        NetworkModificationResult result = NetworkModificationResult.builder()
            .applicationStatus(ApplicationStatus.ALL_OK)
            .networkImpacts((List<SimpleElementImpact>) impacts)
            .build();
        assertEquals(
            "{\"applicationStatus\":\"ALL_OK\",\"networkImpacts\":[{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]},{\"impactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"elementType\":\"LOAD\",\"substationIds\":[\"s3\"]},{\"impactType\":\"DELETION\",\"elementId\":\"generatorId\",\"elementType\":\"GENERATOR\",\"substationIds\":[\"s4\"]}],\"impactedSubstationsIds\":[\"s1\",\"s2\",\"s3\",\"s4\"]}",
            mapper.writeValueAsString(result)
        );

        result.setApplicationStatus(ApplicationStatus.WITH_WARNINGS);
        assertEquals(
            "{\"applicationStatus\":\"WITH_WARNINGS\",\"networkImpacts\":[{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]},{\"impactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"elementType\":\"LOAD\",\"substationIds\":[\"s3\"]},{\"impactType\":\"DELETION\",\"elementId\":\"generatorId\",\"elementType\":\"GENERATOR\",\"substationIds\":[\"s4\"]}],\"impactedSubstationsIds\":[\"s1\",\"s2\",\"s3\",\"s4\"]}",
            mapper.writeValueAsString(result)
        );

        result.setApplicationStatus(ApplicationStatus.WITH_ERRORS);
        assertEquals(
            "{\"applicationStatus\":\"WITH_ERRORS\",\"networkImpacts\":[{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]},{\"impactType\":\"MODIFICATION\",\"elementId\":\"loadId\",\"elementType\":\"LOAD\",\"substationIds\":[\"s3\"]},{\"impactType\":\"DELETION\",\"elementId\":\"generatorId\",\"elementType\":\"GENERATOR\",\"substationIds\":[\"s4\"]}],\"impactedSubstationsIds\":[\"s1\",\"s2\",\"s3\",\"s4\"]}",
            mapper.writeValueAsString(result)
        );

        assertEquals("[s1, s2, s3, s4]", result.getImpactedSubstationsIds().toString());

        impacts = new HashSet<>(List.of(creationImpact, creationImpact, creationImpact));

        assertEquals("[{\"impactType\":\"CREATION\",\"elementId\":\"lineId\",\"elementType\":\"LINE\",\"substationIds\":[\"s1\",\"s2\"]}]", mapper.writeValueAsString(impacts));
    }
}
