/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;
import org.gridsuite.modification.server.impacts.SimpleElementImpact;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Builder
@Data
@Schema(description = "Network modification result")
public class NetworkModificationResult {
    public enum ApplicationStatus {
        ALL_OK(0),
        WITH_WARNINGS(1),
        WITH_ERRORS(2);

        private final int severityLevel;

        ApplicationStatus(int severityLevel) {
            this.severityLevel = severityLevel;
        }

        public ApplicationStatus max(ApplicationStatus other) {
            return severityLevel >= other.severityLevel ? this : other;
        }
    }

    @Schema(description = "Operation application status")
    ApplicationStatus applicationStatus;

    @Schema(description = "Map of modification groups and their corresponding application status")
    Map<UUID, ApplicationStatus> modificationsGroupApplicationStatus;

    @Schema(description = "Network modification impacts")
    @Builder.Default
    private List<SimpleElementImpact> networkImpacts = List.of();

    public Set<String> getImpactedSubstationsIds() {
        return networkImpacts.stream().flatMap(impact -> impact.getSubstationIds().stream()).collect(Collectors.toCollection(TreeSet::new));
    }
}
