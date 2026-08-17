/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.modification.dto.ModificationInfos;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author Maissa Souissi <maissa.souissi at rte-france.com>
 */
public record NetworkModificationExportInfos(
        @JsonProperty("modifications")
        List<ModificationInfos> exportedModifications,
        @JsonProperty("filters")
        Map<UUID, List<AbstractFilter>> exportedFilters,
        @JsonProperty("loadFlowParameters")
        Map<UUID, UUID> exportedLoadFlowParameters
) { }
