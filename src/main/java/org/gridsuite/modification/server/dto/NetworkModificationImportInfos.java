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
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public record NetworkModificationImportInfos(
        @JsonProperty("modifications")
        List<ModificationInfos> modifications,
        @JsonProperty("filtersByOldId")
        Map<UUID, AbstractFilter> filtersByOldId,
        @JsonProperty("loadFlowParametersIdMapping")
        Map<UUID, UUID> loadFlowParametersIdMapping
) { }
