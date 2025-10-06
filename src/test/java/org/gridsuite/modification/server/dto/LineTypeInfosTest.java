/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.server.dto.catalog.LineTypeInfos;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * @author Achour Berrahma <achour.berrahma at rte-france.com>
 */
class LineTypeInfosTest {

    @Test
    @DisplayName("Should throw UnsupportedOperationException when toEntity is called on base class")
    void testToEntityOnBaseClass() {
        LineTypeInfos lineTypeInfos = LineTypeInfos.builder()
                .id(UUID.randomUUID())
                .type("Generic Type")
                .voltage(220)
                .build();
        assertThatThrownBy(lineTypeInfos::toEntity)
                .isInstanceOf(UnsupportedOperationException.class)
                .hasMessage("toEntity() should be implemented in subclasses");
    }
}
