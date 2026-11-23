/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Embeddable
public class RegulatingTerminalModificationEmbeddable {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationType")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationTypeOp"))
    })
    private EnumModificationEmbedded<VoltageRegulationType> voltageRegulationType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalIdOp"))
    })
    private StringModificationEmbedded regulatingTerminalId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalType")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalTypeOp"))
    })
    private StringModificationEmbedded regulatingTerminalType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalVlId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalVlIdOp"))
    })
    private StringModificationEmbedded regulatingTerminalVlId;

    public RegulatingTerminalModificationEmbeddable(AttributeModification<VoltageRegulationType> voltageRegulationType,
                                                    AttributeModification<String> regulatingTerminalId,
                                                    AttributeModification<String> regulatingTerminalType,
                                                    AttributeModification<String> regulatingTerminalVlId) {
        this.voltageRegulationType = voltageRegulationType != null ? new EnumModificationEmbedded<>(voltageRegulationType) : null;
        this.regulatingTerminalId = regulatingTerminalId != null ? new StringModificationEmbedded(regulatingTerminalId) : null;
        this.regulatingTerminalType = regulatingTerminalType != null ? new StringModificationEmbedded(regulatingTerminalType) : null;
        this.regulatingTerminalVlId = regulatingTerminalVlId != null ? new StringModificationEmbedded(regulatingTerminalVlId) : null;
    }

    public static RegulatingTerminalModificationEmbeddable orEmpty(RegulatingTerminalModificationEmbeddable regulatingTerminal) {
        return regulatingTerminal != null ? regulatingTerminal : new RegulatingTerminalModificationEmbeddable();
    }
}
