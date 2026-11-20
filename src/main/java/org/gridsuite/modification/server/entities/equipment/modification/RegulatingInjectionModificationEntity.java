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
import org.gridsuite.modification.dto.InjectionModificationInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class RegulatingInjectionModificationEntity extends InjectionModificationEntity {
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

    protected RegulatingInjectionModificationEntity(InjectionModificationInfos modificationInfos) {
        super(modificationInfos);
    }

    protected void assignRegulatingAttributes(AttributeModification<VoltageRegulationType> voltageRegulationTypeInfos,
                                              AttributeModification<String> regulatingTerminalIdInfos,
                                              AttributeModification<String> regulatingTerminalTypeInfos,
                                              AttributeModification<String> regulatingTerminalVlIdInfos) {
        this.voltageRegulationType = voltageRegulationTypeInfos != null ? new EnumModificationEmbedded<>(voltageRegulationTypeInfos) : null;
        this.regulatingTerminalId = regulatingTerminalIdInfos != null ? new StringModificationEmbedded(regulatingTerminalIdInfos) : null;
        this.regulatingTerminalType = regulatingTerminalTypeInfos != null ? new StringModificationEmbedded(regulatingTerminalTypeInfos) : null;
        this.regulatingTerminalVlId = regulatingTerminalVlIdInfos != null ? new StringModificationEmbedded(regulatingTerminalVlIdInfos) : null;
    }
}
