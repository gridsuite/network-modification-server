/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.NonNull;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.Column;
import javax.persistence.Embedded;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class VoltageLevelModificationEntity extends BasicEquipmentModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "substationId")),
            @AttributeOverride(name = "opType", column = @Column(name = "substationIdOp"))
    })
    private StringModificationEmbedded substationId;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "nominalVoltage")),
            @AttributeOverride(name = "opType", column = @Column(name = "nominalVoltageOp"))
    })
    private DoubleModificationEmbedded nominalVoltage;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "lowVoltageLimit")),
            @AttributeOverride(name = "opType", column = @Column(name = "lowVoltageLimitOp"))
    })
    private DoubleModificationEmbedded lowVoltageLimit;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "highVoltageLimit")),
            @AttributeOverride(name = "opType", column = @Column(name = "highVoltageLimitOp"))
    })
    private DoubleModificationEmbedded highVoltageLimit;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "lowShortCircuitCurrentLimit")),
            @AttributeOverride(name = "opType", column = @Column(name = "lowShortCircuitCurrentLimitOp"))
    })
    private DoubleModificationEmbedded lowShortCircuitCurrentLimit;

    @Embedded
    @AttributeOverrides(value = {
            @AttributeOverride(name = "value", column = @Column(name = "highShortCircuitCurrentLimit")),
            @AttributeOverride(name = "opType", column = @Column(name = "highShortCircuitCurrentLimitOp"))
    })
    private DoubleModificationEmbedded highShortCircuitCurrentLimit;

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VoltageLevelModificationInfos) modificationInfos);
    }

    private void assignAttributes(VoltageLevelModificationInfos voltageLevelModificationInfos) {
        this.substationId = new StringModificationEmbedded(voltageLevelModificationInfos.getSubstationId());
        this.nominalVoltage = new DoubleModificationEmbedded(voltageLevelModificationInfos.getNominalVoltage());
        this.lowVoltageLimit = new DoubleModificationEmbedded(voltageLevelModificationInfos.getLowVoltageLimit());
        this.highVoltageLimit = new DoubleModificationEmbedded(voltageLevelModificationInfos.getHighVoltageLimit());
        this.lowShortCircuitCurrentLimit = new DoubleModificationEmbedded(voltageLevelModificationInfos.getLowShortCircuitCurrentLimit());
        this.highShortCircuitCurrentLimit = new DoubleModificationEmbedded(voltageLevelModificationInfos.getHighShortCircuitCurrentLimit());
    }
}
