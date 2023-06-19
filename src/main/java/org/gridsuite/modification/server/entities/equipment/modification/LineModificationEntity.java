/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;

import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.LineModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;
import javax.persistence.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lineModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lineModification_id_fk_constraint"))
public class LineModificationEntity extends BranchModificationEntity {

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "shuntConductance1")),
        @AttributeOverride(name = "opType", column = @Column(name = "shuntConductance1Op"))
    })
    private DoubleModificationEmbedded shuntConductance1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "shuntSusceptance1")),
        @AttributeOverride(name = "opType", column = @Column(name = "shuntSusceptance1Op"))
    })
    private DoubleModificationEmbedded shuntSusceptance1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "shuntConductance2")),
        @AttributeOverride(name = "opType", column = @Column(name = "shuntConductance2Op"))
    })
    private DoubleModificationEmbedded shuntConductance2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "shuntSusceptance2")),
        @AttributeOverride(name = "opType", column = @Column(name = "shuntSusceptance2Op"))
    })
    private DoubleModificationEmbedded shuntSusceptance2;

    public LineModificationEntity(LineModificationInfos lineModificationInfos) {
        super(lineModificationInfos);
        assignAttributes(lineModificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        LineModificationInfos lineModificationInfos = (LineModificationInfos) modificationInfos;
        assignAttributes(lineModificationInfos);
    }

    private void assignAttributes(LineModificationInfos lineModificationInfos) {
        shuntConductance1 = new DoubleModificationEmbedded(lineModificationInfos.getShuntConductance1());
        shuntSusceptance1 = new DoubleModificationEmbedded(lineModificationInfos.getShuntSusceptance1());
        shuntConductance2 = new DoubleModificationEmbedded(lineModificationInfos.getShuntConductance2());
        shuntSusceptance2 = new DoubleModificationEmbedded(lineModificationInfos.getShuntSusceptance2());
    }

    @Override
    public LineModificationInfos toModificationInfos() {
        return toLineModificationInfosBuilder().build();
    }

    private LineModificationInfos.LineModificationInfosBuilder<?, ?> toLineModificationInfosBuilder() {
        LineModificationInfos.LineModificationInfosBuilder<?, ?> builder = LineModificationInfos
            .builder()
            .uuid(getId())
            .groupUuid(getGroup().getId())
            .date(getDate())
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .seriesResistance(toAttributeModification(getSeriesResistance()))
            .seriesReactance(toAttributeModification(getSeriesReactance()))
            .shuntConductance1(toAttributeModification(getShuntConductance1()))
            .shuntSusceptance1(toAttributeModification(getShuntSusceptance1()))
            .shuntConductance2(toAttributeModification(getShuntConductance2()))
            .shuntSusceptance2(toAttributeModification(getShuntSusceptance2()));

        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }
        return builder;
    }

}
