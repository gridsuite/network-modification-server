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
import jakarta.persistence.*;
import org.springframework.util.CollectionUtils;

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
        @AttributeOverride(name = "value", column = @Column(name = "g1")),
        @AttributeOverride(name = "opType", column = @Column(name = "g1Op"))
    })
    private DoubleModificationEmbedded g1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "b1")),
        @AttributeOverride(name = "opType", column = @Column(name = "b1Op"))
    })
    private DoubleModificationEmbedded b1;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "g2")),
        @AttributeOverride(name = "opType", column = @Column(name = "g2Op"))
    })
    private DoubleModificationEmbedded g2;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "b2")),
        @AttributeOverride(name = "opType", column = @Column(name = "b2Op"))
    })
    private DoubleModificationEmbedded b2;

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
        g1 = new DoubleModificationEmbedded(lineModificationInfos.getG1());
        b1 = new DoubleModificationEmbedded(lineModificationInfos.getB1());
        g2 = new DoubleModificationEmbedded(lineModificationInfos.getG2());
        b2 = new DoubleModificationEmbedded(lineModificationInfos.getB2());
    }

    @Override
    public LineModificationInfos toModificationInfos() {
        return toLineModificationInfosBuilder().build();
    }

    private LineModificationInfos.LineModificationInfosBuilder<?, ?> toLineModificationInfosBuilder() {
        LineModificationInfos.LineModificationInfosBuilder<?, ?> builder = LineModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .voltageLevelId1(toAttributeModification(getVoltageLevelId1()))
            .voltageLevelId2(toAttributeModification(getVoltageLevelId2()))
            .busOrBusbarSectionId1(toAttributeModification(getBusOrBusbarSectionId1()))
            .busOrBusbarSectionId2(toAttributeModification(getBusOrBusbarSectionId2()))
            .connectionName1(toAttributeModification(getConnectionName1()))
            .connectionName2(toAttributeModification(getConnectionName2()))
            .connectionDirection1(toAttributeModification(getConnectionDirection1()))
            .connectionDirection2(toAttributeModification(getConnectionDirection2()))
            .connectionPosition1(toAttributeModification(getConnectionPosition1()))
            .connectionPosition2(toAttributeModification(getConnectionPosition2()))
            .terminal1Connected(toAttributeModification(getTerminal1Connected()))
            .terminal2Connected(toAttributeModification(getTerminal2Connected()))
            .r(toAttributeModification(getR()))
            .x(toAttributeModification(getX()))
            .g1(toAttributeModification(getG1()))
            .b1(toAttributeModification(getB1()))
            .g2(toAttributeModification(getG2()))
            .b2(toAttributeModification(getB2()))
             // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }
        return builder;
    }

}
