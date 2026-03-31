/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;

import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LineModificationInfos;
import org.gridsuite.modification.dto.LineSegmentInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.LineSegmentEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;
import jakarta.persistence.*;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

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

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinTable(
        joinColumns = @JoinColumn(name = "line_id"), foreignKey = @ForeignKey(name = "line_modification_id_fk"),
        inverseJoinColumns = @JoinColumn(name = "line_segments_id"), inverseForeignKey = @ForeignKey(name = "line_segments_id_fk"),
        uniqueConstraints = @UniqueConstraint(name = "line_modification_line_segments_uk", columnNames = {"line_segments_id"}))
    private List<LineSegmentEntity> lineSegments;

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
        lineSegments = assignLineSegments(lineModificationInfos.getLineSegments());
    }

    private List<LineSegmentEntity> assignLineSegments(List<LineSegmentInfos> lineSegmentInfos) {
        List<LineSegmentEntity> updatedLineSegments = lineSegments;

        if (updatedLineSegments == null) {
            updatedLineSegments = new ArrayList<>();
        } else {
            updatedLineSegments.clear();
        }
        updatedLineSegments.addAll(LineSegmentEntity.toLineSegmentEntities(lineSegmentInfos));
        return updatedLineSegments;
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
            .description(getDescription())
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .voltageLevelId1(toAttributeModification(getVoltageLevelId1()))
            .voltageLevelId2(toAttributeModification(getVoltageLevelId2()))
            .busOrBusbarSectionId1(toAttributeModification(getBusOrBusbarSectionId1()))
            .busOrBusbarSectionId2(toAttributeModification(getBusOrBusbarSectionId2()))
            .connectionName1(toAttributeModification(getConnectionName1()))
            .connectionName2(toAttributeModification(getConnectionName2()))
            .selectedOperationalLimitsGroupId1(toAttributeModification(getSelectedOperationalLimitsGroupId1()))
            .selectedOperationalLimitsGroupId2(toAttributeModification(getSelectedOperationalLimitsGroupId2()))
            .enableOLGModification(getEnableOLGModification())
            .operationalLimitsGroupsModificationType(getOperationalLimitsGroupsModificationType())
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
            .p1MeasurementValue(toAttributeModification(getP1MeasurementValue()))
            .p1MeasurementValidity(toAttributeModification(getP1MeasurementValidity()))
            .q1MeasurementValue(toAttributeModification(getQ1MeasurementValue()))
            .q1MeasurementValidity(toAttributeModification(getQ1MeasurementValidity()))
            .p2MeasurementValue(toAttributeModification(getP2MeasurementValue()))
            .p2MeasurementValidity(toAttributeModification(getP2MeasurementValidity()))
            .q2MeasurementValue(toAttributeModification(getQ2MeasurementValue()))
            .q2MeasurementValidity(toAttributeModification(getQ2MeasurementValidity()))
            .lineSegments(LineSegmentEntity.fromLineSegmentsEntity(getLineSegments()))
             // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
        if (getOperationalLimitsGroups() != null) {
            builder.operationalLimitsGroups(OperationalLimitsGroupModificationEntity.fromOperationalLimitsGroupsEntities(getOperationalLimitsGroups()));
        }
        return builder;
    }

}
