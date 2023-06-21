/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;

import org.gridsuite.modification.server.TapChangerType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.RatioTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.TapChangerStepCreationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerModificationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.TapChangerStepCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.persistence.*;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "twoWindingsTransformerModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "twoWindingsTransformerModification_id_fk_constraint"))
public class TwoWindingsTransformerModificationEntity extends BranchModificationEntity {

    @Column(name = "magnetizingConductance")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "magnetizingConductance")),
        @AttributeOverride(name = "opType", column = @Column(name = "magnetizingConductanceOp"))
    })
    private DoubleModificationEmbedded magnetizingConductance;

    @Column(name = "magnetizingSusceptance")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "magnetizingSusceptance")),
        @AttributeOverride(name = "opType", column = @Column(name = "magnetizingSusceptanceOp"))
    })
    private DoubleModificationEmbedded magnetizingSusceptance;

    @Column(name = "ratedVoltage1")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedVoltage1")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedVoltage1Op"))
    })
    private DoubleModificationEmbedded ratedVoltage1;

    @Column(name = "ratedVoltage2")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedVoltage2")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedVoltage2Op"))
    })
    private DoubleModificationEmbedded ratedVoltage2;

    @Column(name = "rateds")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "rateds")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedsOp"))
    })
    private DoubleModificationEmbedded ratedS;

    @Column(name = "ratiotapchangerlowtapposition")
    private Integer ratioTapChangerLowTapPosition;

    @Column(name = "ratiotapchangertapposition")
    private Integer ratioTapChangerTapPosition;

    @Column(name = "ratiotapchangerregulating")
    private Boolean ratioTapChangerRegulating;

    @Column(name = "ratiotapchangertargetdeadband")
    private Double ratioTapChangerTargetDeadband;

    @Column(name = "ratiotapchangerterminalrefconnectableid")
    private String ratioTapChangerTerminalRefConnectableId;

    @Column(name = "ratiotapchangerterminalrefvoltagelevelid")
    private String ratioTapChangerTerminalRefVoltageLevelId;

    @Column(name = "ratiotapchangerterminalreftype")
    private String ratioTapChangerTerminalRefType;

    @Column(name = "ratiotapchangerloadtapchangingcapabilities")
    private Boolean ratioTapChangerLoadTapChangingCapabilities;

    @Column(name = "ratiotapchangertargetv")
    private Double ratioTapChangerTargetV;

    @ElementCollection
    @CollectionTable(
            name = "tapChangerStepModification",
            joinColumns = @JoinColumn(name = "modification_id")
    )
    private List<TapChangerStepCreationEmbeddable> tapChangerSteps;

    public TwoWindingsTransformerModificationEntity(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos) {
        super(twoWindingsTransformerModificationInfos);
        assignAttributes(twoWindingsTransformerModificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) modificationInfos;
        assignAttributes(twoWindingsTransformerModificationInfos);
    }

    private void assignAttributes(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos) {
        this.magnetizingConductance = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getMagnetizingConductance());
        this.magnetizingSusceptance = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getMagnetizingSusceptance());
        this.ratedVoltage1 = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getRatedVoltage1());
        this.ratedVoltage2 = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getRatedVoltage2());
        this.ratedS = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getRatedS());
        this.tapChangerSteps = new ArrayList<>();
        assignTapChanger(twoWindingsTransformerModificationInfos);
    }

    private void assignTapChanger(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos) {
        Optional.ofNullable(twoWindingsTransformerModificationInfos.getRatioTapChanger()).ifPresent(this::assignRatioTapChanger);
    }

    private void assignRatioTapChanger(RatioTapChangerCreationInfos ratioTapChanger) {
        this.ratioTapChangerLowTapPosition = ratioTapChanger.getLowTapPosition();
        this.ratioTapChangerTapPosition = ratioTapChanger.getTapPosition();
        this.ratioTapChangerRegulating = ratioTapChanger.isRegulating();
        this.ratioTapChangerTargetDeadband = ratioTapChanger.getTargetDeadband();
        this.ratioTapChangerTerminalRefConnectableId = ratioTapChanger.getRegulatingTerminalId();
        this.ratioTapChangerTerminalRefVoltageLevelId = ratioTapChanger.getRegulatingTerminalVlId();
        this.ratioTapChangerTerminalRefType = ratioTapChanger.getRegulatingTerminalType();
        this.ratioTapChangerLoadTapChangingCapabilities = ratioTapChanger.getLoadTapChangingCapabilities();
        this.ratioTapChangerTargetV = ratioTapChanger.getTargetV();
        this.tapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddableRatioTapChangerSteps(ratioTapChanger.getSteps()));
    }

    @Override
    public TwoWindingsTransformerModificationInfos toModificationInfos() {
        return toTwoWindingsTransformerModificationInfosBuilder().build();
    }

    private TwoWindingsTransformerModificationInfos.TwoWindingsTransformerModificationInfosBuilder<?, ?> toTwoWindingsTransformerModificationInfosBuilder() {

        List<TapChangerStepCreationEmbeddable> ratioTapChangerSteps = new ArrayList<>();
        if (getTapChangerSteps() != null && getTapChangerSteps().size() > 0) {
            ratioTapChangerSteps = getTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.RATIO)).sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.toList());
        }

        TwoWindingsTransformerModificationInfos.TwoWindingsTransformerModificationInfosBuilder<?, ?> builder = TwoWindingsTransformerModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .seriesResistance(AttributeModification.toAttributeModification(getSeriesResistance()))
                .seriesReactance(AttributeModification.toAttributeModification(getSeriesReactance()))
                .magnetizingConductance(AttributeModification.toAttributeModification(getMagnetizingConductance()))
                .magnetizingSusceptance(AttributeModification.toAttributeModification(getMagnetizingSusceptance()))
                .ratedVoltage1(AttributeModification.toAttributeModification(getRatedVoltage1()))
                .ratedVoltage2(AttributeModification.toAttributeModification(getRatedVoltage2()))
                .ratedS(AttributeModification.toAttributeModification(getRatedS()));

        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }

        if (!ratioTapChangerSteps.isEmpty()) {
            List<TapChangerStepCreationInfos> ratioTapChangerStepCreationInfos = ratioTapChangerSteps.stream().map(TapChangerStepCreationEmbeddable::toModificationInfos).collect(Collectors.toList());
            builder.ratioTapChanger(RatioTapChangerCreationInfos.builder()
                    .lowTapPosition(getRatioTapChangerLowTapPosition())
                    .tapPosition(getRatioTapChangerTapPosition())
                    .targetDeadband(getRatioTapChangerTargetDeadband())
                    .regulating(getRatioTapChangerRegulating())
                    .loadTapChangingCapabilities(getRatioTapChangerLoadTapChangingCapabilities())
                    .targetV(getRatioTapChangerTargetV())
                    .regulatingTerminalId(getRatioTapChangerTerminalRefConnectableId())
                    .regulatingTerminalVlId(getRatioTapChangerTerminalRefVoltageLevelId())
                    .regulatingTerminalType(getRatioTapChangerTerminalRefType())
                    .steps(ratioTapChangerStepCreationInfos)
                    .build());
        }

        return builder;
    }
}
