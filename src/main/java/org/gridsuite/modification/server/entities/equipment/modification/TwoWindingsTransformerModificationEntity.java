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
import org.gridsuite.modification.server.dto.RatioTapChangerModificationInfos;
import org.gridsuite.modification.server.dto.TapChangerStepCreationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerModificationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.TapChangerStepCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

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

    @Column(name = "ratioTapChangerEnabled")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratioTapChangerEnabled")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratioTapChangerEnabledOp"))
    })
    private BooleanModificationEmbedded ratioTapChangerEnabled;

    @Column(name = "ratiotapchangerlowtapposition")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerlowtapposition")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerlowtappositionOp"))
    })
    private IntegerModificationEmbedded ratioTapChangerLowTapPosition;

    @Column(name = "ratiotapchangertapposition")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangertapposition")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangertappositionOp"))
    })
    private IntegerModificationEmbedded ratioTapChangerTapPosition;

    @Column(name = "ratiotapchangerregulating")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerregulating")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerregulatingOp"))
    })
    private BooleanModificationEmbedded ratioTapChangerRegulating;

    @Column(name = "ratiotapchangertargetdeadband")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangertargetdeadband")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangertargetdeadbandOp"))
    })
    private DoubleModificationEmbedded ratioTapChangerTargetDeadband;

    @Column(name = "ratiotapchangerterminalrefconnectableid")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerterminalrefconnectableid")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerterminalrefconnectableidOp"))
    })
    private StringModificationEmbedded ratioTapChangerTerminalRefConnectableId;

    @Column(name = "ratiotapchangerterminalrefvoltagelevelid")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerterminalrefvoltagelevelid")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerterminalrefvoltagelevelidOp"))
    })
    private StringModificationEmbedded ratioTapChangerTerminalRefVoltageLevelId;

    @Column(name = "ratiotapchangerterminalreftype")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerterminalreftype")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerterminalreftypeOp"))
    })
    private StringModificationEmbedded ratioTapChangerTerminalRefType;

    @Column(name = "ratiotapchangerloadtapchangingcapabilities")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerloadtapchangingcapabilities")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerloadtapchangingcapabilitiesOp"))
    })
    private BooleanModificationEmbedded ratioTapChangerLoadTapChangingCapabilities;

    @Column(name = "ratiotapchangertargetv")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangertargetv")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangertargetvOp"))
    })
    private DoubleModificationEmbedded ratioTapChangerTargetV;

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

    private void assignRatioTapChanger(RatioTapChangerModificationInfos ratioTapChanger) {
        this.ratioTapChangerEnabled = new BooleanModificationEmbedded(ratioTapChanger.getEnabled());
        this.ratioTapChangerLowTapPosition = new IntegerModificationEmbedded(ratioTapChanger.getLowTapPosition());
        this.ratioTapChangerTapPosition = new IntegerModificationEmbedded(ratioTapChanger.getTapPosition());
        this.ratioTapChangerRegulating = new BooleanModificationEmbedded(ratioTapChanger.getRegulating());
        this.ratioTapChangerTargetDeadband = new DoubleModificationEmbedded(ratioTapChanger.getTargetDeadband());
        this.ratioTapChangerTerminalRefConnectableId = new StringModificationEmbedded(ratioTapChanger.getRegulatingTerminalId());
        this.ratioTapChangerTerminalRefVoltageLevelId = new StringModificationEmbedded(ratioTapChanger.getRegulatingTerminalVlId());
        this.ratioTapChangerTerminalRefType = new StringModificationEmbedded(ratioTapChanger.getRegulatingTerminalType());
        this.ratioTapChangerLoadTapChangingCapabilities = new BooleanModificationEmbedded(ratioTapChanger.getLoadTapChangingCapabilities());
        this.ratioTapChangerTargetV = new DoubleModificationEmbedded(ratioTapChanger.getTargetV());
        if (ratioTapChanger.getSteps() != null){
            this.tapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddableRatioTapChangerSteps(ratioTapChanger.getSteps()));
        }
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

        List<TapChangerStepCreationInfos> ratioTapChangerStepCreationInfos = null;
        if (ratioTapChangerSteps != null && ratioTapChangerSteps.isEmpty() == false) {
            ratioTapChangerStepCreationInfos = ratioTapChangerSteps.stream().map(TapChangerStepCreationEmbeddable::toModificationInfos).collect(Collectors.toList());
        }
        builder.ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .enabled(AttributeModification.toAttributeModification(getRatioTapChangerEnabled()))
                .lowTapPosition(AttributeModification.toAttributeModification(getRatioTapChangerLowTapPosition()))
                .tapPosition(AttributeModification.toAttributeModification(getRatioTapChangerTapPosition()))
                .targetDeadband(AttributeModification.toAttributeModification(getRatioTapChangerTargetDeadband()))
                .regulating(AttributeModification.toAttributeModification(getRatioTapChangerRegulating()))
                .loadTapChangingCapabilities(AttributeModification.toAttributeModification(getRatioTapChangerLoadTapChangingCapabilities()))
                .targetV(AttributeModification.toAttributeModification(getRatioTapChangerTargetV()))
                .regulatingTerminalId(AttributeModification.toAttributeModification(getRatioTapChangerTerminalRefConnectableId()))
                .regulatingTerminalVlId(AttributeModification.toAttributeModification(getRatioTapChangerTerminalRefVoltageLevelId()))
                .regulatingTerminalType(AttributeModification.toAttributeModification(getRatioTapChangerTerminalRefType()))
                .steps(ratioTapChangerStepCreationInfos)
                .build());

        return builder;
    }
}
