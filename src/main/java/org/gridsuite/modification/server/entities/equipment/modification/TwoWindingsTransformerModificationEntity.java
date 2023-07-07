/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.PhaseTapChanger;
import lombok.Getter;
import lombok.NoArgsConstructor;

import org.gridsuite.modification.server.TapChangerType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.creation.TapChangerStepCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;

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
    private List<TapChangerStepCreationEmbeddable> ratioTapChangerSteps;

    @Column(name = "phasetapchangerenabled")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerenabled")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerenabledOp"))
    })
    private BooleanModificationEmbedded phaseTapChangerEnabled;

    @Column(name = "phasetapchangerlowtapposition")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerlowtapposition")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerlowtappositionOp"))
    })
    private IntegerModificationEmbedded phaseTapChangerLowTapPosition;

    @Column(name = "phasetapchangertapposition")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangertapposition")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangertappositionOp"))
    })
    private IntegerModificationEmbedded phaseTapChangerTapPosition;

    @Column(name = "phasetapchangertargetdeadband")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangertargetdeadband")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangertargetdeadbandOp"))
    })
    private DoubleModificationEmbedded phaseTapChangerTargetDeadband;

    @Column(name = "phasetapchangerterminalrefconnectableid")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerterminalrefconnectableid")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerterminalrefconnectableidOp"))
    })
    private StringModificationEmbedded phaseTapChangerTerminalRefConnectableId;

    @Column(name = "phasetapchangerterminalrefvoltagelevelid")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerterminalrefvoltagelevelid")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerterminalrefvoltagelevelidOp"))
    })
    private StringModificationEmbedded phaseTapChangerTerminalRefVoltageLevelId;

    @Column(name = "phasetapchangerterminalreftype")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerterminalreftype")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerterminalreftypeOp"))
    })
    private StringModificationEmbedded phaseTapChangerTerminalRefType;

    @Column(name = "phasetapchangerregulationmode")
    @Enumerated(EnumType.STRING)
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerregulationmode")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerregulationmodeOp"))
    })
    private EnumModificationEmbedded<PhaseTapChanger.RegulationMode> phaseTapChangerRegulationMode;

    @Column(name = "phasetapchangerregulationvalue")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerregulationvalue")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerregulationvalueOp"))
    })
    private DoubleModificationEmbedded phaseTapChangerRegulationValue;

    @ElementCollection
    @CollectionTable(
        name = "tapChangerStepModification",
        joinColumns = @JoinColumn(name = "modification_id")
    )
    private List<TapChangerStepCreationEmbeddable> phaseTapChangerSteps;

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
        this.ratioTapChangerSteps = new ArrayList<>();
        this.phaseTapChangerSteps = new ArrayList<>();
        assignTapChanger(twoWindingsTransformerModificationInfos);
    }

    private void assignTapChanger(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos) {
        Optional.ofNullable(twoWindingsTransformerModificationInfos.getRatioTapChanger()).ifPresent(this::assignRatioTapChanger);
        Optional.ofNullable(twoWindingsTransformerModificationInfos.getPhaseTapChanger()).ifPresent(this::assignPhaseTapChanger);
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
        if (ratioTapChanger.getSteps() != null) {
            this.ratioTapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddableRatioTapChangerSteps(ratioTapChanger.getSteps()));
        }
    }

    private void assignPhaseTapChanger(PhaseTapChangerModificationInfos phaseTapChanger) {
        this.phaseTapChangerEnabled = new BooleanModificationEmbedded(phaseTapChanger.getEnabled());
        this.phaseTapChangerRegulationMode = new EnumModificationEmbedded<>(phaseTapChanger.getRegulationMode());
        this.phaseTapChangerRegulationValue = new DoubleModificationEmbedded(phaseTapChanger.getRegulationValue());
        this.phaseTapChangerLowTapPosition = new IntegerModificationEmbedded(phaseTapChanger.getLowTapPosition());
        this.phaseTapChangerTapPosition = new IntegerModificationEmbedded(phaseTapChanger.getTapPosition());
        this.phaseTapChangerTargetDeadband = new DoubleModificationEmbedded(phaseTapChanger.getTargetDeadband());
        this.phaseTapChangerTerminalRefConnectableId = new StringModificationEmbedded(phaseTapChanger.getRegulatingTerminalId());
        this.phaseTapChangerTerminalRefVoltageLevelId = new StringModificationEmbedded(phaseTapChanger.getRegulatingTerminalVlId());
        this.phaseTapChangerTerminalRefType = new StringModificationEmbedded(phaseTapChanger.getRegulatingTerminalType());
        if (phaseTapChanger.getSteps() != null) {
            this.phaseTapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddablePhaseTapChangerSteps(phaseTapChanger.getSteps()));
        }
    }

    @Override
    public TwoWindingsTransformerModificationInfos toModificationInfos() {
        return toTwoWindingsTransformerModificationInfosBuilder().build();
    }

    private TwoWindingsTransformerModificationInfos.TwoWindingsTransformerModificationInfosBuilder<?, ?> toTwoWindingsTransformerModificationInfosBuilder() {
        List<TapChangerStepCreationEmbeddable> ratioTapChangerSteps = null;
        if (getRatioTapChangerSteps() != null && getRatioTapChangerSteps().size() > 0) {
            ratioTapChangerSteps = getRatioTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.RATIO)).sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.toList());
        }
        List<TapChangerStepCreationEmbeddable> phaseTapChangerSteps = null;
        if (getPhaseTapChangerSteps() != null && getPhaseTapChangerSteps().size() > 0) {
            phaseTapChangerSteps = getPhaseTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.PHASE)).sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.toList());
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
        if (ratioTapChangerSteps != null && !ratioTapChangerSteps.isEmpty()) {
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

        PhaseTapChangerModificationInfos.PhaseTapChangerModificationInfosBuilder phaseTapChangerModificationInfosBuilder = PhaseTapChangerModificationInfos.builder()
            .enabled(AttributeModification.toAttributeModification(getPhaseTapChangerEnabled()))
            .lowTapPosition(AttributeModification.toAttributeModification(getPhaseTapChangerLowTapPosition()))
            .tapPosition(AttributeModification.toAttributeModification(getPhaseTapChangerTapPosition()))
            .targetDeadband(AttributeModification.toAttributeModification(getPhaseTapChangerTargetDeadband()))
            .regulationMode(AttributeModification.toAttributeModification(getPhaseTapChangerRegulationMode()))
            .regulationValue(AttributeModification.toAttributeModification(getPhaseTapChangerRegulationValue()))
            .regulatingTerminalId(AttributeModification.toAttributeModification(getPhaseTapChangerTerminalRefConnectableId()))
            .regulatingTerminalVlId(AttributeModification.toAttributeModification(getPhaseTapChangerTerminalRefVoltageLevelId()))
            .regulatingTerminalType(AttributeModification.toAttributeModification(getPhaseTapChangerTerminalRefType()));

        if (phaseTapChangerSteps != null) {
            List<TapChangerStepCreationInfos> phaseTapChangerStepCreationInfos = phaseTapChangerSteps.stream().map(TapChangerStepCreationEmbeddable::toModificationInfos).toList();
            if (!phaseTapChangerStepCreationInfos.isEmpty()) {
                phaseTapChangerModificationInfosBuilder.steps(phaseTapChangerStepCreationInfos);
            }
        }
        builder.phaseTapChanger(phaseTapChangerModificationInfosBuilder.build());
        return builder;
    }
}
