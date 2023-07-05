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

import javax.persistence.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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

    @Column(name = "phasetapchangerregulating")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerregulating")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerregulatingOp"))
    })
    private BooleanModificationEmbedded phaseTapChangerRegulating;

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
        Optional.ofNullable(twoWindingsTransformerModificationInfos.getPhaseTapChanger()).ifPresent(this::assignPhaseTapChanger);
    }

    private void assignPhaseTapChanger(PhaseTapChangerModificationInfos phaseTapChanger) {
        this.phaseTapChangerRegulating = new BooleanModificationEmbedded(phaseTapChanger.getRegulating());
        this.phaseTapChangerRegulationMode = new EnumModificationEmbedded<>(phaseTapChanger.getRegulationMode());
        this.phaseTapChangerRegulationValue = new DoubleModificationEmbedded(phaseTapChanger.getRegulationValue());
        this.phaseTapChangerLowTapPosition = new IntegerModificationEmbedded(phaseTapChanger.getLowTapPosition());
        this.phaseTapChangerTapPosition = new IntegerModificationEmbedded(phaseTapChanger.getTapPosition());
        this.phaseTapChangerTargetDeadband = new DoubleModificationEmbedded(phaseTapChanger.getTargetDeadband());
        this.phaseTapChangerTerminalRefConnectableId = new StringModificationEmbedded(phaseTapChanger.getRegulatingTerminalId());
        this.phaseTapChangerTerminalRefVoltageLevelId = new StringModificationEmbedded(phaseTapChanger.getRegulatingTerminalVlId());
        this.phaseTapChangerTerminalRefType = new StringModificationEmbedded(phaseTapChanger.getRegulatingTerminalType());
        if (phaseTapChanger.getSteps() != null) {
            this.tapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddablePhaseTapChangerSteps(phaseTapChanger.getSteps()));
        }
    }

    @Override
    public TwoWindingsTransformerModificationInfos toModificationInfos() {
        return toTwoWindingsTransformerModificationInfosBuilder().build();
    }

    private TwoWindingsTransformerModificationInfos.TwoWindingsTransformerModificationInfosBuilder<?, ?> toTwoWindingsTransformerModificationInfosBuilder() {

        List<TapChangerStepCreationEmbeddable> phaseTapChangerSteps = null;
        if (getTapChangerSteps() != null && getTapChangerSteps().size() > 0) {
            phaseTapChangerSteps = getTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.PHASE)).sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.toList());
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

        PhaseTapChangerModificationInfos.PhaseTapChangerModificationInfosBuilder phaseTapChangerModificationInfosBuilder = PhaseTapChangerModificationInfos.builder()
            .lowTapPosition(AttributeModification.toAttributeModification(getPhaseTapChangerLowTapPosition()))
            .tapPosition(AttributeModification.toAttributeModification(getPhaseTapChangerTapPosition()))
            .targetDeadband(AttributeModification.toAttributeModification(getPhaseTapChangerTargetDeadband()))
            .regulating(AttributeModification.toAttributeModification(getPhaseTapChangerRegulating()))
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
