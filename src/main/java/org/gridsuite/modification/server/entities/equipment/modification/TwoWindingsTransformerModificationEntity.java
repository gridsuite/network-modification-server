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
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;

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
    private Integer phaseTapChangerLowTapPosition;

    @Column(name = "phasetapchangertapposition")
    private Integer phaseTapChangerTapPosition;

    @Column(name = "phasetapchangerregulating")
    private Boolean phaseTapChangerRegulating;

    @Column(name = "phasetapchangertargetdeadband")
    private Double phaseTapChangerTargetDeadband;

    @Column(name = "phasetapchangerterminalrefconnectableid")
    private String phaseTapChangerTerminalRefConnectableId;

    @Column(name = "phasetapchangerterminalrefvoltagelevelid")
    private String phaseTapChangerTerminalRefVoltageLevelId;

    @Column(name = "phasetapchangerterminalreftype")
    private String phaseTapChangerTerminalRefType;

    @Column(name = "phasetapchangerregulationmode")
    @Enumerated(EnumType.STRING)
    private PhaseTapChanger.RegulationMode phaseTapChangerRegulationMode;

    @Column(name = "phasetapchangerregulationvalue")
    private Double phaseTapChangerRegulationValue;

    @ElementCollection
    @CollectionTable(
        name = "tapChangerStepModification",
        joinColumns = @JoinColumn(name = "modification_id")
    )
    private List<TapChangerStepModificationEmbeddable> tapChangerSteps;

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
        this.phaseTapChangerRegulating = phaseTapChanger.getRegulating();
        this.phaseTapChangerRegulationMode = phaseTapChanger.getRegulationMode();
        this.phaseTapChangerRegulationValue = phaseTapChanger.getRegulationValue();
        this.phaseTapChangerLowTapPosition = phaseTapChanger.getLowTapPosition();
        this.phaseTapChangerTapPosition = phaseTapChanger.getTapPosition();
        this.phaseTapChangerTargetDeadband = phaseTapChanger.getTargetDeadband();
        this.phaseTapChangerTerminalRefConnectableId = phaseTapChanger.getRegulatingTerminalId();
        this.phaseTapChangerTerminalRefVoltageLevelId = phaseTapChanger.getRegulatingTerminalVlId();
        this.phaseTapChangerTerminalRefType = phaseTapChanger.getRegulatingTerminalType();
        this.tapChangerSteps.addAll(TapChangerStepModificationEmbeddable.toEmbeddablePhaseTapChangerSteps(phaseTapChanger.getSteps()));
    }

    @Override
    public TwoWindingsTransformerModificationInfos toModificationInfos() {
        return toTwoWindingsTransformerModificationInfosBuilder().build();
    }

    private TwoWindingsTransformerModificationInfos.TwoWindingsTransformerModificationInfosBuilder<?, ?> toTwoWindingsTransformerModificationInfosBuilder() {

        List<TapChangerStepModificationEmbeddable> phaseTapChangerSteps = new ArrayList<>();
        if (getTapChangerSteps() != null && getTapChangerSteps().size() > 0) {
            phaseTapChangerSteps = getTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.PHASE)).sorted(Comparator.comparing(TapChangerStepModificationEmbeddable::getIndex)).collect(Collectors.toList());
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

        if (!phaseTapChangerSteps.isEmpty()) {
            List<TapChangerStepModificationInfos> phaseTapChangerStepModificationInfos = phaseTapChangerSteps.stream().map(TapChangerStepModificationEmbeddable::toModificationInfos).collect(Collectors.toList());
            builder.phaseTapChanger(PhaseTapChangerModificationInfos.builder()
                .lowTapPosition(getPhaseTapChangerLowTapPosition())
                .tapPosition(getPhaseTapChangerTapPosition())
                .targetDeadband(getPhaseTapChangerTargetDeadband())
                .regulating(getPhaseTapChangerRegulating())
                .regulationMode(getPhaseTapChangerRegulationMode())
                .regulationValue(getPhaseTapChangerRegulationValue())
                .regulatingTerminalId(getPhaseTapChangerTerminalRefConnectableId())
                .regulatingTerminalVlId(getPhaseTapChangerTerminalRefVoltageLevelId())
                .regulatingTerminalType(getPhaseTapChangerTerminalRefType())
                .steps(phaseTapChangerStepModificationInfos)
                .build());
        }

        return builder;
    }
}
