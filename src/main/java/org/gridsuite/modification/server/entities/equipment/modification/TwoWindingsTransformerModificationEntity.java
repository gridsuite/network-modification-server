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

import org.gridsuite.modification.TapChangerType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.entities.equipment.creation.TapChangerStepCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import jakarta.persistence.*;
import org.springframework.util.CollectionUtils;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "twoWindingsTransformerModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "twoWindingsTransformerModification_id_fk_constraint"))
public class TwoWindingsTransformerModificationEntity extends BranchModificationEntity {

    @Column(name = "g")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "g")),
        @AttributeOverride(name = "opType", column = @Column(name = "gOp"))
    })
    private DoubleModificationEmbedded g;

    @Column(name = "b")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "b")),
        @AttributeOverride(name = "opType", column = @Column(name = "bOp"))
    })
    private DoubleModificationEmbedded b;

    @Column(name = "ratedU1")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedU1")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedU1_op"))
    })
    private DoubleModificationEmbedded ratedU1;

    @Column(name = "ratedU2")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedU2")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedU2_op"))
    })
    private DoubleModificationEmbedded ratedU2;

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

    @Column(name = "ratiotapchangerregulationtype")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerregulationtype")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerregulationtypeOp"))
    })
    private EnumModificationEmbedded<VoltageRegulationType> ratioTapChangerRegulationType;

    @Column(name = "ratiotapchangerregulationside")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangerregulationside")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangerregulationsideOp"))
    })
    private EnumModificationEmbedded<RegulationSide> ratioTapChangerRegulationSide;

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

    @Column(name = "phasetapchangerenabled")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerenabled")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerenabledOp"))
    })
    private BooleanModificationEmbedded phaseTapChangerEnabled;

    @Column(name = "phasetapchangerregulationtype")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerregulationtype")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerregulationtypeOp"))
    })
    private EnumModificationEmbedded<VoltageRegulationType> phaseTapChangerRegulationType;

    @Column(name = "phasetapchangerregulationside")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangerregulationside")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangerregulationsideOp"))
    })
    private EnumModificationEmbedded<RegulationSide> phaseTapChangerRegulationSide;

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

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "phasetapchangertobeestimated")),
        @AttributeOverride(name = "opType", column = @Column(name = "phasetapchangertobeestimatedOp"))
    })
    private BooleanModificationEmbedded phaseTapChangerToBeEstimated;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratiotapchangertobeestimated")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratiotapchangertobeestimatedOp"))
    })
    private BooleanModificationEmbedded ratioTapChangerToBeEstimated;

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
        this.g = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getG());
        this.b = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getB());
        this.ratedU1 = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getRatedU1());
        this.ratedU2 = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getRatedU2());
        this.ratedS = new DoubleModificationEmbedded(twoWindingsTransformerModificationInfos.getRatedS());
        this.phaseTapChangerToBeEstimated = twoWindingsTransformerModificationInfos.getPhaseTapChangerToBeEstimated() != null ? new BooleanModificationEmbedded(twoWindingsTransformerModificationInfos.getPhaseTapChangerToBeEstimated()) : null;
        this.ratioTapChangerToBeEstimated = twoWindingsTransformerModificationInfos.getRatioTapChangerToBeEstimated() != null ? new BooleanModificationEmbedded(twoWindingsTransformerModificationInfos.getRatioTapChangerToBeEstimated()) : null;
        this.tapChangerSteps = new ArrayList<>();
        assignTapChangers(twoWindingsTransformerModificationInfos);
    }

    private void assignTapChangers(TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos) {
        Optional.ofNullable(twoWindingsTransformerModificationInfos.getRatioTapChanger()).ifPresent(this::assignRatioTapChanger);
        Optional.ofNullable(twoWindingsTransformerModificationInfos.getPhaseTapChanger()).ifPresent(this::assignPhaseTapChanger);
    }

    private void assignRatioTapChanger(RatioTapChangerModificationInfos ratioTapChanger) {
        this.ratioTapChangerEnabled = new BooleanModificationEmbedded(ratioTapChanger.getEnabled());
        this.ratioTapChangerRegulationType = new EnumModificationEmbedded<>(ratioTapChanger.getRegulationType());
        this.ratioTapChangerRegulationSide = new EnumModificationEmbedded<>(ratioTapChanger.getRegulationSide());
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
            this.tapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddableRatioTapChangerSteps(ratioTapChanger.getSteps()));
        }
    }

    private void assignPhaseTapChanger(PhaseTapChangerModificationInfos phaseTapChanger) {
        this.phaseTapChangerEnabled = new BooleanModificationEmbedded(phaseTapChanger.getEnabled());
        this.phaseTapChangerRegulationType = new EnumModificationEmbedded<>(phaseTapChanger.getRegulationType());
        this.phaseTapChangerRegulationSide = new EnumModificationEmbedded<>(phaseTapChanger.getRegulationSide());
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

        List<TapChangerStepCreationEmbeddable> ratioTapChangerStepsEmbeddable = null;
        List<TapChangerStepCreationEmbeddable> phaseTapChangerStepsEmbeddable = null;
        if (getTapChangerSteps() != null && getTapChangerSteps().size() > 0) {
            Map<TapChangerType, List<TapChangerStepCreationEmbeddable>> tapChangerStepsMap = getTapChangerSteps().stream().sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.groupingBy(TapChangerStepCreationEmbeddable::getTapChangerType));
            ratioTapChangerStepsEmbeddable = tapChangerStepsMap.get(TapChangerType.RATIO);
            phaseTapChangerStepsEmbeddable = tapChangerStepsMap.get(TapChangerType.PHASE);

        }

        TwoWindingsTransformerModificationInfos.TwoWindingsTransformerModificationInfosBuilder<?, ?> builder = TwoWindingsTransformerModificationInfos
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
                .r(IAttributeModificationEmbeddable.toAttributeModification(getR()))
                .x(IAttributeModificationEmbeddable.toAttributeModification(getX()))
                .g(IAttributeModificationEmbeddable.toAttributeModification(getG()))
                .b(IAttributeModificationEmbeddable.toAttributeModification(getB()))
                .ratedU1(IAttributeModificationEmbeddable.toAttributeModification(getRatedU1()))
                .ratedU2(IAttributeModificationEmbeddable.toAttributeModification(getRatedU2()))
                .ratedS(IAttributeModificationEmbeddable.toAttributeModification(getRatedS()))
                .p1MeasurementValue(toAttributeModification(getP1MeasurementValue()))
                .p1MeasurementValidity(toAttributeModification(getP1MeasurementValidity()))
                .q1MeasurementValue(toAttributeModification(getQ1MeasurementValue()))
                .q1MeasurementValidity(toAttributeModification(getQ1MeasurementValidity()))
                .p2MeasurementValue(toAttributeModification(getP2MeasurementValue()))
                .p2MeasurementValidity(toAttributeModification(getP2MeasurementValidity()))
                .q2MeasurementValue(toAttributeModification(getQ2MeasurementValue()))
                .q2MeasurementValidity(toAttributeModification(getQ2MeasurementValidity()))
                .phaseTapChangerToBeEstimated(toAttributeModification(getPhaseTapChangerToBeEstimated()))
                .ratioTapChangerToBeEstimated(toAttributeModification(getRatioTapChangerToBeEstimated()))
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

        List<TapChangerStepCreationInfos> ratioTapChangerStepCreationInfos = null;
        if (ratioTapChangerStepsEmbeddable != null && !ratioTapChangerStepsEmbeddable.isEmpty()) {
            ratioTapChangerStepCreationInfos = ratioTapChangerStepsEmbeddable.stream().map(TapChangerStepCreationEmbeddable::toModificationInfos).collect(Collectors.toList());
        }
        builder.ratioTapChanger(RatioTapChangerModificationInfos.builder()
                .enabled(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerEnabled()))
                .regulationType(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerRegulationType()))
                .regulationSide(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerRegulationSide()))
                .lowTapPosition(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerLowTapPosition()))
                .tapPosition(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerTapPosition()))
                .targetDeadband(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerTargetDeadband()))
                .regulating(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerRegulating()))
                .loadTapChangingCapabilities(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerLoadTapChangingCapabilities()))
                .targetV(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerTargetV()))
                .regulatingTerminalId(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerTerminalRefConnectableId()))
                .regulatingTerminalVlId(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerTerminalRefVoltageLevelId()))
                .regulatingTerminalType(IAttributeModificationEmbeddable.toAttributeModification(getRatioTapChangerTerminalRefType()))
                .steps(ratioTapChangerStepCreationInfos)
                .build());

        List<TapChangerStepCreationInfos> phaseTapChangerStepCreationInfos = null;
        if (phaseTapChangerStepsEmbeddable != null && !phaseTapChangerStepsEmbeddable.isEmpty()) {
            phaseTapChangerStepCreationInfos = phaseTapChangerStepsEmbeddable.stream().map(TapChangerStepCreationEmbeddable::toModificationInfos).toList();
        }
        builder.phaseTapChanger(PhaseTapChangerModificationInfos.builder()
            .enabled(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerEnabled()))
            .regulationType(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerRegulationType()))
            .regulationSide(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerRegulationSide()))
            .lowTapPosition(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerLowTapPosition()))
            .tapPosition(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerTapPosition()))
            .targetDeadband(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerTargetDeadband()))
            .regulationMode(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerRegulationMode()))
            .regulationValue(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerRegulationValue()))
            .regulatingTerminalId(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerTerminalRefConnectableId()))
            .regulatingTerminalVlId(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerTerminalRefVoltageLevelId()))
            .regulatingTerminalType(IAttributeModificationEmbeddable.toAttributeModification(getPhaseTapChangerTerminalRefType()))
            .steps(phaseTapChangerStepCreationInfos)
            .build());

        return builder;
    }
}
