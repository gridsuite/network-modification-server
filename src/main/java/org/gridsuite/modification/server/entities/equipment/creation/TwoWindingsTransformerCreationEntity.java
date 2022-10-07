/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.PhaseTapChanger;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.TapChangerType;
import org.gridsuite.modification.server.dto.*;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "twoWindingsTransformerCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "twoWindingsTransformerCreation_id_fk_constraint"))
public class TwoWindingsTransformerCreationEntity extends BranchCreationEntity {

    @Column(name = "magnetizingConductance")
    private double magnetizingConductance;

    @Column(name = "magnetizingSusceptance")
    private double magnetizingSusceptance;

    @Column(name = "ratedVoltage1")
    private double ratedVoltage1;

    @Column(name = "ratedVoltage2")
    private double ratedVoltage2;

    @Column(name = "rateds")
    private Double ratedS;

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
            name = "tapChangerStepCreation",
            joinColumns = @JoinColumn(name = "modification_id")
    )
    private List<TapChangerStepCreationEmbeddable> tapChangerSteps;

    public TwoWindingsTransformerCreationEntity(String equipmentId,
                                                String equipmentName,
                                                double seriesResistance,
                                                double seriesReactance,
                                                double magnetizingConductance,
                                                double magnetizingSusceptance,
                                                double ratedVoltage1,
                                                double ratedVoltage2,
                                                Double ratedS,
                                                String voltageLevelId1,
                                                String busOrBusbarSectionId1,
                                                String voltageLevelId2,
                                                String busOrBusbarSectionId2,
                                                Double permanentCurrentLimit1,
                                                Double permanentCurrentLimit2,
                                                Integer phaseTapChangerLowTapPosition,
                                                Integer phaseTapChangerTapPosition,
                                                Boolean phaseTapChangerRegulating,
                                                Double phaseTapChangerTargetDeadband,
                                                String phaseTapChangerTerminalRefConnectableId,
                                                String phaseTapChangerTerminalRefVoltageLevelId,
                                                String phaseTapChangerTerminalRefType,
                                                PhaseTapChanger.RegulationMode phaseTapChangerRegulationMode,
                                                Double phaseTapChangerRegulationValue,
                                                Integer ratioTapChangerLowTapPosition,
                                                Integer ratioTapChangerTapPosition,
                                                Boolean ratioTapChangerRegulating,
                                                Double ratioTapChangerTargetDeadband,
                                                String ratioTapChangerTerminalRefConnectableId,
                                                String ratioTapChangerTerminalRefVoltageLevelId,
                                                String ratioTapChangerTerminalRefType,
                                                Boolean ratioTapChangerLoadTapChangingCapabilities,
                                                Double ratioTapChangerTargetV,
                                                List<TapChangerStepCreationEmbeddable> tapChangerSteps
    ) {
        super(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION,
                equipmentId,
                equipmentName,
                seriesResistance,
                seriesReactance,
                voltageLevelId1,
                voltageLevelId2,
                busOrBusbarSectionId1,
                busOrBusbarSectionId2,
                permanentCurrentLimit1 != null ? new CurrentLimitsEntity(null, permanentCurrentLimit1) : null,
                permanentCurrentLimit2 != null ? new CurrentLimitsEntity(null, permanentCurrentLimit2) : null);
        this.magnetizingConductance = magnetizingConductance;
        this.magnetizingSusceptance = magnetizingSusceptance;
        this.ratedVoltage1 = ratedVoltage1;
        this.ratedVoltage2 = ratedVoltage2;
        this.ratedS = ratedS;

        this.phaseTapChangerLowTapPosition = phaseTapChangerLowTapPosition;
        this.phaseTapChangerTapPosition = phaseTapChangerTapPosition;
        this.phaseTapChangerRegulating = phaseTapChangerRegulating;
        this.phaseTapChangerTargetDeadband = phaseTapChangerTargetDeadband;
        this.phaseTapChangerTerminalRefConnectableId = phaseTapChangerTerminalRefConnectableId;
        this.phaseTapChangerTerminalRefVoltageLevelId = phaseTapChangerTerminalRefVoltageLevelId;
        this.phaseTapChangerTerminalRefType = phaseTapChangerTerminalRefType;
        this.phaseTapChangerRegulationMode = phaseTapChangerRegulationMode;
        this.phaseTapChangerRegulationValue = phaseTapChangerRegulationValue;
        this.ratioTapChangerLowTapPosition = ratioTapChangerLowTapPosition;
        this.ratioTapChangerTapPosition = ratioTapChangerTapPosition;
        this.ratioTapChangerRegulating = ratioTapChangerRegulating;
        this.ratioTapChangerTargetDeadband = ratioTapChangerTargetDeadband;
        this.ratioTapChangerTerminalRefConnectableId = ratioTapChangerTerminalRefConnectableId;
        this.ratioTapChangerTerminalRefVoltageLevelId = ratioTapChangerTerminalRefVoltageLevelId;
        this.ratioTapChangerTerminalRefType = ratioTapChangerTerminalRefType;
        this.ratioTapChangerLoadTapChangingCapabilities = ratioTapChangerLoadTapChangingCapabilities;
        this.ratioTapChangerTargetV = ratioTapChangerTargetV;
        this.tapChangerSteps = tapChangerSteps;
    }

    @Override
    public TwoWindingsTransformerCreationInfos toModificationInfos() {
        return toTwoWindingsTransformerCreationInfosBuilder().build();
    }

    public static List<TapChangerStepCreationEmbeddable> toEmbeddableRatioTapChangerSteps(List<RatioTapChangerStepInfos> tapChangerStepsInfos) {
        List<TapChangerStepCreationEmbeddable> ratioTapChangerSteps = new ArrayList<>();
        if (tapChangerStepsInfos != null) {
            for (RatioTapChangerStepInfos ratioTapChangerStepInfos : tapChangerStepsInfos) {
                ratioTapChangerSteps.add(new TapChangerStepCreationEmbeddable(TapChangerType.RATIO, ratioTapChangerStepInfos.getIndex(), ratioTapChangerStepInfos.getRho(), ratioTapChangerStepInfos.getR(), ratioTapChangerStepInfos.getX(), ratioTapChangerStepInfos.getG(), ratioTapChangerStepInfos.getB(), null));
            }
        }
        return ratioTapChangerSteps;
    }

    public static List<TapChangerStepCreationEmbeddable> toEmbeddablePhaseTapChangerSteps(List<PhaseTapChangerStepInfos> tapChangerStepsInfos) {
        List<TapChangerStepCreationEmbeddable> phaseTapChangerSteps = new ArrayList<>();
        if (tapChangerStepsInfos != null) {
            for (PhaseTapChangerStepInfos phaseTapChangerStepInfos : tapChangerStepsInfos) {
                phaseTapChangerSteps.add(new TapChangerStepCreationEmbeddable(TapChangerType.PHASE, phaseTapChangerStepInfos.getIndex(), phaseTapChangerStepInfos.getRho(), phaseTapChangerStepInfos.getR(), phaseTapChangerStepInfos.getX(), phaseTapChangerStepInfos.getG(), phaseTapChangerStepInfos.getB(), phaseTapChangerStepInfos.getAlpha()));
            }
        }
        return phaseTapChangerSteps;
    }

    public static TwoWindingsTransformerCreationEntity toEntity(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        TwoWindingsTransformerCreationEntity twoWindingsTransformerCreationEntity;
        List<TapChangerStepCreationEmbeddable> tapChangerSteps = new ArrayList<>();
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            tapChangerSteps.addAll(toEmbeddableRatioTapChangerSteps(twoWindingsTransformerCreationInfos.getRatioTapChanger().getSteps()));
        }
        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            tapChangerSteps.addAll(toEmbeddablePhaseTapChangerSteps(twoWindingsTransformerCreationInfos.getPhaseTapChanger().getSteps()));
        }
        PhaseTapChangerInfos phaseTapChangerInfos = twoWindingsTransformerCreationInfos.getPhaseTapChanger();
        RatioTapChangerInfos ratioTapChangerInfos = twoWindingsTransformerCreationInfos.getRatioTapChanger();

        twoWindingsTransformerCreationEntity = new TwoWindingsTransformerCreationEntity(twoWindingsTransformerCreationInfos.getEquipmentId(),
                twoWindingsTransformerCreationInfos.getEquipmentName(),
                twoWindingsTransformerCreationInfos.getSeriesResistance(),
                twoWindingsTransformerCreationInfos.getSeriesReactance(),
                twoWindingsTransformerCreationInfos.getMagnetizingConductance(),
                twoWindingsTransformerCreationInfos.getMagnetizingSusceptance(),
                twoWindingsTransformerCreationInfos.getRatedVoltage1(),
                twoWindingsTransformerCreationInfos.getRatedVoltage2(),
                twoWindingsTransformerCreationInfos.getRatedS(),
                twoWindingsTransformerCreationInfos.getVoltageLevelId1(),
                twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId1(),
                twoWindingsTransformerCreationInfos.getVoltageLevelId2(),
                twoWindingsTransformerCreationInfos.getBusOrBusbarSectionId2(),
                twoWindingsTransformerCreationInfos.getCurrentLimits1() != null ? twoWindingsTransformerCreationInfos.getCurrentLimits1().getPermanentLimit() : null,
                twoWindingsTransformerCreationInfos.getCurrentLimits2() != null ? twoWindingsTransformerCreationInfos.getCurrentLimits2().getPermanentLimit() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getLowTapPosition() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getTapPosition() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.isRegulating() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getTargetDeadband() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getRegulatingTerminalId() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getRegulatingTerminalVlId() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getRegulatingTerminalType() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getRegulationMode() : null,
                phaseTapChangerInfos != null ? phaseTapChangerInfos.getRegulationValue() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getLowTapPosition() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getTapPosition() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.isRegulating() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getTargetDeadband() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getRegulatingTerminalId() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getRegulatingTerminalVlId() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getRegulatingTerminalType() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.isLoadTapChangingCapabilities() : null,
                ratioTapChangerInfos != null ? ratioTapChangerInfos.getTargetV() : null,
                tapChangerSteps);

        return twoWindingsTransformerCreationEntity;
    }

    private TwoWindingsTransformerCreationInfos.TwoWindingsTransformerCreationInfosBuilder<?, ?> toTwoWindingsTransformerCreationInfosBuilder() {
        List<TapChangerStepCreationEmbeddable> ratioTapChangerSteps = new ArrayList<>();
        List<TapChangerStepCreationEmbeddable> phaseTapChangerSteps = new ArrayList<>();
        if (getTapChangerSteps() != null && getTapChangerSteps().size() > 0) {
            ratioTapChangerSteps = getTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.RATIO)).sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.toList());
            phaseTapChangerSteps = getTapChangerSteps().stream().filter(step -> step.getTapChangerType().equals(TapChangerType.PHASE)).sorted(Comparator.comparing(TapChangerStepCreationEmbeddable::getIndex)).collect(Collectors.toList());
        }

        TwoWindingsTransformerCreationInfos.TwoWindingsTransformerCreationInfosBuilder<?, ?> builder = TwoWindingsTransformerCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .seriesResistance(getSeriesResistance())
                .seriesReactance(getSeriesReactance())
                .magnetizingConductance(getMagnetizingConductance())
                .magnetizingSusceptance(getMagnetizingSusceptance())
                .ratedVoltage1(getRatedVoltage1())
                .ratedVoltage2(getRatedVoltage2())
                .ratedS(getRatedS())
                .voltageLevelId1(getVoltageLevelId1())
                .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
                .voltageLevelId2(getVoltageLevelId2())
                .busOrBusbarSectionId2(getBusOrBusbarSectionId2());

        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }

        if (!ratioTapChangerSteps.isEmpty()) {
            List<RatioTapChangerStepInfos> ratioTapChangerStepInfos = ratioTapChangerSteps.stream().map(step -> RatioTapChangerStepInfos.builder().index(step.getIndex()).rho(step.getRho()).r(step.getR()).x(step.getX()).g(step.getG()).b(step.getB()).build()).collect(Collectors.toList());
            builder.ratioTapChanger(RatioTapChangerInfos.builder()
                    .lowTapPosition(getRatioTapChangerLowTapPosition())
                    .tapPosition(getRatioTapChangerTapPosition())
                    .targetDeadband(getRatioTapChangerTargetDeadband())
                    .regulating(getRatioTapChangerRegulating())
                    .loadTapChangingCapabilities(getRatioTapChangerLoadTapChangingCapabilities())
                    .targetV(getRatioTapChangerTargetV())
                    .regulatingTerminalId(getRatioTapChangerTerminalRefConnectableId())
                    .regulatingTerminalVlId(getRatioTapChangerTerminalRefVoltageLevelId())
                    .regulatingTerminalType(getRatioTapChangerTerminalRefType())
                    .steps(ratioTapChangerStepInfos)
                    .build());
        }

        if (!phaseTapChangerSteps.isEmpty()) {
            List<PhaseTapChangerStepInfos> phaseTapChangerStepInfos = phaseTapChangerSteps.stream().map(step -> PhaseTapChangerStepInfos.builder().index(step.getIndex()).rho(step.getRho()).r(step.getR()).x(step.getX()).g(step.getG()).b(step.getB()).alpha(step.getAlpha()).build()).collect(Collectors.toList());
            builder.phaseTapChanger(PhaseTapChangerInfos.builder()
                    .lowTapPosition(getPhaseTapChangerLowTapPosition())
                    .tapPosition(getPhaseTapChangerTapPosition())
                    .targetDeadband(getPhaseTapChangerTargetDeadband())
                    .regulating(getPhaseTapChangerRegulating())
                    .regulationMode(getPhaseTapChangerRegulationMode())
                    .regulationValue(getPhaseTapChangerRegulationValue())
                    .regulatingTerminalId(getPhaseTapChangerTerminalRefConnectableId())
                    .regulatingTerminalVlId(getPhaseTapChangerTerminalRefVoltageLevelId())
                    .regulatingTerminalType(getPhaseTapChangerTerminalRefType())
                    .steps(phaseTapChangerStepInfos)
                    .build());
        }

        return builder;
    }

}
