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
import org.gridsuite.modification.TapChangerType;
import org.gridsuite.modification.dto.*;

import jakarta.persistence.*;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
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

    @Column(name = "g")
    private double g;

    @Column(name = "b")
    private double b;

    @Column(name = "rated_u1")
    private double ratedU1;

    @Column(name = "rated_u2")
    private double ratedU2;

    @Column(name = "rated_s")
    private Double ratedS;

    @Column(name = "phase_tap_changer_low_tap_position")
    private Integer phaseTapChangerLowTapPosition;

    @Column(name = "phase_tap_changer_tap_position")
    private Integer phaseTapChangerTapPosition;

    @Column(name = "phase_tap_changer_regulating")
    private Boolean phaseTapChangerRegulating;

    @Column(name = "phase_tap_changer_target_deadband")
    private Double phaseTapChangerTargetDeadband;

    @Column(name = "phase_tap_changer_terminal_ref_connectable_id")
    private String phaseTapChangerTerminalRefConnectableId;

    @Column(name = "phase_tap_changer_terminal_ref_voltage_level_id")
    private String phaseTapChangerTerminalRefVoltageLevelId;

    @Column(name = "phase_tap_changer_terminal_ref_type")
    private String phaseTapChangerTerminalRefType;

    @Column(name = "phase_tap_changer_regulation_mode")
    @Enumerated(EnumType.STRING)
    private PhaseTapChanger.RegulationMode phaseTapChangerRegulationMode;

    @Column(name = "phase_tap_changer_regulation_value")
    private Double phaseTapChangerRegulationValue;

    @Column(name = "ratio_tap_changer_low_tap_position")
    private Integer ratioTapChangerLowTapPosition;

    @Column(name = "ratio_tap_changer_tap_position")
    private Integer ratioTapChangerTapPosition;

    @Column(name = "ratio_tap_changer_regulating")
    private Boolean ratioTapChangerRegulating;

    @Column(name = "ratio_tap_changer_target_deadband")
    private Double ratioTapChangerTargetDeadband;

    @Column(name = "ratio_tap_changer_terminal_ref_connectable_id")
    private String ratioTapChangerTerminalRefConnectableId;

    @Column(name = "ratio_tap_changer_terminal_ref_voltage_level_id")
    private String ratioTapChangerTerminalRefVoltageLevelId;

    @Column(name = "ratio_tap_changer_terminal_ref_type")
    private String ratioTapChangerTerminalRefType;

    @Column(name = "ratio_tap_changer_load_tap_changing_capabilities")
    private Boolean ratioTapChangerLoadTapChangingCapabilities;

    @Column(name = "ratio_tap_changer_target_v")
    private Double ratioTapChangerTargetV;

    @ElementCollection
    @CollectionTable(
            name = "tapChangerStepCreation",
            joinColumns = @JoinColumn(name = "modification_id")
    )
    private List<TapChangerStepCreationEmbeddable> tapChangerSteps;

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos = (TwoWindingsTransformerCreationInfos) modificationInfos;
        assignAttributes(twoWindingsTransformerCreationInfos);
    }

    public TwoWindingsTransformerCreationEntity(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        super(twoWindingsTransformerCreationInfos);
        assignAttributes(twoWindingsTransformerCreationInfos);
    }

    private void assignAttributes(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        this.g = twoWindingsTransformerCreationInfos.getG();
        this.b = twoWindingsTransformerCreationInfos.getB();
        this.ratedU1 = twoWindingsTransformerCreationInfos.getRatedU1();
        this.ratedU2 = twoWindingsTransformerCreationInfos.getRatedU2();
        this.ratedS = twoWindingsTransformerCreationInfos.getRatedS();
        this.tapChangerSteps = new ArrayList<>();
        assignTapChanger(twoWindingsTransformerCreationInfos);
    }

    private void assignTapChanger(TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos) {
        Optional.ofNullable(twoWindingsTransformerCreationInfos.getPhaseTapChanger()).ifPresent(this::assignPhaseTapChanger);
        Optional.ofNullable(twoWindingsTransformerCreationInfos.getRatioTapChanger()).ifPresent(this::assignRatioTapChanger);
    }

    private void assignRatioTapChanger(RatioTapChangerCreationInfos ratioTapChanger) {
        this.ratioTapChangerLowTapPosition = ratioTapChanger.getLowTapPosition();
        this.ratioTapChangerTapPosition = ratioTapChanger.getTapPosition();
        this.ratioTapChangerRegulating = ratioTapChanger.isRegulating();
        this.ratioTapChangerTargetDeadband = ratioTapChanger.getTargetDeadband();
        this.ratioTapChangerTerminalRefConnectableId = ratioTapChanger.getRegulatingTerminalId();
        this.ratioTapChangerTerminalRefVoltageLevelId = ratioTapChanger.getRegulatingTerminalVlId();
        this.ratioTapChangerTerminalRefType = ratioTapChanger.getRegulatingTerminalType();
        this.ratioTapChangerLoadTapChangingCapabilities = ratioTapChanger.isLoadTapChangingCapabilities();
        this.ratioTapChangerTargetV = ratioTapChanger.getTargetV();
        this.tapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddableRatioTapChangerSteps(ratioTapChanger.getSteps()));
    }

    private void assignPhaseTapChanger(PhaseTapChangerCreationInfos phaseTapChangerCreationInfos) {
        this.phaseTapChangerLowTapPosition = phaseTapChangerCreationInfos.getLowTapPosition();
        this.phaseTapChangerTapPosition = phaseTapChangerCreationInfos.getTapPosition();
        this.phaseTapChangerRegulating = phaseTapChangerCreationInfos.isRegulating();
        this.phaseTapChangerTargetDeadband = phaseTapChangerCreationInfos.getTargetDeadband();
        this.phaseTapChangerTerminalRefConnectableId = phaseTapChangerCreationInfos.getRegulatingTerminalId();
        this.phaseTapChangerTerminalRefVoltageLevelId = phaseTapChangerCreationInfos.getRegulatingTerminalVlId();
        this.phaseTapChangerTerminalRefType = phaseTapChangerCreationInfos.getRegulatingTerminalType();
        this.phaseTapChangerRegulationMode = phaseTapChangerCreationInfos.getRegulationMode();
        this.phaseTapChangerRegulationValue = phaseTapChangerCreationInfos.getRegulationValue();
        this.tapChangerSteps.addAll(TapChangerStepCreationEmbeddable.toEmbeddablePhaseTapChangerSteps(phaseTapChangerCreationInfos.getSteps()));
    }

    @Override
    public TwoWindingsTransformerCreationInfos toModificationInfos() {
        return toTwoWindingsTransformerCreationInfosBuilder().build();
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
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                // branch
                .r(getR())
                .x(getX())
                .voltageLevelId1(getVoltageLevelId1())
                .voltageLevelId2(getVoltageLevelId2())
                .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
                .busOrBusbarSectionId2(getBusOrBusbarSectionId2())
                .connectionName1(getConnectionName1())
                .connectionName2(getConnectionName2())
                .selectedOperationalLimitsGroup1(getSelectedOperationalLimitsGroupId1())
                .selectedOperationalLimitsGroup2(getSelectedOperationalLimitsGroupId2())
                .connectionDirection1(getConnectionDirection1())
                .connectionDirection2(getConnectionDirection2())
                .connectionPosition1(getConnectionPosition1())
                .connectionPosition2(getConnectionPosition2())
                .connected1(isConnected1())
                .connected2(isConnected2())
                // 2WT
                .g(getG())
                .b(getB())
                .ratedU1(getRatedU1())
                .ratedU2(getRatedU2())
                .ratedS(getRatedS())
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList())
                .operationalLimitsGroups1(OperationalLimitsGroupEntity.fromOperationalLimitsGroupsEntities(getOperationalLimitsGroups1()))
                .operationalLimitsGroups2(OperationalLimitsGroupEntity.fromOperationalLimitsGroupsEntities(getOperationalLimitsGroups2()));

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

        if (!phaseTapChangerSteps.isEmpty()) {
            List<TapChangerStepCreationInfos> phaseTapChangerStepCreationInfos = phaseTapChangerSteps.stream().map(TapChangerStepCreationEmbeddable::toModificationInfos).collect(Collectors.toList());
            builder.phaseTapChanger(PhaseTapChangerCreationInfos.builder()
                    .lowTapPosition(getPhaseTapChangerLowTapPosition())
                    .tapPosition(getPhaseTapChangerTapPosition())
                    .targetDeadband(getPhaseTapChangerTargetDeadband())
                    .regulating(getPhaseTapChangerRegulating())
                    // loadTapChangingCapabilities always true because in gridsuite the user can't change it
                    .loadTapChangingCapabilities(true)
                    .regulationMode(getPhaseTapChangerRegulationMode())
                    .regulationValue(getPhaseTapChangerRegulationValue())
                    .regulatingTerminalId(getPhaseTapChangerTerminalRefConnectableId())
                    .regulatingTerminalVlId(getPhaseTapChangerTerminalRefVoltageLevelId())
                    .regulatingTerminalType(getPhaseTapChangerTerminalRefType())
                    .steps(phaseTapChangerStepCreationInfos)
                    .build());
        }

        return builder;
    }
}
