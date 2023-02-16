/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBays;
import com.powsybl.iidm.modification.topology.CreateBranchFeederBaysBuilder;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.dto.*;

import java.util.Optional;

public class TwoWindingsTransformerCreation extends AbstractModification {

    private final TwoWindingsTransformerCreationInfos modificationInfos;

    public TwoWindingsTransformerCreation(TwoWindingsTransformerCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // create the 2wt in the network
        VoltageLevel voltageLevel1 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId1());
        VoltageLevel voltageLevel2 = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId2());
        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER && voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            create2WTInNodeBreaker(network, voltageLevel1, voltageLevel2, subReporter);
        } else {
            // Create 2wt in bus/mixed breaker
            create2WTInOtherBreaker(network, voltageLevel1, voltageLevel2, modificationInfos, true, true, subReporter);
        }

    }

    private void create2WTInNodeBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, Reporter subReporter) {
        var twoWindingsTransformerAdder = createTwoWindingsTransformerAdder(network, voltageLevel1, voltageLevel2, modificationInfos, false, false);

        var position1 = modificationInfos.getConnectionPosition1() != null ? modificationInfos.getConnectionPosition1() : ModificationUtils.getInstance().getPosition(modificationInfos.getBusOrBusbarSectionId1(), network, voltageLevel1);
        var position2 = modificationInfos.getConnectionPosition2() != null ? modificationInfos.getConnectionPosition2() : ModificationUtils.getInstance().getPosition(modificationInfos.getBusOrBusbarSectionId2(), network, voltageLevel2);

        CreateBranchFeederBays algo = new CreateBranchFeederBaysBuilder()
                .withBusOrBusbarSectionId1(modificationInfos.getBusOrBusbarSectionId1())
                .withBusOrBusbarSectionId2(modificationInfos.getBusOrBusbarSectionId2())
                .withFeederName1(modificationInfos.getConnectionName1() != null ? modificationInfos.getConnectionName1() : modificationInfos.getEquipmentId())
                .withFeederName2(modificationInfos.getConnectionName2() != null ? modificationInfos.getConnectionName2() : modificationInfos.getEquipmentId())
                .withDirection1(modificationInfos.getConnectionDirection1())
                .withDirection2(modificationInfos.getConnectionDirection2())
                .withPositionOrder1(position1)
                .withPositionOrder2(position2)
                .withBranchAdder(twoWindingsTransformerAdder).build();
        algo.apply(network, true, subReporter);

        var twt = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        addTapChangersToTwoWindingsTransformer(network, modificationInfos, twt);
    }

    private TwoWindingsTransformerAdder createTwoWindingsTransformerAdder(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2) {
        Optional<Substation> optS1 = voltageLevel1.getSubstation();
        Optional<Substation> optS2 = voltageLevel2.getSubstation();
        Substation s1 = optS1.orElse(null);
        Substation s2 = optS2.orElse(null);
        BranchAdder<TwoWindingsTransformerAdder> branchAdder;

        if (s1 != null) {
            branchAdder = s1.newTwoWindingsTransformer();
        } else if (s2 != null) {
            branchAdder = s2.newTwoWindingsTransformer();
        } else {
            branchAdder = network.newTwoWindingsTransformer();
        }
        // common settings
        TwoWindingsTransformerAdder twoWindingsTransformerAdder = branchAdder.setId(twoWindingsTransformerCreationInfos.getEquipmentId())
                .setName(twoWindingsTransformerCreationInfos.getEquipmentName())
                .setVoltageLevel1(twoWindingsTransformerCreationInfos.getVoltageLevelId1())
                .setVoltageLevel2(twoWindingsTransformerCreationInfos.getVoltageLevelId2())
                .setG(twoWindingsTransformerCreationInfos.getMagnetizingConductance())
                .setB(twoWindingsTransformerCreationInfos.getMagnetizingSusceptance())
                .setR(twoWindingsTransformerCreationInfos.getSeriesResistance())
                .setX(twoWindingsTransformerCreationInfos.getSeriesReactance())
                .setRatedU1(twoWindingsTransformerCreationInfos.getRatedVoltage1())
                .setRatedU2(twoWindingsTransformerCreationInfos.getRatedVoltage2());

        if (twoWindingsTransformerCreationInfos.getRatedS() != null) {
            twoWindingsTransformerAdder.setRatedS(twoWindingsTransformerCreationInfos.getRatedS());
        }

        // BranchAdder completion by topology
        ModificationUtils.getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel1, twoWindingsTransformerCreationInfos, Branch.Side.ONE, withSwitch1);
        ModificationUtils.getInstance().setBranchAdderNodeOrBus(branchAdder, voltageLevel2, twoWindingsTransformerCreationInfos, Branch.Side.TWO, withSwitch2);

        return twoWindingsTransformerAdder;
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, com.powsybl.iidm.network.TwoWindingsTransformer twt) {
        if (twoWindingsTransformerCreationInfos.getRatioTapChanger() != null) {
            addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt);
        }

        if (twoWindingsTransformerCreationInfos.getPhaseTapChanger() != null) {
            addPhaseTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt);
        }
    }

    private void addPhaseTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt) {
        PhaseTapChangerCreationInfos phaseTapChangerInfos = twoWindingsTransformerCreationInfos.getPhaseTapChanger();
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                phaseTapChangerInfos.getRegulatingTerminalId(),
                phaseTapChangerInfos.getRegulatingTerminalType(),
                phaseTapChangerInfos.getRegulatingTerminalVlId());

        if (phaseTapChangerInfos.isRegulating()) {
            phaseTapChangerAdder.setRegulationValue(phaseTapChangerInfos.getRegulationValue())
                    .setTargetDeadband(phaseTapChangerInfos.getTargetDeadband() != null ? phaseTapChangerInfos.getTargetDeadband() : 0.)
                    .setRegulationTerminal(terminal);
        }

        phaseTapChangerAdder.setRegulating(phaseTapChangerInfos.isRegulating())
                .setRegulationMode(phaseTapChangerInfos.getRegulationMode())
                .setLowTapPosition(phaseTapChangerInfos.getLowTapPosition())
                .setTapPosition(phaseTapChangerInfos.getTapPosition());

        if (phaseTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : phaseTapChangerInfos.getSteps()) {
                phaseTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
            }

            phaseTapChangerAdder.add();
        }
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, TwoWindingsTransformer twt) {
        RatioTapChangerCreationInfos ratioTapChangerInfos = twoWindingsTransformerCreationInfos.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                ratioTapChangerInfos.getRegulatingTerminalId(),
                ratioTapChangerInfos.getRegulatingTerminalType(),
                ratioTapChangerInfos.getRegulatingTerminalVlId());

        if (ratioTapChangerInfos.isRegulating()) {
            ratioTapChangerAdder.setTargetV(ratioTapChangerInfos.getTargetV())
                    .setTargetDeadband(ratioTapChangerInfos.getTargetDeadband() != null ? ratioTapChangerInfos.getTargetDeadband() : 0.)
                    .setRegulationTerminal(terminal);
        }

        ratioTapChangerAdder.setRegulating(ratioTapChangerInfos.isRegulating())
                .setLoadTapChangingCapabilities(ratioTapChangerInfos.isLoadTapChangingCapabilities())
                .setLowTapPosition(ratioTapChangerInfos.getLowTapPosition())
                .setTapPosition(ratioTapChangerInfos.getTapPosition());

        if (ratioTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : ratioTapChangerInfos.getSteps()) {
                ratioTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
            }

            ratioTapChangerAdder.add();
        }
    }

    private void create2WTInOtherBreaker(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, TwoWindingsTransformerCreationInfos twoWindingsTransformerCreationInfos, boolean withSwitch1, boolean withSwitch2, Reporter subReporter) {
        var twt = createTwoWindingsTransformerAdder(network, voltageLevel1, voltageLevel2, twoWindingsTransformerCreationInfos, withSwitch1, withSwitch2).add();
        addTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerCreationInfos, twt);
        subReporter.report(Report.builder()
                .withKey("twoWindingsTransformerCreated")
                .withDefaultMessage("New two windings transformer with id=${id} created")
                .withValue("id", twoWindingsTransformerCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

}
