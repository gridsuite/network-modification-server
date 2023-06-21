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
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.RatioTapChanger;
import com.powsybl.iidm.network.RatioTapChangerAdder;
import com.powsybl.iidm.network.RatioTapChangerStep;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TwoWindingsTransformer;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchModificationInfos;
import org.gridsuite.modification.server.dto.RatioTapChangerCreationInfos;
import org.gridsuite.modification.server.dto.TapChangerStepCreationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public class TwoWindingsTransformerModification extends AbstractBranchModification {

    public TwoWindingsTransformerModification(TwoWindingsTransformerModificationInfos modificationInfos) {
        super(modificationInfos);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getTwoWindingsTransformer(modificationInfos.getEquipmentId()) == null) {
            throw new NetworkModificationException(TWO_WINDINGS_TRANSFORMER_NOT_FOUND,
                    "Two windings transformer with ID '" + modificationInfos.getEquipmentId() + "' does not exist in the network");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        TwoWindingsTransformer twoWindingsTransformer = network.getTwoWindingsTransformer(modificationInfos.getEquipmentId());
        // modify the 2wt in the network
        modifyTwoWindingsTransformer(twoWindingsTransformer, modificationInfos, subReporter, network);
    }

    private void modifyTwoWindingsTransformer(TwoWindingsTransformer twoWindingsTransformer, BranchModificationInfos twoWindingsTransformerModificationInfos, Reporter subReporter, Network network) {
        modifyBranch(twoWindingsTransformer, twoWindingsTransformerModificationInfos, subReporter, "twoWindingsTransformerModification", "TwoWindingsTransformer with id=${id} modified :");
        addTapChangersToTwoWindingsTransformer(network, (TwoWindingsTransformerModificationInfos) modificationInfos, twoWindingsTransformer, subReporter);
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos, Reporter subReporter) {
        TwoWindingsTransformer twoWindingsTransformer = (TwoWindingsTransformer) branch;
        Reporter characteristicsReporter = subReporter.createSubReporter("characteristics", "Characteristics");
        characteristicsReporter.report(Report.builder()
                .withKey("characteristicsModification")
                .withDefaultMessage("Characteristics")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        // Branch specific fields
        if (branchModificationInfos.getSeriesResistance() != null && branchModificationInfos.getSeriesResistance().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getR(),
                    branchModificationInfos.getSeriesResistance().getValue(), "Series resistance", 1));
            twoWindingsTransformer.setR(branchModificationInfos.getSeriesResistance().getValue());
        }
        if (branchModificationInfos.getSeriesReactance() != null && branchModificationInfos.getSeriesReactance().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getX(),
                    branchModificationInfos.getSeriesReactance().getValue(), "Series reactance", 1));
            twoWindingsTransformer.setX(branchModificationInfos.getSeriesReactance().getValue());
        }

        // Transformer specific fields
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) branchModificationInfos;
        if (twoWindingsTransformerModificationInfos.getMagnetizingConductance() != null && twoWindingsTransformerModificationInfos.getMagnetizingConductance().getValue() != null) {
            // convert reported value from siemens to microsiemens
            double oldMagnetizingConductanceToReport = twoWindingsTransformer.getG() * Math.pow(10, 6);
            double newMagnetizingConductanceToReport = twoWindingsTransformerModificationInfos.getMagnetizingConductance().getValue() * Math.pow(10, 6);
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldMagnetizingConductanceToReport,
                    newMagnetizingConductanceToReport, "Magnetizing conductance", 1));
            twoWindingsTransformer.setG(twoWindingsTransformerModificationInfos.getMagnetizingConductance().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getMagnetizingSusceptance() != null && twoWindingsTransformerModificationInfos.getMagnetizingSusceptance().getValue() != null) {
            // convert reported value from siemens to microsiemens
            double oldMagnetizingSusceptanceToReport = twoWindingsTransformer.getB() * Math.pow(10, 6);
            double newMagnetizingSusceptanceToReport = twoWindingsTransformerModificationInfos.getMagnetizingSusceptance().getValue() * Math.pow(10, 6);
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldMagnetizingSusceptanceToReport,
                    newMagnetizingSusceptanceToReport, "Magnetizing susceptance", 1));
            twoWindingsTransformer.setB(twoWindingsTransformerModificationInfos.getMagnetizingSusceptance().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getRatedS() != null && twoWindingsTransformerModificationInfos.getRatedS().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatedS(),
                    twoWindingsTransformerModificationInfos.getRatedS().getValue(), "Rated nominal power", 1));
            twoWindingsTransformer.setRatedS(twoWindingsTransformerModificationInfos.getRatedS().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getRatedVoltage1() != null && twoWindingsTransformerModificationInfos.getRatedVoltage1().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatedU1(),
                    twoWindingsTransformerModificationInfos.getRatedVoltage1().getValue(), "Rated Voltage (Side 1)", 1));
            twoWindingsTransformer.setRatedU1(twoWindingsTransformerModificationInfos.getRatedVoltage1().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getRatedVoltage2() != null && twoWindingsTransformerModificationInfos.getRatedVoltage2().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatedU2(),
                    twoWindingsTransformerModificationInfos.getRatedVoltage2().getValue(), "Rated Voltage (Side 2)", 1));
            twoWindingsTransformer.setRatedU2(twoWindingsTransformerModificationInfos.getRatedVoltage2().getValue());
        }
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, com.powsybl.iidm.network.TwoWindingsTransformer twt, Reporter subReporter) {
        if (twoWindingsTransformerModificationInfos.getRatioTapChanger() != null) {
            addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerModificationInfos, twt, subReporter);
        }
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        List<Report> tapChangerReports = new ArrayList<>();
        RatioTapChanger ratioTapChanger = twt.getRatioTapChanger();
        RatioTapChangerCreationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos.getRatioTapChanger();
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

        boolean loadTapChangingCapabilities = twt.getRatioTapChanger().hasLoadTapChangingCapabilities();
        if (ratioTapChangerInfos.getLoadTapChangingCapabilities() != null) {
            loadTapChangingCapabilities = ratioTapChangerInfos.getLoadTapChangingCapabilities();
        }
        ratioTapChangerAdder.setRegulating(ratioTapChangerInfos.isRegulating())
                .setLoadTapChangingCapabilities(loadTapChangingCapabilities)
                .setLowTapPosition(ratioTapChangerInfos.getLowTapPosition())
                .setTapPosition(ratioTapChangerInfos.getTapPosition());

        if (ratioTapChangerInfos.getSteps() != null) {
            for (TapChangerStepCreationInfos step : ratioTapChangerInfos.getSteps()) {
                RatioTapChangerStep stepToModify = null;
                int index = 0;
                if (ratioTapChanger != null) {
                    if (ratioTapChanger.getLowTapPosition() < ratioTapChangerInfos.getLowTapPosition() || ratioTapChanger.getLowTapPosition() > ratioTapChangerInfos.getLowTapPosition()) {
                        index = ratioTapChanger.getLowTapPosition() - ratioTapChangerInfos.getLowTapPosition();
                    }
                    stepToModify = ratioTapChanger.getStep(step.getIndex() + index);
                }
                if (stepToModify == null) {
                    tapChangerReports.add(Report.builder().withKey("stepAdded" + step.getIndex())
                        .withDefaultMessage("            Tap (${index}) added")
                        .withValue("index", step.getIndex() + index)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
                } else if (stepToModify.getR() != step.getR() || stepToModify.getX() != step.getX() || stepToModify.getG() != step.getG() || stepToModify.getB() != step.getB() || stepToModify.getRho() != step.getRho()) {
                    tapChangerReports.add(Report.builder().withKey("stepModified" + step.getIndex())
                        .withDefaultMessage("            Tap (${index}) modified")
                        .withValue("index", step.getIndex() + index)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
                }
                ratioTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
            }

            ratioTapChangerAdder.add();
        }

        if (!tapChangerReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReporter, tapChangerReports, "ratioTapChangerModification", "Ratio Tap Changer");
        }
    }

    @Override
    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) branchModificationInfos;
        return super.characteristicsModified(branchModificationInfos)
                || twoWindingsTransformerModificationInfos.getMagnetizingConductance() != null
                && twoWindingsTransformerModificationInfos.getMagnetizingConductance().getValue() != null
                || twoWindingsTransformerModificationInfos.getMagnetizingSusceptance() != null
                && twoWindingsTransformerModificationInfos.getMagnetizingSusceptance().getValue() != null
                || twoWindingsTransformerModificationInfos.getRatedVoltage1() != null
                && twoWindingsTransformerModificationInfos.getRatedVoltage1().getValue() != null
                || twoWindingsTransformerModificationInfos.getRatedVoltage2() != null
                && twoWindingsTransformerModificationInfos.getRatedVoltage2().getValue() != null
                || twoWindingsTransformerModificationInfos.getRatedS() != null
                && twoWindingsTransformerModificationInfos.getRatedS().getValue() != null;
    }
}
