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
import com.powsybl.iidm.network.PhaseTapChangerAdder;
import com.powsybl.iidm.network.RatioTapChanger;
import com.powsybl.iidm.network.RatioTapChangerAdder;
import com.powsybl.iidm.network.TapChanger;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TwoWindingsTransformer;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchModificationInfos;
import org.gridsuite.modification.server.dto.RatioTapChangerModificationInfos;
import org.gridsuite.modification.server.dto.TapChangerModificationInfos;
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

    private void addStepAttributeReport(List<Report> tapChangerStepsReports, String key, String defaultMessage,
            String valueKey, String value) {
        tapChangerStepsReports.add(Report.builder().withKey(key)
                .withDefaultMessage(defaultMessage)
                .withValue(valueKey, value)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private <T> void addTapchangerSteps(List<Report> tapChangerStepsReports, TapChangerModificationInfos tapChangerModificationInfos, T adder) {
        tapChangerStepsReports.add(Report.builder().withKey("tapChangerStepsModification")
                .withDefaultMessage("            Taps were replaced by new ones below")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        for (TapChangerStepCreationInfos step : tapChangerModificationInfos.getSteps()) {
            addStepAttributeReport(tapChangerStepsReports, "newStepIndex" + step.getIndex(),
                    "            Tap (${index})", "index", String.valueOf(step.getIndex()));
            addStepAttributeReport(tapChangerStepsReports, "newStepResistance" + step.getR(),
                    "                Δ resistance : ${r}", "r", String.valueOf(step.getR()));
            addStepAttributeReport(tapChangerStepsReports, "newStepReactance" + step.getX(),
                                    "                Δ reactance : ${x}", "x", String.valueOf(step.getX()));
            addStepAttributeReport(tapChangerStepsReports, "newStepConductance" + step.getX(),
                                    "                Δ conductance : ${g}", "g", String.valueOf(step.getG()));
            addStepAttributeReport(tapChangerStepsReports, "newStepSusceptance" + step.getB(),
                                    "                Δ susceptance : ${b}", "b", String.valueOf(step.getB()));
            addStepAttributeReport(tapChangerStepsReports, "newStepRatio" + step.getRho(),
                                    "                Ratio : ${rho}", "rho", String.valueOf(step.getRho()));
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder).beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
            } else {
                addStepAttributeReport(tapChangerStepsReports, "newStepAlpha" + step.getAlpha(),
                                    "                Shift angle : ${alpha}", "alpha", String.valueOf(step.getAlpha()));
                ((PhaseTapChangerAdder) adder).beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
            }
        }
        if (adder instanceof RatioTapChangerAdder) {
            ((RatioTapChangerAdder) adder).add();
        } else {
            ((PhaseTapChangerAdder) adder).add();
        }
    }

    private <T> void addTapChanger(TapChangerModificationInfos tapChangerModificationInfos, TapChanger<?, ?> tapChanger, T adder, Reporter subReporter) {
        List<Report> tapChangerReports = new ArrayList<>();
        if (tapChangerModificationInfos.getLowTapPosition() != null && tapChangerModificationInfos.getLowTapPosition().getValue() != null) {
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(tapChanger.getLowTapPosition(),
                tapChangerModificationInfos.getLowTapPosition().getValue(), "Low tap position", 2));
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setLowTapPosition(tapChangerModificationInfos.getLowTapPosition().getValue());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setLowTapPosition(tapChangerModificationInfos.getLowTapPosition().getValue());
            }
        }
        if (tapChangerModificationInfos.getTapPosition() != null && tapChangerModificationInfos.getTapPosition().getValue() != null) {
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(tapChanger.getTapPosition(),
                tapChangerModificationInfos.getTapPosition().getValue(), "Tap position", 2));
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
            }
        }
        if (tapChangerModificationInfos.getSteps() != null) {
            tapChangerReports.add(Report.builder()
                .withKey("tapsModification")
                .withDefaultMessage("        Taps")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
            addTapchangerSteps(tapChangerReports, tapChangerModificationInfos, adder);
        }
        if (!tapChangerReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReporter, tapChangerReports, "tapChangerModification", "    Tap changer");
        }
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        Reporter ratioTapChangerReporter = subReporter.createSubReporter("ratioTapChangerModification", "Ratio tap changer");
        ratioTapChangerReporter.report(Report.builder()
                    .withKey("ratioTapChangerModification")
                    .withDefaultMessage("Ratio tap changer")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        RatioTapChangerModificationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        RatioTapChanger ratioTapChanger = twt.getRatioTapChanger();

        /*boolean loadTapChangingCapabilities = twt.getRatioTapChanger().hasLoadTapChangingCapabilities();
        if (ratioTapChangerInfos.getLoadTapChangingCapabilities() != null) {
            loadTapChangingCapabilities = ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue();
        }*/

        List<Report> voltageRegulationReports = new ArrayList<>();
        if (ratioTapChangerInfos.getTargetV() != null && ratioTapChangerInfos.getTargetV().getValue() != null) {
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(ratioTapChanger.getTargetV(),
                ratioTapChangerInfos.getTargetV().getValue(), "Voltage", 2));
            ratioTapChangerAdder.setTargetV(ratioTapChangerInfos.getTargetV().getValue());
        }

        if (ratioTapChangerInfos.getTargetDeadband() != null && ratioTapChangerInfos.getTargetDeadband().getValue() != null) {
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(ratioTapChanger.getTargetDeadband(),
                ratioTapChangerInfos.getTargetDeadband().getValue(), "Target deadband", 2));
            ratioTapChangerAdder.setTargetDeadband(ratioTapChangerInfos.getTargetDeadband().getValue());
        }

        Terminal regulatingTerminal = ratioTapChanger.getRegulationTerminal();

        String oldVoltageLevel = null;
        String oldEquipment = null;
        // If there is no regulating terminal in file, regulating terminal voltage level
        // is equal to generator voltage level
        if (regulatingTerminal != null) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }
        if (ratioTapChangerInfos.getRegulatingTerminalId() != null
                && ratioTapChangerInfos.getRegulatingTerminalType() != null
                && ratioTapChangerInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                    ratioTapChangerInfos.getRegulatingTerminalId().getValue(),
                    ratioTapChangerInfos.getRegulatingTerminalType().getValue(),
                    ratioTapChangerInfos.getRegulatingTerminalVlId().getValue());
            ratioTapChangerAdder.setRegulationTerminal(terminal);

            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldVoltageLevel,
                    ratioTapChangerInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level", 2));
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldEquipment,
                    ratioTapChangerInfos.getRegulatingTerminalType().getValue() + ":"
                            + ratioTapChangerInfos.getRegulatingTerminalId().getValue(),
                    "Equipment", 2));
        }

        if (!voltageRegulationReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, voltageRegulationReports, "ratioTapChangerVoltageRegulationModification", "    Voltage regulation");
        }
        addTapChanger(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, ratioTapChangerReporter);
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
