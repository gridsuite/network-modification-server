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
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.TapChangerType;
import org.gridsuite.modification.server.dto.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TWO_WINDINGS_TRANSFORMER_NOT_FOUND;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public class TwoWindingsTransformerModification extends AbstractBranchModification {

    private static final String RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE = "Ratio tap changer";
    private static final String PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE = "Phase tap changer";

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
        PropertiesUtils.applyProperties(twoWindingsTransformer, subReporter, modificationInfos.getProperties());
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
        if (branchModificationInfos.getR() != null && branchModificationInfos.getR().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getR(),
                    branchModificationInfos.getR().getValue(), "Series resistance", 1));
            twoWindingsTransformer.setR(branchModificationInfos.getR().getValue());
        }
        if (branchModificationInfos.getX() != null && branchModificationInfos.getX().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getX(),
                    branchModificationInfos.getX().getValue(), "Series reactance", 1));
            twoWindingsTransformer.setX(branchModificationInfos.getX().getValue());
        }

        // Transformer specific fields
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) branchModificationInfos;
        if (twoWindingsTransformerModificationInfos.getG() != null && twoWindingsTransformerModificationInfos.getG().getValue() != null) {
            // convert reported value from siemens to microsiemens
            double oldMagnetizingConductanceToReport = twoWindingsTransformer.getG() * Math.pow(10, 6);
            double newMagnetizingConductanceToReport = twoWindingsTransformerModificationInfos.getG().getValue() * Math.pow(10, 6);
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldMagnetizingConductanceToReport,
                    newMagnetizingConductanceToReport, "Magnetizing conductance", 1));
            twoWindingsTransformer.setG(twoWindingsTransformerModificationInfos.getG().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getB() != null && twoWindingsTransformerModificationInfos.getB().getValue() != null) {
            // convert reported value from siemens to microsiemens
            double oldMagnetizingSusceptanceToReport = twoWindingsTransformer.getB() * Math.pow(10, 6);
            double newMagnetizingSusceptanceToReport = twoWindingsTransformerModificationInfos.getB().getValue() * Math.pow(10, 6);
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldMagnetizingSusceptanceToReport,
                    newMagnetizingSusceptanceToReport, "Magnetizing susceptance", 1));
            twoWindingsTransformer.setB(twoWindingsTransformerModificationInfos.getB().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getRatedS() != null && twoWindingsTransformerModificationInfos.getRatedS().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatedS(),
                    twoWindingsTransformerModificationInfos.getRatedS().getValue(), "Rated nominal power", 1));
            twoWindingsTransformer.setRatedS(twoWindingsTransformerModificationInfos.getRatedS().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getRatedU1() != null && twoWindingsTransformerModificationInfos.getRatedU1().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatedU1(),
                    twoWindingsTransformerModificationInfos.getRatedU1().getValue(), "Rated Voltage (Side 1)", 1));
            twoWindingsTransformer.setRatedU1(twoWindingsTransformerModificationInfos.getRatedU1().getValue());
        }
        if (twoWindingsTransformerModificationInfos.getRatedU2() != null && twoWindingsTransformerModificationInfos.getRatedU2().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(twoWindingsTransformer.getRatedU2(),
                    twoWindingsTransformerModificationInfos.getRatedU2().getValue(), "Rated Voltage (Side 2)", 1));
            twoWindingsTransformer.setRatedU2(twoWindingsTransformerModificationInfos.getRatedU2().getValue());
        }
    }

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        if (twt.hasRatioTapChanger() && twoWindingsTransformerModificationInfos.getRatioTapChanger().getEnabled() != null && Boolean.FALSE.equals(twoWindingsTransformerModificationInfos.getRatioTapChanger().getEnabled().getValue())) {
            twt.getRatioTapChanger().remove();
            subReporter.report(Report.builder().withKey("RatioTapChangerRemoved")
                .withDefaultMessage("The ratio tap changer has been removed")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        } else if (ratioTapChangerModified(twoWindingsTransformerModificationInfos.getRatioTapChanger())) {
            processRatioTapChanger(network, twoWindingsTransformerModificationInfos, twt, subReporter, twt.hasRatioTapChanger());
        }

        if (twt.hasPhaseTapChanger() && twoWindingsTransformerModificationInfos.getPhaseTapChanger().getEnabled() != null && Boolean.FALSE.equals(twoWindingsTransformerModificationInfos.getPhaseTapChanger().getEnabled().getValue())) {
            twt.getPhaseTapChanger().remove();
            subReporter.report(Report.builder().withKey("PhaseTapChangerRemoved")
                .withDefaultMessage("The phase tap changer has been removed")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        } else if (phaseTapChangerModified(twoWindingsTransformerModificationInfos.getPhaseTapChanger())) {
            processPhaseTapChanger(network, twoWindingsTransformerModificationInfos, twt, subReporter, twt.hasPhaseTapChanger());
        }
    }

    private void processPhaseTapChanger(Network network,
            TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos,
            TwoWindingsTransformer twt,
            Reporter subReporter,
            boolean isModification) {
        PhaseTapChanger phaseTapChanger = isModification ? twt.getPhaseTapChanger() : null;
        PhaseTapChangerAdder phaseTapChangerAdder = isModification ? null : twt.newPhaseTapChanger();
        PhaseTapChangerModificationInfos phaseTapChangerInfos = twoWindingsTransformerModificationInfos
                .getPhaseTapChanger();

        List<Report> phaseTapChangerReports = new ArrayList<>();
        phaseTapChangerReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? phaseTapChanger::setRegulationMode : phaseTapChangerAdder::setRegulationMode,
                isModification ? phaseTapChanger::getRegulationMode : () -> null,
                phaseTapChangerInfos.getRegulationMode(), "Regulation mode", 1));

        List<Report> regulationReports = new ArrayList<>();
        PhaseTapChanger.RegulationMode regulationMode = isModification ? phaseTapChanger.getRegulationMode() : null;
        if (phaseTapChangerInfos.getRegulationMode() != null
                && phaseTapChangerInfos.getRegulationMode().getValue() != null) {
            regulationMode = phaseTapChangerInfos.getRegulationMode().getValue();
        }
        if (!PhaseTapChanger.RegulationMode.FIXED_TAP.equals(regulationMode)) {
            processPhaseTapRegulation(phaseTapChangerInfos, phaseTapChanger, phaseTapChangerAdder, regulationReports,
                    regulationMode, isModification);
        }

        processRegulatingTerminal(phaseTapChangerInfos, phaseTapChanger, phaseTapChangerAdder, regulationReports,
                network,
                twt, isModification);

        List<Report> positionsAndStepsReports = new ArrayList<>();
        processTapChangerPositionsAndSteps(phaseTapChangerInfos, phaseTapChanger, phaseTapChangerAdder, positionsAndStepsReports,
                isModification);

        if (!isModification) {
            phaseTapChangerAdder.add();
        }

        if (!phaseTapChangerReports.isEmpty() || !regulationReports.isEmpty() || !positionsAndStepsReports.isEmpty()) {
            Reporter phaseTapChangerSubreporter = ModificationUtils.getInstance().reportModifications(subReporter,
                    phaseTapChangerReports, TapChangerType.PHASE.name(), PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE,
                    Map.of());
            if (phaseTapChangerSubreporter == null) {
                phaseTapChangerSubreporter = subReporter.createSubReporter(TapChangerType.PHASE.name(),
                        PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE);
                phaseTapChangerSubreporter.report(Report.builder()
                        .withKey(TapChangerType.PHASE.name())
                        .withDefaultMessage(PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(phaseTapChangerSubreporter, regulationReports,
                    regulationMode != null ? regulationMode.name() : null,
                    "${regulationMode}",
                    Map.of("regulationMode", ModificationUtils.getInstance().formatRegulationModeReport(regulationMode)));
            ModificationUtils.getInstance().reportModifications(phaseTapChangerSubreporter, positionsAndStepsReports,
                    "phaseTapChangerPositionsAndStepsModification", "    Tap Changer", Map.of());
        }
    }

    private void processPhaseTapRegulation(PhaseTapChangerModificationInfos phaseTapChangerInfos,
            PhaseTapChanger phaseTapChanger,
            PhaseTapChangerAdder phaseTapChangerAdder,
            List<Report> regulationReports,
            PhaseTapChanger.RegulationMode regulationMode,
            boolean isModification) {

        if (regulationMode != null) {
            regulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                    isModification ? phaseTapChanger::setRegulationValue
                            : phaseTapChangerAdder::setRegulationValue,
                    isModification ? phaseTapChanger::getRegulationValue : () -> null,
                    phaseTapChangerInfos.getRegulationValue(),
                    regulationMode.equals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER) ? "Value" : "Flow set point",
                    2));
        }

        regulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? phaseTapChanger::setTargetDeadband
                        : phaseTapChangerAdder::setTargetDeadband,
                isModification ? phaseTapChanger::getTargetDeadband : () -> null,
                phaseTapChangerInfos.getTargetDeadband(), "Target deadband", 2));

        if (isModification) {
            phaseTapChanger.setRegulating(true);
        } else {
            phaseTapChangerAdder.setRegulating(true);
        }
    }

    private void processRatioTapChanger(Network network,
            TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos,
            TwoWindingsTransformer twt,
            Reporter subReporter,
            boolean isModification) {
        RatioTapChangerModificationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos
                .getRatioTapChanger();
        RatioTapChanger ratioTapChanger = isModification ? twt.getRatioTapChanger() : null;
        RatioTapChangerAdder ratioTapChangerAdder = isModification ? null : twt.newRatioTapChanger();
        List<Report> ratioTapChangerReports = new ArrayList<>();
        ratioTapChangerReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? ratioTapChanger::setLoadTapChangingCapabilities
                        : ratioTapChangerAdder::setLoadTapChangingCapabilities,
                isModification ? ratioTapChanger::hasLoadTapChangingCapabilities : () -> null,
                ratioTapChangerInfos.getLoadTapChangingCapabilities(), "Load tap changing capabilities", 1));
        processRegulating(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, ratioTapChangerReports, isModification);

        List<Report> voltageRegulationReports = new ArrayList<>();
        processRatioVoltageRegulation(ratioTapChangerInfos, twt, ratioTapChanger, ratioTapChangerAdder, voltageRegulationReports, network,
                isModification);
        List<Report> positionsAndStepsReports = new ArrayList<>();
        processTapChangerPositionsAndSteps(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, positionsAndStepsReports,
                isModification);

        if (!isModification) {
            ratioTapChangerAdder.add();
        }

        if (!ratioTapChangerReports.isEmpty() || !voltageRegulationReports.isEmpty()
                || !positionsAndStepsReports.isEmpty()) {
            Reporter ratioTapChangerReporter = ModificationUtils.getInstance().reportModifications(subReporter,
                    ratioTapChangerReports, TapChangerType.RATIO.name(), RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE, Map.of());
            if (ratioTapChangerReporter == null) {
                ratioTapChangerReporter = subReporter.createSubReporter(TapChangerType.RATIO.name(),
                        RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE);
                ratioTapChangerReporter.report(Report.builder()
                        .withKey(TapChangerType.RATIO.name())
                        .withDefaultMessage(RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, voltageRegulationReports,
                    "ratioTapChangerVoltageRegulationModification", "    Voltage regulation", Map.of());
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, positionsAndStepsReports,
                    "ratioTapChangerPositionsAndStepsModification", "    Tap Changer", Map.of());
        }
    }

    private void processRegulating(RatioTapChangerModificationInfos ratioTapChangerInfos,
            RatioTapChanger ratioTapChanger, RatioTapChangerAdder ratioTapChangerAdder,
            List<Report> ratioTapChangerReports, boolean isModification) {
        if (ratioTapChangerInfos.getRegulating() != null && ratioTapChangerInfos.getRegulating().getValue() != null) {
            boolean regulating = ratioTapChangerInfos.getRegulating().getValue();
            ratioTapChangerReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                    isModification ? ratioTapChanger::setRegulating
                            : ratioTapChangerAdder::setRegulating,
                    isModification ? ratioTapChanger::isRegulating : () -> null,
                    ratioTapChangerInfos.getRegulating(), regulating ? "Voltage regulation" : "Fixed ratio", 1));
        }
    }

    private void processRatioVoltageRegulation(RatioTapChangerModificationInfos ratioTapChangerInfos,
            TwoWindingsTransformer twt,
            RatioTapChanger ratioTapChanger,
            RatioTapChangerAdder ratioTapChangerAdder,
            List<Report> voltageRegulationReports,
            Network network,
            boolean isModification) {
        voltageRegulationReports
                .add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                        isModification ? ratioTapChanger::setTargetV
                                : ratioTapChangerAdder::setTargetV,
                        isModification ? ratioTapChanger::getTargetV : () -> null,
                        ratioTapChangerInfos.getTargetV(), "Target voltage", 2));

        voltageRegulationReports
                .add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                        isModification ? ratioTapChanger::setTargetDeadband
                                : ratioTapChangerAdder::setTargetDeadband,
                        isModification ? ratioTapChanger::getTargetDeadband : () -> null,
                        ratioTapChangerInfos.getTargetDeadband(), "Target deadband", 2));

        processRegulatingTerminal(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, voltageRegulationReports,
                    network, twt, isModification);
    }

    private void processRegulatingTerminal(TapChangerModificationInfos tapChangerModificationInfos,
            TapChanger<?, ?, ?, ?> tapChanger,
            TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder,
            List<Report> regulationReports,
            Network network,
            TwoWindingsTransformer twt,
            boolean isModification) {
        String oldVoltageLevel = null;
        String oldEquipment = null;

        if (isModification && tapChanger.getRegulationTerminal() != null) {
            oldVoltageLevel = tapChanger.getRegulationTerminal().getVoltageLevel().getId();
            oldEquipment = tapChanger.getRegulationTerminal().getConnectable().getType()
                    .name() + ":"
                    + tapChanger.getRegulationTerminal().getConnectable().getId();
        }

        if (tapChangerModificationInfos.getRegulationSide() != null
                && tapChangerModificationInfos.getRegulationSide().getValue() != null) {
            Terminal terminal = tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE1
                    ? twt.getTerminal1()
                    : twt.getTerminal2();
            setRegulatingTerminalInfos(tapChangerModificationInfos, terminal);
        }

        if (tapChangerModificationInfos.getRegulatingTerminalId() != null
                && tapChangerModificationInfos.getRegulatingTerminalType() != null
                && tapChangerModificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                    tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue(),
                    tapChangerModificationInfos.getRegulatingTerminalVlId().getValue());
            if (isModification) {
                tapChanger.setRegulationTerminal(terminal);
            } else {
                tapChangerAdder.setRegulationTerminal(terminal);
            }
            regulationReports
                    .add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldVoltageLevel,
                            tapChangerModificationInfos.getRegulatingTerminalVlId().getValue(),
                            "Voltage level", 2));
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldEquipment,
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment", 2));
        }
    }

    private void setRegulatingTerminalInfos(TapChangerModificationInfos tapChangerModificationInfos, Terminal terminal) {
        tapChangerModificationInfos.setRegulatingTerminalVlId(new AttributeModification<>(terminal.getVoltageLevel().getId(), OperationType.SET));
        tapChangerModificationInfos.setRegulatingTerminalId(new AttributeModification<>(terminal.getConnectable().getId(), OperationType.SET));
        tapChangerModificationInfos.setRegulatingTerminalType(new AttributeModification<>(terminal.getConnectable().getType().name(), OperationType.SET));
    }

    private void processTapchangerSteps(List<Report> tapChangerStepsReports,
            TapChangerModificationInfos tapChangerModificationInfos,
            TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder,
            TapChangerStepsReplacer<?, ?> tapChangerStepReplacer,
            boolean isModification) {
        tapChangerStepsReports.add(Report.builder().withKey("tapChangerStepsModification")
                .withDefaultMessage("            Taps were replaced by new ones below")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        for (TapChangerStepCreationInfos step : tapChangerModificationInfos.getSteps()) {
            addStepAttributeReports(tapChangerStepsReports, step);
            if (tapChangerStepReplacer instanceof RatioTapChangerStepsReplacer || tapChangerAdder instanceof RatioTapChangerAdder) {
                if (isModification) {
                    tapChangerStepReplacer.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                            .setB(step.getB()).setRho(step.getRho()).endStep();
                } else {
                    tapChangerAdder.beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                            .setB(step.getB()).setRho(step.getRho()).endStep();
                }
            } else {
                addStepAttributeReport(tapChangerStepsReports, "newStepAlpha" + step.getAlpha(),
                        "                Shift angle : ${alpha}", "alpha", String.valueOf(step.getAlpha()));
                if (isModification) {
                    ((PhaseTapChangerStepsReplacer) tapChangerStepReplacer).beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                            .setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
                } else {
                    ((PhaseTapChangerAdder) tapChangerAdder).beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG())
                            .setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
                }
            }
        }
        if (isModification) {
            tapChangerStepReplacer.replaceSteps();
        }
    }

    private void addStepAttributeReports(List<Report> tapChangerStepsReports, TapChangerStepCreationInfos step) {
        addStepAttributeReport(tapChangerStepsReports, "newStepIndex" + step.getIndex(),
                "            Tap (${index})", "index", String.valueOf(step.getIndex()));
        addStepAttributeReport(tapChangerStepsReports, "newStepResistance" + step.getR(),
                "                Δ resistance : ${r}", "r", String.valueOf(step.getR()));
        addStepAttributeReport(tapChangerStepsReports, "newStepReactance" + step.getX(),
                "                Δ reactance : ${x}", "x", String.valueOf(step.getX()));
        addStepAttributeReport(tapChangerStepsReports, "newStepConductance" + step.getG(),
                "                Δ conductance : ${g}", "g", String.valueOf(step.getG()));
        addStepAttributeReport(tapChangerStepsReports, "newStepSusceptance" + step.getB(),
                "                Δ susceptance : ${b}", "b", String.valueOf(step.getB()));
        addStepAttributeReport(tapChangerStepsReports, "newStepRatio" + step.getRho(),
                "                Ratio : ${rho}", "rho", String.valueOf(step.getRho()));
    }

    private void processTapChangerPositionsAndSteps(TapChangerModificationInfos tapChangerModificationInfos,
            TapChanger<?, ?, ?, ?> tapChanger,
            TapChangerAdder<?, ?, ?, ?, ?, ?> tapChangerAdder,
            List<Report> tapChangerReports,
            boolean isModification) {
        tapChangerReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? tapChanger::setLowTapPosition
                        : tapChangerAdder::setLowTapPosition,
                isModification ? tapChanger::getLowTapPosition : () -> null,
                tapChangerModificationInfos.getLowTapPosition(), "Low tap position", 2));

        tapChangerReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(
                isModification ? tapChanger::setTapPosition
                        : tapChangerAdder::setTapPosition,
                isModification ? tapChanger::getTapPosition : () -> null,
                tapChangerModificationInfos.getTapPosition(), "Tap position", 2));

        // Add steps
        if (tapChangerModificationInfos.getSteps() != null) {
            tapChangerReports.add(Report.builder()
                    .withKey("tapsModification")
                    .withDefaultMessage("        Taps")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            processTapchangerSteps(tapChangerReports, tapChangerModificationInfos,
                    tapChangerAdder, isModification ? tapChanger.stepsReplacer() : null, isModification);
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

    private boolean ratioTapChangerModified(RatioTapChangerModificationInfos ratioTapChangerModificationInfos) {
        return ratioTapChangerModificationInfos != null && (
                ratioTapChangerModificationInfos.getLoadTapChangingCapabilities() != null
                && ratioTapChangerModificationInfos.getLoadTapChangingCapabilities().getValue() != null
                || ratioTapChangerModificationInfos.getTargetV() != null
                && ratioTapChangerModificationInfos.getTargetV().getValue() != null
                || commonTapChangerAttributesModified(ratioTapChangerModificationInfos));
    }

    private boolean phaseTapChangerModified(PhaseTapChangerModificationInfos phaseTapChangerModificationInfos) {
        return phaseTapChangerModificationInfos != null && (
                phaseTapChangerModificationInfos.getRegulationMode() != null
                && phaseTapChangerModificationInfos.getRegulationMode().getValue() != null
                || phaseTapChangerModificationInfos.getRegulationValue() != null
                && phaseTapChangerModificationInfos.getRegulationValue().getValue() != null
                || commonTapChangerAttributesModified(phaseTapChangerModificationInfos));
    }

    private boolean commonTapChangerAttributesModified(TapChangerModificationInfos tapChangerModificationInfos) {
        return tapChangerModificationInfos != null && (
                tapChangerModificationInfos.getRegulating() != null
                && tapChangerModificationInfos.getRegulating().getValue() != null
                || tapChangerModificationInfos.getRegulationType() != null
                && tapChangerModificationInfos.getRegulationType().getValue() != null
                || tapChangerModificationInfos.getRegulationSide() != null
                && tapChangerModificationInfos.getRegulationSide().getValue() != null
                || tapChangerModificationInfos.getRegulatingTerminalId() != null
                && tapChangerModificationInfos.getRegulatingTerminalId().getValue() != null
                || tapChangerModificationInfos.getRegulatingTerminalType() != null
                && tapChangerModificationInfos.getRegulatingTerminalType().getValue() != null
                || tapChangerModificationInfos.getRegulatingTerminalVlId() != null
                && tapChangerModificationInfos.getRegulatingTerminalVlId().getValue() != null
                || tapChangerModificationInfos.getTargetDeadband() != null
                && tapChangerModificationInfos.getTargetDeadband().getValue() != null
                || positionsAndStepsModified(tapChangerModificationInfos));
    }

    private boolean positionsAndStepsModified(TapChangerModificationInfos tapChangerModificationInfos) {
        return tapChangerModificationInfos.getTapPosition() != null
            && tapChangerModificationInfos.getTapPosition().getValue() != null
            || tapChangerModificationInfos.getLowTapPosition() != null
            && tapChangerModificationInfos.getLowTapPosition().getValue() != null
            || tapChangerModificationInfos.getSteps() != null;
    }

    @Override
    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos = (TwoWindingsTransformerModificationInfos) branchModificationInfos;
        return super.characteristicsModified(branchModificationInfos)
            || twoWindingsTransformerModificationInfos.getG() != null
            && twoWindingsTransformerModificationInfos.getG().getValue() != null
            || twoWindingsTransformerModificationInfos.getB() != null
            && twoWindingsTransformerModificationInfos.getB().getValue() != null
            || twoWindingsTransformerModificationInfos.getRatedU1() != null
            && twoWindingsTransformerModificationInfos.getRatedU1().getValue() != null
            || twoWindingsTransformerModificationInfos.getRatedU2() != null
            && twoWindingsTransformerModificationInfos.getRatedU2().getValue() != null
            || twoWindingsTransformerModificationInfos.getRatedS() != null
            && twoWindingsTransformerModificationInfos.getRatedS().getValue() != null;
    }

}
