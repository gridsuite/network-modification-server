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

    private void addTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        if (twt.hasRatioTapChanger() && twoWindingsTransformerModificationInfos.getRatioTapChanger().getEnabled() != null && Boolean.FALSE.equals(twoWindingsTransformerModificationInfos.getRatioTapChanger().getEnabled().getValue())) {
            twt.getRatioTapChanger().remove();
            subReporter.report(Report.builder().withKey("RatioTapChangerRemoved")
                .withDefaultMessage("The ratio tap changer has been removed")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        } else {
            if (ratioTapChangerModified(twoWindingsTransformerModificationInfos.getRatioTapChanger())) {
                addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerModificationInfos, twt, subReporter);
            }
        }

        if (twt.hasPhaseTapChanger() && twoWindingsTransformerModificationInfos.getPhaseTapChanger().getEnabled() != null && Boolean.FALSE.equals(twoWindingsTransformerModificationInfos.getPhaseTapChanger().getEnabled().getValue())) {
            twt.getPhaseTapChanger().remove();
            subReporter.report(Report.builder().withKey("PhaseTapChangerRemoved")
                .withDefaultMessage("The phase tap changer has been removed")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        } else {
            if (phaseTapChangerModified(twoWindingsTransformerModificationInfos.getPhaseTapChanger())) {
                addPhaseTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerModificationInfos, twt, subReporter);
            }
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
            addStepAttributeReport(tapChangerStepsReports, "newStepConductance" + step.getG(),
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
                ((PhaseTapChangerAdder) adder).beginStep().setR(step.getR()).setX(step.getX()).setG(step.getG()).setB(step.getB()).setRho(step.getRho()).setAlpha(step.getAlpha()).endStep();
            }
        }
    }

    private <T> void modifyTapPositions(TapChangerModificationInfos tapChangerModificationInfos, T adder, TapChanger<?, ?> tapChanger, List<Report> tapChangerReports) {
        if (tapChangerModificationInfos.getLowTapPosition() != null && tapChangerModificationInfos.getLowTapPosition().getValue() != null) {
            Integer oldLowTapPosition = tapChanger != null ? tapChanger.getLowTapPosition() : null;
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldLowTapPosition,
                tapChangerModificationInfos.getLowTapPosition().getValue(), "Low tap position", 2));
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setLowTapPosition(tapChangerModificationInfos.getLowTapPosition().getValue());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setLowTapPosition(tapChangerModificationInfos.getLowTapPosition().getValue());
            }
        } else if (tapChanger != null) {
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setLowTapPosition(tapChanger.getLowTapPosition());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setLowTapPosition(tapChanger.getLowTapPosition());
            }
        }
        if (tapChangerModificationInfos.getTapPosition() != null && tapChangerModificationInfos.getTapPosition().getValue() != null) {
            Integer oldTapPosition = tapChanger != null ? tapChanger.getTapPosition() : null;
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTapPosition,
                tapChangerModificationInfos.getTapPosition().getValue(), "Tap position", 2));
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
            }
        } else if (tapChanger != null) {
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setTapPosition(tapChanger.getTapPosition());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setTapPosition(tapChanger.getTapPosition());
            }
        }
    }

    private <T> void addTapChangerPositionsAndSteps(TapChangerModificationInfos tapChangerModificationInfos, TapChanger<?, ?> tapChanger, T adder, List<Report> tapChangerReports) {
        modifyTapPositions(tapChangerModificationInfos, adder, tapChanger, tapChangerReports);

        // Add steps
        if (tapChangerModificationInfos.getSteps() != null) {
            tapChangerReports.add(Report.builder()
                .withKey("tapsModification")
                .withDefaultMessage("        Taps")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
            addTapchangerSteps(tapChangerReports, tapChangerModificationInfos, adder);
        } else if (tapChanger != null) {
            for (Object step : tapChanger.getAllSteps().values()) {
                if (step instanceof RatioTapChangerStep) {
                    RatioTapChangerStep ratioStep = (RatioTapChangerStep) step;
                    ((RatioTapChangerAdder) adder).beginStep().setR(ratioStep.getR()).setX(ratioStep.getX()).setG(ratioStep.getG()).setB(ratioStep.getB()).setRho(ratioStep.getRho()).endStep();
                } else if (step instanceof PhaseTapChangerStep) {
                    PhaseTapChangerStep phaseStep = (PhaseTapChangerStep) step;
                    ((PhaseTapChangerAdder) adder).beginStep().setR(phaseStep.getR()).setX(phaseStep.getX()).setG(phaseStep.getG()).setB(phaseStep.getB()).setRho(phaseStep.getRho()).setAlpha(phaseStep.getAlpha()).endStep();
                }
            }
        }
    }

    private <T> void modifyRegulatingTerminal(TapChangerModificationInfos tapChangerModificationInfos, T adder, TapChanger<?, ?> tapChanger, List<Report> regulationReports, Network network, Terminal terminal1, Terminal terminal2) {
        Terminal regulatingTerminal = tapChanger != null ? tapChanger.getRegulationTerminal() : null;

        String oldVoltageLevel = null;
        String oldEquipment = null;

        if (regulatingTerminal != null) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }

        if (tapChangerModificationInfos.getRegulationSide() != null && tapChangerModificationInfos.getRegulationSide().getValue() != null) {
            if (tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE1) {
                tapChangerModificationInfos.setRegulatingTerminalVlId(new AttributeModification<>(terminal1.getVoltageLevel().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalId(new AttributeModification<>(terminal1.getConnectable().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalType(new AttributeModification<>(terminal1.getConnectable().getType().name(), OperationType.SET));
            } else if (tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE2) {
                tapChangerModificationInfos.setRegulatingTerminalVlId(new AttributeModification<>(terminal2.getVoltageLevel().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalId(new AttributeModification<>(terminal2.getConnectable().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalType(new AttributeModification<>(terminal2.getConnectable().getType().name(), OperationType.SET));
            }
        }
        if (tapChangerModificationInfos.getRegulatingTerminalId() != null
                && tapChangerModificationInfos.getRegulatingTerminalType() != null
                && tapChangerModificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(network,
                    tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue(),
                    tapChangerModificationInfos.getRegulatingTerminalVlId().getValue());
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder).setRegulationTerminal(terminal);
            } else {
                ((PhaseTapChangerAdder) adder).setRegulationTerminal(terminal);
            }

            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldVoltageLevel,
                    tapChangerModificationInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level", 2));
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldEquipment,
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment", 2));
        } else {
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder).setRegulationTerminal(regulatingTerminal);
            } else {
                ((PhaseTapChangerAdder) adder).setRegulationTerminal(regulatingTerminal);
            }
        }
    }

    private void modifyRatioVoltageRegulation(RatioTapChangerModificationInfos ratioTapChangerInfos, TwoWindingsTransformer twt, RatioTapChangerAdder ratioTapChangerAdder, List<Report> voltageRegulationReports, Network network, Boolean regulating) {
        RatioTapChanger ratioTapChanger = twt.getRatioTapChanger();
        if (ratioTapChangerInfos.getTargetV() != null && ratioTapChangerInfos.getTargetV().getValue() != null) {
            Double oldTargetV = ratioTapChanger != null ? ratioTapChanger.getTargetV() : null;
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV,
                ratioTapChangerInfos.getTargetV().getValue(), "Voltage", 2));
            ratioTapChangerAdder.setTargetV(ratioTapChangerInfos.getTargetV().getValue());
        } else if (ratioTapChanger != null) {
            ratioTapChangerAdder.setTargetV(ratioTapChanger.getTargetV());
        }

        if (ratioTapChangerInfos.getTargetDeadband() != null && ratioTapChangerInfos.getTargetDeadband().getValue() != null) {
            Double oldTargetDeadband = ratioTapChanger != null ? ratioTapChanger.getTargetDeadband() : null;
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetDeadband,
                ratioTapChangerInfos.getTargetDeadband().getValue(), "Target deadband", 2));
            ratioTapChangerAdder.setTargetDeadband(ratioTapChangerInfos.getTargetDeadband().getValue());
        } else if (ratioTapChanger != null && regulating) {
            ratioTapChangerAdder.setTargetDeadband(Double.isNaN(ratioTapChanger.getTargetDeadband()) ? 0. : ratioTapChanger.getTargetDeadband());
        }

        modifyRegulatingTerminal(ratioTapChangerInfos, ratioTapChangerAdder, ratioTapChanger, voltageRegulationReports, network, twt.getTerminal1(), twt.getTerminal2());
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        RatioTapChangerModificationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        RatioTapChanger ratioTapChanger = twt.hasRatioTapChanger() ? twt.getRatioTapChanger() : null;
        List<Report> ratioTapChangerReports = new ArrayList<>();
        Boolean loadTapChangingCapabilities = ratioTapChangerInfos.getLoadTapChangingCapabilities() != null && ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue() != null ? ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue() : null;
        if (loadTapChangingCapabilities != null) {
            Boolean oldLoadTapChangingCapabilities = ratioTapChanger != null ? ratioTapChanger.hasLoadTapChangingCapabilities() : null;
            ratioTapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldLoadTapChangingCapabilities,
                loadTapChangingCapabilities, "On-load", 1));
            ratioTapChangerAdder.setLoadTapChangingCapabilities(loadTapChangingCapabilities);
        } else if (ratioTapChanger != null) {
            ratioTapChangerAdder.setLoadTapChangingCapabilities(ratioTapChanger.hasLoadTapChangingCapabilities());
        }

        String oldRegulationMode = null;
        boolean regulating = false;
        if (ratioTapChanger != null) {
            oldRegulationMode = ratioTapChanger.isRegulating() ? "Voltage regulation" : "Fixed ratio";
        }
        if (ratioTapChangerInfos.getRegulating() != null && ratioTapChangerInfos.getRegulating().getValue() != null) {
            regulating = ratioTapChangerInfos.getRegulating().getValue();
            ratioTapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldRegulationMode,
                regulating ? "Voltage regulation" : "Fixed ratio", "Regulating mode", 1));
            ratioTapChangerAdder.setRegulating(regulating);
        } else if (ratioTapChanger != null) {
            regulating = ratioTapChanger.isRegulating();
            ratioTapChangerAdder.setRegulating(regulating);
        }
        List<Report> voltageRegulationReports = new ArrayList<>();
        modifyRatioVoltageRegulation(ratioTapChangerInfos, twt, ratioTapChangerAdder, voltageRegulationReports, network, regulating);
        List<Report> positionsAndStepsReports = new ArrayList<>();
        addTapChangerPositionsAndSteps(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerAdder, positionsAndStepsReports);

        // TODO : This is a workaround that needs to be removed when modifying tap changer steps will be possible
        // workaround : we need to remove the tap changer before adding the new one to avoid having two tap changers on the same transformer
        // and we need to prepare a backup in case the addition fails
        RatioTapChangerAdder ratioTapChangerBackUp = null;
        if (ratioTapChanger != null) {
            ratioTapChangerBackUp = rollbackRatioModificationsAdder(twt, ratioTapChanger);
            ratioTapChanger.remove();
        }

        try {
            ratioTapChangerAdder.add();
        } catch (Exception e) {
            if (ratioTapChangerBackUp != null) {
                ratioTapChangerBackUp.add();
            }
            throw e;
        }

        if (!ratioTapChangerReports.isEmpty() || !voltageRegulationReports.isEmpty() || !positionsAndStepsReports.isEmpty()) {
            Reporter ratioTapChangerReporter = ModificationUtils.getInstance().reportModifications(subReporter, ratioTapChangerReports, TapChangerType.RATIO.name(), RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE);
            if (ratioTapChangerReporter == null) {
                ratioTapChangerReporter = subReporter.createSubReporter(TapChangerType.RATIO.name(), RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE);
                ratioTapChangerReporter.report(Report.builder()
                            .withKey(TapChangerType.RATIO.name())
                            .withDefaultMessage(RATIO_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
            }
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, voltageRegulationReports, "ratioTapChangerVoltageRegulationModification", "    Voltage regulation");
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, positionsAndStepsReports, "ratioTapChangerPositionsAndStepsModification", "    Tap Changer");
        }
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
                || tapChangerModificationInfos.getTapPosition() != null
                && tapChangerModificationInfos.getTapPosition().getValue() != null
                || tapChangerModificationInfos.getLowTapPosition() != null
                && tapChangerModificationInfos.getLowTapPosition().getValue() != null
                || tapChangerModificationInfos.getSteps() != null);
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

    private void addPhaseTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        PhaseTapChangerModificationInfos phaseTapChangerInfos = twoWindingsTransformerModificationInfos.getPhaseTapChanger();
        PhaseTapChanger phaseTapChanger = twt.hasPhaseTapChanger() ? twt.getPhaseTapChanger() : null;

        List<Report> phaseTapChangerReports = new ArrayList<>();
        PhaseTapChanger.RegulationMode oldRegulationMode = null;
        if (phaseTapChanger != null) {
            oldRegulationMode = phaseTapChanger.getRegulationMode();
            if (!phaseTapChanger.isRegulating()) {
                oldRegulationMode = PhaseTapChanger.RegulationMode.FIXED_TAP;
            }
        }
        if (phaseTapChangerInfos.getRegulationMode() != null && phaseTapChangerInfos.getRegulationMode().getValue() != null) {
            phaseTapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldRegulationMode,
                twoWindingsTransformerModificationInfos.getPhaseTapChanger().getRegulationMode().getValue(), "Regulation mode", 1));
            phaseTapChangerAdder.setRegulationMode(phaseTapChangerInfos.getRegulationMode().getValue());
        } else if (phaseTapChanger != null) {
            phaseTapChangerAdder.setRegulationMode(phaseTapChanger.getRegulationMode());
        }
        List<Report> regulationReports = new ArrayList<>();
        PhaseTapChanger.RegulationMode regulationMode = null;
        if (phaseTapChangerInfos.getRegulationMode() != null && phaseTapChangerInfos.getRegulationMode().getValue() != null) {
            regulationMode = phaseTapChangerInfos.getRegulationMode().getValue();
        } else if (phaseTapChanger != null) {
            regulationMode = oldRegulationMode;
        }
        if (!PhaseTapChanger.RegulationMode.FIXED_TAP.equals(regulationMode)) {
            modifyPhaseTapRegulation(phaseTapChangerInfos, phaseTapChanger, phaseTapChangerAdder, regulationReports, regulationMode);
        }
        modifyRegulatingTerminal(phaseTapChangerInfos, phaseTapChangerAdder, phaseTapChanger, regulationReports, network, twt.getTerminal1(), twt.getTerminal2());

        List<Report> positionsAndStepsReports = new ArrayList<>();
        addTapChangerPositionsAndSteps(phaseTapChangerInfos, phaseTapChanger, phaseTapChangerAdder, positionsAndStepsReports);

        // TODO : This is a workaround that needs to be removed when modifying tap changer steps will be possible
        // workaround : we need to remove the tap changer before adding the new one to avoid having two tap changers on the same transformer
        // and we need to prepare a backup in case the addition fails
        PhaseTapChangerAdder phaseTapChangerBackUp = null;
        if (phaseTapChanger != null) {
            phaseTapChangerBackUp = rollbackPhaseModificationsAdder(twt, phaseTapChanger);
            phaseTapChanger.remove();
        }

        try {
            phaseTapChangerAdder.add();
        } catch (Exception e) {
            if (phaseTapChangerBackUp != null) {
                phaseTapChangerBackUp.add();
            }
            throw e;
        }

        if (!phaseTapChangerReports.isEmpty() || !regulationReports.isEmpty() || !positionsAndStepsReports.isEmpty()) {
            Reporter phaseTapChangerSubreporter = ModificationUtils.getInstance().reportModifications(subReporter, phaseTapChangerReports, TapChangerType.PHASE.name(), PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE);
            if (phaseTapChangerSubreporter == null) {
                phaseTapChangerSubreporter = subReporter.createSubReporter(TapChangerType.PHASE.name(), PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE);
                phaseTapChangerSubreporter.report(Report.builder()
                            .withKey(TapChangerType.PHASE.name())
                            .withDefaultMessage(PHASE_TAP_CHANGER_SUBREPORTER_DEFAULT_MESSAGE)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
            }
            ModificationUtils.getInstance().reportModifications(phaseTapChangerSubreporter, regulationReports, regulationMode != null ? regulationMode.name() : null, ModificationUtils.getInstance().formatRegulationModeReport(regulationMode));
            ModificationUtils.getInstance().reportModifications(phaseTapChangerSubreporter, positionsAndStepsReports, "phaseTapChangerPositionsAndStepsModification", "    Tap Changer");
        }
    }

    private PhaseTapChangerAdder rollbackPhaseModificationsAdder(TwoWindingsTransformer twt, PhaseTapChanger phaseTapChanger) {
        PhaseTapChangerAdder phaseTapChangerAdder = twt.newPhaseTapChanger();
        phaseTapChangerAdder.setRegulationMode(phaseTapChanger.getRegulationMode());
        phaseTapChangerAdder.setRegulating(phaseTapChanger.isRegulating());
        phaseTapChangerAdder.setRegulationValue(phaseTapChanger.getRegulationValue());
        phaseTapChangerAdder.setTapPosition(phaseTapChanger.getTapPosition());
        phaseTapChangerAdder.setLowTapPosition(phaseTapChanger.getLowTapPosition());
        phaseTapChangerAdder.setRegulationTerminal(phaseTapChanger.getRegulationTerminal());
        phaseTapChangerAdder.setTargetDeadband(phaseTapChanger.getTargetDeadband());
        for (PhaseTapChangerStep step : phaseTapChanger.getAllSteps().values()) {
            phaseTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX())
                    .setG(step.getG()).setB(step.getB()).setRho(step.getRho())
                    .setAlpha(step.getAlpha()).endStep();
        }
        return phaseTapChangerAdder;
    }

    private RatioTapChangerAdder rollbackRatioModificationsAdder(TwoWindingsTransformer twt, RatioTapChanger ratioTapChanger) {
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();
        ratioTapChangerAdder.setLoadTapChangingCapabilities(ratioTapChanger.hasLoadTapChangingCapabilities());
        ratioTapChangerAdder.setRegulating(ratioTapChanger.isRegulating());
        ratioTapChangerAdder.setTargetV(ratioTapChanger.getTargetV());
        ratioTapChangerAdder.setTapPosition(ratioTapChanger.getTapPosition());
        ratioTapChangerAdder.setLowTapPosition(ratioTapChanger.getLowTapPosition());
        ratioTapChangerAdder.setRegulationTerminal(ratioTapChanger.getRegulationTerminal());
        ratioTapChangerAdder.setTargetDeadband(ratioTapChanger.getTargetDeadband());
        for (RatioTapChangerStep step : ratioTapChanger.getAllSteps().values()) {
            ratioTapChangerAdder.beginStep().setR(step.getR()).setX(step.getX())
                    .setG(step.getG()).setB(step.getB()).setRho(step.getRho()).endStep();
        }
        return ratioTapChangerAdder;
    }

    private void modifyPhaseTapRegulation(PhaseTapChangerModificationInfos phaseTapChangerInfos, PhaseTapChanger phaseTapChanger, PhaseTapChangerAdder phaseTapChangerAdder, List<Report> regulationReports, PhaseTapChanger.RegulationMode regulationMode) {
        phaseTapChangerAdder.setRegulating(true);

        if (phaseTapChangerInfos.getRegulationValue() != null && phaseTapChangerInfos.getRegulationValue().getValue() != null && regulationMode != null) {
            Double oldRegulationValue = phaseTapChanger != null ? phaseTapChanger.getRegulationValue() : null;
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldRegulationValue,
                phaseTapChangerInfos.getRegulationValue().getValue(), regulationMode.equals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER) ? "Value" : "Flow set point", 2));
            phaseTapChangerAdder.setRegulationValue(phaseTapChangerInfos.getRegulationValue().getValue());
        } else if (phaseTapChanger != null) {
            phaseTapChangerAdder.setRegulationValue(phaseTapChanger.getRegulationValue());
        }

        if (phaseTapChangerInfos.getTargetDeadband() != null && phaseTapChangerInfos.getTargetDeadband().getValue() != null) {
            Double oldTargetDeadband = phaseTapChanger != null ? phaseTapChanger.getTargetDeadband() : null;
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetDeadband,
                phaseTapChangerInfos.getTargetDeadband().getValue(), "Target deadband", 2));
            phaseTapChangerAdder.setTargetDeadband(phaseTapChangerInfos.getTargetDeadband().getValue());
        } else if (phaseTapChanger != null) {
            phaseTapChangerAdder.setTargetDeadband(Double.isNaN(phaseTapChanger.getTargetDeadband()) ? 0. : phaseTapChanger.getTargetDeadband());
        }

    }
}
