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
import com.powsybl.network.store.model.TapChangerStepAttributes;

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
        } else if (ratioTapChangerModified(twoWindingsTransformerModificationInfos.getRatioTapChanger())) {
            if (!twt.hasRatioTapChanger()) {
                addRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerModificationInfos, twt, subReporter);
            } else {
                modifyRatioTapChangersToTwoWindingsTransformer(network, twoWindingsTransformerModificationInfos, twt, subReporter);
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

    private void modifyRatioTapChangersToTwoWindingsTransformer(Network network,
            TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt,
            Reporter subReporter) {
        RatioTapChangerModificationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos
                .getRatioTapChanger();
        RatioTapChanger ratioTapChanger = twt.getRatioTapChanger();

        Reporter ratioTapChangerReporter = subReporter.createSubReporter("ratioTapChangerModification",
                "Ratio tap changer");
        ratioTapChangerReporter.report(Report.builder()
                .withKey("ratioTapChangerModification")
                .withDefaultMessage("Ratio tap changer")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        Boolean loadTapChangingCapabilities = ratioTapChangerInfos.getLoadTapChangingCapabilities() != null
                && ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue() != null
                        ? ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue()
                        : null;
        if (loadTapChangingCapabilities != null) {
            Boolean oldLoadTapChangingCapabilities = ratioTapChanger.hasLoadTapChangingCapabilities();
            ratioTapChangerReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(
                    oldLoadTapChangingCapabilities,
                    loadTapChangingCapabilities, "On-load", 1));
            ratioTapChanger.setLoadTapChangingCapabilities(loadTapChangingCapabilities);
        }
        String oldRegulationMode = ratioTapChanger.isRegulating() ? "Voltage regulation" : "Fixed ratio";
        if (ratioTapChangerInfos.getRegulating() != null && ratioTapChangerInfos.getRegulating().getValue() != null) {
            ratioTapChangerReporter
                    .report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldRegulationMode,
                            Boolean.TRUE.equals(ratioTapChangerInfos.getRegulating().getValue()) ? "Voltage regulation"
                                    : "Fixed ratio",
                            "Regulating mode", 1));
            ratioTapChanger.setRegulating(ratioTapChangerInfos.getRegulating().getValue());
        }
        modifyRatioVoltageRegulation(ratioTapChangerInfos, twt, ratioTapChanger, ratioTapChangerReporter, network);
        modifyTapChangerPositionsAndSteps(ratioTapChangerInfos, ratioTapChanger, ratioTapChangerReporter, twt);
    }

    private void modifyTapChangerPositionsAndSteps(TapChangerModificationInfos tapChangerModificationInfos,
            TapChanger<?, ?> tapChanger, Reporter subReporter, TwoWindingsTransformer twt) {
        List<Report> tapChangerReports = new ArrayList<>();
        modifyTapPositions(tapChangerModificationInfos, tapChanger, tapChangerReports);

        // Add steps
        if (tapChangerModificationInfos.getSteps() != null) {
            tapChangerReports.add(Report.builder()
                .withKey("tapsModification")
                .withDefaultMessage("        Taps")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
            modifyTapchangerSteps(tapChangerReports, tapChangerModificationInfos, tapChanger, twt);
        }
        if (!tapChangerReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReporter, tapChangerReports, "tapChangerModification", "    Tap changer");
        }
    }

    private void modifyTapchangerSteps(List<Report> tapChangerStepsReports,
            TapChangerModificationInfos tapChangerModificationInfos, TapChanger<?, ?> tapChanger, TwoWindingsTransformer twt) {
        tapChangerStepsReports.add(Report.builder().withKey("tapChangerStepsModification")
                .withDefaultMessage("            Taps were replaced by new ones below")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        List<TapChangerStepAttributes> tapChangerStepAttributes = new ArrayList<>();
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
            //tapChangerStepAttributes.add(new TapChangerStepAttributes(step.getIndex(), step.getR(), step.getX(), step.getG(), step.getB(), step.getRho(), null, null, null));
        }
        // after powsybl TapChanger interface is updated, this method could be used to modify tapChanger steps
        // TODO : tapChanger.setSteps(tapChangerStepAttributes);
    }

    private void modifyTapPositions(TapChangerModificationInfos tapChangerModificationInfos,
            TapChanger<?, ?> tapChanger, List<Report> tapChangerReports) {
        if (tapChangerModificationInfos.getLowTapPosition() != null && tapChangerModificationInfos.getLowTapPosition().getValue() != null) {
            Integer oldLowTapPosition = tapChanger.getLowTapPosition();
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldLowTapPosition,
                tapChangerModificationInfos.getLowTapPosition().getValue(), "Low tap position", 2));
            tapChanger.setLowTapPosition(tapChangerModificationInfos.getLowTapPosition().getValue());
        }
        if (tapChangerModificationInfos.getTapPosition() != null && tapChangerModificationInfos.getTapPosition().getValue() != null) {
            Integer oldTapPosition = tapChanger.getTapPosition();
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTapPosition,
                tapChangerModificationInfos.getTapPosition().getValue(), "Tap position", 2));
            tapChanger.setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
        }
    }

    private void modifyRatioVoltageRegulation(RatioTapChangerModificationInfos ratioTapChangerInfos,
            TwoWindingsTransformer twt, RatioTapChanger ratioTapChanger, Reporter ratioTapChangerReporter,
            Network network) {
        List<Report> voltageRegulationReports = new ArrayList<>();
        if (ratioTapChangerInfos.getTargetV() != null && ratioTapChangerInfos.getTargetV().getValue() != null) {
            Double oldTargetV = ratioTapChanger.getTargetV();
            voltageRegulationReports
                    .add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetV,
                            ratioTapChangerInfos.getTargetV().getValue(), "Voltage", 2));
            ratioTapChanger.setTargetV(ratioTapChangerInfos.getTargetV().getValue());
        }

        if (ratioTapChangerInfos.getTargetDeadband() != null
                && ratioTapChangerInfos.getTargetDeadband().getValue() != null) {
            Double oldTargetDeadband = ratioTapChanger.getTargetDeadband();
            voltageRegulationReports
                    .add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldTargetDeadband,
                            ratioTapChangerInfos.getTargetDeadband().getValue(), "Target deadband", 2));
            ratioTapChanger.setTargetDeadband(ratioTapChangerInfos.getTargetDeadband().getValue());
        }
        modifyRegulatingTerminal(ratioTapChangerInfos, ratioTapChanger, voltageRegulationReports,
                network, twt.getTerminal1(), twt.getTerminal2());
        if (!voltageRegulationReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, voltageRegulationReports,
                    "ratioTapChangerVoltageRegulationModification", "    Voltage regulation");
        }
    }

    private void modifyRegulatingTerminal(TapChangerModificationInfos tapChangerModificationInfos,
            TapChanger<?, ?> tapChanger, List<Report> regulationReports, Network network, Terminal terminal1,
            Terminal terminal2) {
        Terminal regulatingTerminal = tapChanger.getRegulationTerminal();

        String oldVoltageLevel = null;
        String oldEquipment = null;

        if (regulatingTerminal != null) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }

        if (tapChangerModificationInfos.getRegulationType() != null && tapChangerModificationInfos.getRegulationType().getValue() == VoltageRegulationType.LOCAL) {
            if (tapChangerModificationInfos.getRegulationSide() != null && tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE1) {
                tapChangerModificationInfos.setRegulatingTerminalVlId(new AttributeModification<>(terminal1.getVoltageLevel().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalId(new AttributeModification<>(terminal1.getConnectable().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalType(new AttributeModification<>(terminal1.getConnectable().getType().name(), OperationType.SET));
            } else if (tapChangerModificationInfos.getRegulationSide() != null && tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE2) {
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
            tapChanger.setRegulationTerminal(terminal);
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldVoltageLevel,
                    tapChangerModificationInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level", 2));
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldEquipment,
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment", 2));
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

    private <T> void addTapPositions(TapChangerModificationInfos tapChangerModificationInfos, T adder, List<Report> tapChangerReports) {
        if (tapChangerModificationInfos.getLowTapPosition() != null && tapChangerModificationInfos.getLowTapPosition().getValue() != null) {
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
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
            tapChangerReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null, tapChangerModificationInfos.getTapPosition().getValue(), "Tap position", 2));
            if (adder instanceof RatioTapChangerAdder) {
                ((RatioTapChangerAdder) adder)
                        .setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
            } else {
                ((PhaseTapChangerAdder) adder)
                        .setTapPosition(tapChangerModificationInfos.getTapPosition().getValue());
            }
        }
    }

    private <T> void addTapChangerPositionsAndSteps(TapChangerModificationInfos tapChangerModificationInfos, T adder, Reporter subReporter) {
        List<Report> tapChangerReports = new ArrayList<>();
        addTapPositions(tapChangerModificationInfos, adder, tapChangerReports);

        // Add steps
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

    private <T> void addRegulatingTerminal(TapChangerModificationInfos tapChangerModificationInfos, T adder, List<Report> regulationReports, Network network, Terminal terminal1, Terminal terminal2) {
        if (tapChangerModificationInfos.getRegulationType() != null && tapChangerModificationInfos.getRegulationType().getValue() == VoltageRegulationType.LOCAL) {
            if (tapChangerModificationInfos.getRegulationSide() != null && tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE1) {
                tapChangerModificationInfos.setRegulatingTerminalVlId(new AttributeModification<>(terminal1.getVoltageLevel().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalId(new AttributeModification<>(terminal1.getConnectable().getId(), OperationType.SET));
                tapChangerModificationInfos.setRegulatingTerminalType(new AttributeModification<>(terminal1.getConnectable().getType().name(), OperationType.SET));
            } else if (tapChangerModificationInfos.getRegulationSide() != null && tapChangerModificationInfos.getRegulationSide().getValue() == RegulationSide.SIDE2) {
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

            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                    tapChangerModificationInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level", 2));
            regulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                    tapChangerModificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + tapChangerModificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment", 2));
        }
    }

    private void addRatioVoltageRegulation(RatioTapChangerModificationInfos ratioTapChangerInfos, TwoWindingsTransformer twt, RatioTapChangerAdder ratioTapChangerAdder, Reporter ratioTapChangerReporter, Network network) {
        List<Report> voltageRegulationReports = new ArrayList<>();
        if (ratioTapChangerInfos.getTargetV() != null && ratioTapChangerInfos.getTargetV().getValue() != null) {
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                ratioTapChangerInfos.getTargetV().getValue(), "Voltage", 2));
            ratioTapChangerAdder.setTargetV(ratioTapChangerInfos.getTargetV().getValue());
        }

        if (ratioTapChangerInfos.getTargetDeadband() != null && ratioTapChangerInfos.getTargetDeadband().getValue() != null) {
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null, ratioTapChangerInfos.getTargetDeadband().getValue(), "Target deadband", 2));
            ratioTapChangerAdder.setTargetDeadband(ratioTapChangerInfos.getTargetDeadband().getValue());
        }

        addRegulatingTerminal(ratioTapChangerInfos, ratioTapChangerAdder, voltageRegulationReports, network, twt.getTerminal1(), twt.getTerminal2());

        if (!voltageRegulationReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(ratioTapChangerReporter, voltageRegulationReports, "ratioTapChangerVoltageRegulationModification", "    Voltage regulation");
        }
    }

    private void addRatioTapChangersToTwoWindingsTransformer(Network network, TwoWindingsTransformerModificationInfos twoWindingsTransformerModificationInfos, TwoWindingsTransformer twt, Reporter subReporter) {
        RatioTapChangerModificationInfos ratioTapChangerInfos = twoWindingsTransformerModificationInfos.getRatioTapChanger();
        RatioTapChangerAdder ratioTapChangerAdder = twt.newRatioTapChanger();

        Reporter ratioTapChangerReporter = subReporter.createSubReporter("ratioTapChangerModification", "Ratio tap changer");
        ratioTapChangerReporter.report(Report.builder()
                    .withKey("ratioTapChangerModification")
                    .withDefaultMessage("Ratio tap changer")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

        Boolean loadTapChangingCapabilities = ratioTapChangerInfos.getLoadTapChangingCapabilities() != null && ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue() != null ? ratioTapChangerInfos.getLoadTapChangingCapabilities().getValue() : null;
        if (loadTapChangingCapabilities != null) {
            ratioTapChangerReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                loadTapChangingCapabilities, "On-load", 1));
            ratioTapChangerAdder.setLoadTapChangingCapabilities(loadTapChangingCapabilities);
        }
        if (ratioTapChangerInfos.getRegulating() != null && ratioTapChangerInfos.getRegulating().getValue() != null) {
            ratioTapChangerReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                Boolean.TRUE.equals(ratioTapChangerInfos.getRegulating().getValue()) ? "Voltage regulation" : "Fixed ratio", "Regulating mode", 1));
            ratioTapChangerAdder.setRegulating(ratioTapChangerInfos.getRegulating().getValue());
        }
        addRatioVoltageRegulation(ratioTapChangerInfos, twt, ratioTapChangerAdder, ratioTapChangerReporter, network);
        addTapChangerPositionsAndSteps(ratioTapChangerInfos, ratioTapChangerAdder, ratioTapChangerReporter);
        ratioTapChangerAdder.add();
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
        Reporter phaseTapChangerSubreporter = subReporter.createSubReporter(TapChangerType.PHASE.name(), "Phase tap changer");
        phaseTapChangerSubreporter.report(Report.builder()
                    .withKey(TapChangerType.PHASE.name())
                    .withDefaultMessage("Phase tap changer")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        if (phaseTapChangerInfos.getRegulationMode() != null && phaseTapChangerInfos.getRegulationMode().getValue() != null) {
            phaseTapChangerSubreporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                twoWindingsTransformerModificationInfos.getPhaseTapChanger().getRegulationMode().getValue(), "Regulation mode", 1));
            phaseTapChangerAdder.setRegulationMode(phaseTapChangerInfos.getRegulationMode().getValue());
        }
        List<Report> regulationReports = new ArrayList<>();
        PhaseTapChanger.RegulationMode regulationMode = null;
        if (phaseTapChangerInfos.getRegulationMode() != null && phaseTapChangerInfos.getRegulationMode().getValue() != null) {
            regulationMode = phaseTapChangerInfos.getRegulationMode().getValue();
        }
        if (!PhaseTapChanger.RegulationMode.FIXED_TAP.equals(regulationMode)) {
            modifyPhaseTapRegulation(phaseTapChangerInfos, phaseTapChangerAdder, regulationReports, regulationMode);
        }
        addRegulatingTerminal(phaseTapChangerInfos, phaseTapChangerAdder, regulationReports, network, twt.getTerminal1(), twt.getTerminal2());
        if (!regulationReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(phaseTapChangerSubreporter, regulationReports, regulationMode != null ? regulationMode.name() : null, ModificationUtils.getInstance().formatRegulationModeReport(regulationMode));
        }

        addTapChangerPositionsAndSteps(phaseTapChangerInfos, phaseTapChangerAdder, phaseTapChangerSubreporter);
        phaseTapChangerAdder.add();
    }

    private void modifyPhaseTapRegulation(PhaseTapChangerModificationInfos phaseTapChangerInfos, PhaseTapChangerAdder phaseTapChangerAdder, List<Report> phaseTapChangerReporter, PhaseTapChanger.RegulationMode regulationMode) {
        phaseTapChangerAdder.setRegulating(true);

        if (phaseTapChangerInfos.getRegulationValue() != null && phaseTapChangerInfos.getRegulationValue().getValue() != null && regulationMode != null) {
            phaseTapChangerReporter.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                phaseTapChangerInfos.getRegulationValue().getValue(), regulationMode.equals(PhaseTapChanger.RegulationMode.CURRENT_LIMITER) ? "Value" : "Flow set point", 2));
            phaseTapChangerAdder.setRegulationValue(phaseTapChangerInfos.getRegulationValue().getValue());
        }

        if (phaseTapChangerInfos.getTargetDeadband() != null && phaseTapChangerInfos.getTargetDeadband().getValue() != null) {
            phaseTapChangerReporter.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(null,
                phaseTapChangerInfos.getTargetDeadband().getValue(), "Target deadband", 2));
            phaseTapChangerAdder.setTargetDeadband(phaseTapChangerInfos.getTargetDeadband().getValue());
        }
    }
}
