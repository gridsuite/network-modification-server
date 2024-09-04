/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.insertReportNode;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorModification extends AbstractModification {

    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";

    private final GeneratorModificationInfos modificationInfos;

    public GeneratorModification(GeneratorModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Missing required attributes to modify the equipment");
        }
        Generator generator = ModificationUtils.getInstance().getGenerator(network, modificationInfos.getEquipmentId());
        // check min max reactive limits
        String errorMessage = "Generator '" + modificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkReactiveLimit(generator, modificationInfos.getMinQ(), modificationInfos.getMaxQ(),
                modificationInfos.getReactiveCapabilityCurvePoints(), MODIFY_GENERATOR_ERROR, errorMessage);
        // check regulated terminal
        if (modificationInfos.getRegulatingTerminalId() != null && modificationInfos.getRegulatingTerminalType() != null &&
                modificationInfos.getRegulatingTerminalVlId() != null) {
            VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getRegulatingTerminalVlId().getValue());
            ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                    modificationInfos.getRegulatingTerminalId().getValue(),
                    modificationInfos.getRegulatingTerminalType().getValue(),
                    modificationInfos.getRegulatingTerminalVlId().getValue());
        }
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(modificationInfos, generator, MODIFY_GENERATOR_ERROR, errorMessage);
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(GeneratorModificationInfos modificationInfos, Generator generator, NetworkModificationException.Type exceptionType, String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                modificationInfos.getTargetP(),
                modificationInfos.getMinP(),
                modificationInfos.getMaxP(),
                generator.getMinP(),
                generator.getMaxP(),
                generator.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Generator generator = ModificationUtils.getInstance().getGenerator(network, modificationInfos.getEquipmentId());
        // modify the generator in the network
        modifyGenerator(generator, modificationInfos, subReportNode);
    }

    private void modifyGenerator(Generator generator, GeneratorModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("generatorModification", "Generator with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(generator::setName, () -> generator.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        }
        ModificationUtils.getInstance().applyElementaryModifications(generator::setEnergySource, generator::getEnergySource, modificationInfos.getEnergySource(), subReportNode, "Energy source");

        modifyGeneratorLimitsAttributes(modificationInfos, generator, subReportNode);
        modifyGeneratorSetpointsAttributes(modificationInfos, generator, subReportNode);
        modifyGeneratorShortCircuitAttributes(modificationInfos.getDirectTransX(), modificationInfos.getStepUpTransformerX(), generator, subReportNode);
        modifyGeneratorStartUpAttributes(modificationInfos, generator, subReportNode);
        modifyGeneratorConnectivityAttributes(modificationInfos, generator, subReportNode);
        PropertiesUtils.applyProperties(generator, subReportNode, modificationInfos.getProperties(), "GeneratorProperties");
    }

    public static void modifyGeneratorShortCircuitAttributes(AttributeModification<Double> directTransX,
                                                             AttributeModification<Double> stepUpTransformerX,
                                                             Generator generator,
                                                             ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        Double oldTransientReactance = generatorShortCircuit != null ? generatorShortCircuit.getDirectTransX() : Double.NaN;
        Double oldStepUpTransformerReactance = generatorShortCircuit != null ? generatorShortCircuit.getStepUpTransformerX() : Double.NaN;
        // Either transient reactance or step-up transformer reactance are modified or
        // both
        if (directTransX != null && stepUpTransformerX != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(directTransX.getValue())
                    .withStepUpTransformerX(stepUpTransformerX.getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldTransientReactance,
                    directTransX.getValue(),
                    "Transient reactance"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldStepUpTransformerReactance,
                    stepUpTransformerX.getValue(),
                    "Transformer reactance"));

        } else if (directTransX != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withStepUpTransformerX(oldStepUpTransformerReactance)
                    .withDirectTransX(directTransX.getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldTransientReactance,
                    directTransX.getValue(),
                    "Transient reactance"));
        } else if (stepUpTransformerX != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withStepUpTransformerX(stepUpTransformerX.getValue())
                    .withDirectTransX(oldTransientReactance)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldStepUpTransformerReactance,
                    stepUpTransformerX.getValue(),
                    "Transformer reactance"));
        }
        if (subReportNode != null) {
            ModificationUtils.getInstance().reportModifications(subReportNode, reports, "shortCircuitAttributesModified", "Short-circuit", Map.of());
        }
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(GeneratorModificationInfos modificationInfos,
                                                              Generator generator, ReportNode subReportNode, ReportNode subReportNodeLimits) {
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReportNode, subReportNodeLimits);
    }

    public static ReportNode modifyGeneratorActiveLimitsAttributes(AttributeModification<Double> maxP,
                                                                   AttributeModification<Double> minP,
                                                                   AttributeModification<Double> ratedS,
                                                                   Generator generator,
                                                                   ReportNode subReportNode) {
        ReportNode subReporterLimits = null;
        ReportNode reportMaxActivePower;
        ReportNode reportMinActivePower;

        if (maxP != null && maxP.getValue() > generator.getMinP()) {
            reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMaxP, generator::getMaxP, maxP, "Max active power");
            reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMinP, generator::getMinP, minP, "Min active power");

        } else {
            reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMinP, generator::getMinP, minP, "Min active power");
            reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMaxP, generator::getMaxP, maxP, "Max active power");
        }
        ReportNode reportRatedNominalPower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setRatedS, generator::getRatedS, ratedS, "Rated nominal power");
        if (subReportNode != null && (reportMaxActivePower != null || reportMinActivePower != null || reportRatedNominalPower != null)) {
            subReporterLimits = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            subReporterLimits.newReportNode()
                    .withMessageTemplate(LIMITS, LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

            ReportNode subReporterActiveLimits = subReporterLimits.newReportNode().withMessageTemplate(ACTIVE_LIMITS, ACTIVE_LIMITS).add();
            subReporterActiveLimits.newReportNode()
                    .withMessageTemplate(ACTIVE_LIMITS, ACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            if (reportMaxActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMinActivePower);
            }
            if (reportRatedNominalPower != null) {
                insertReportNode(subReporterActiveLimits, reportRatedNominalPower);
            }
        }
        return subReporterLimits;
    }

    private void modifyGeneratorReactiveLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                         Generator generator, ReportNode subReportNode, ReportNode subReportNodeLimits) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyGeneratorReactiveCapabilityCurvePoints(modificationInfos, generator, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationInfos.getMinQ(), modificationInfos.getMaxQ(), generator, subReportNode, subReportNodeLimits);
            }
        }
    }

    private ReportNode modifyGeneratorActivePowerControlAttributes(GeneratorModificationInfos modificationInfos,
                                                                 Generator generator, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder<Generator> activePowerControlAdder = generator.newExtension(ActivePowerControlAdder.class);
        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder, modificationInfos.getParticipate(), modificationInfos.getDroop(), subReportNode, subReportNodeSetpoints);
    }

    private void modifyGeneratorStartUpAttributes(GeneratorModificationInfos modificationInfos, Generator generator,
                                                  ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        modifyGeneratorStartUpAttributes(modificationInfos.getPlannedActivePowerSetPoint(),
                modificationInfos.getMarginalCost(),
                modificationInfos.getPlannedOutageRate(),
                modificationInfos.getForcedOutageRate(),
                generator,
                subReportNode,
                reports);
    }

    public static void modifyGeneratorStartUpAttributes(AttributeModification<Double> plannedActivePowerSetPoint,
                                                        AttributeModification<Double> marginalCost,
                                                        AttributeModification<Double> plannedOutageRate,
                                                        AttributeModification<Double> forcedOutageRate,
                                                        Generator generator,
                                                        ReportNode subReportNode,
                                                        List<ReportNode> reports) {
        GeneratorStartupAdder generatorStartupAdder = generator.newExtension(GeneratorStartupAdder.class);
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        boolean plannedActivePowerSetPointUpdated = addPlannedActivePowerSetPoint(plannedActivePowerSetPoint,
                generatorStartupAdder,
                generatorStartup,
                reports);
        boolean marginalCostUpdated = addMarginalCost(marginalCost, generatorStartupAdder, generatorStartup, reports);
        boolean plannedOutageRateUpdated = addPlannedOutageRate(plannedOutageRate, generatorStartupAdder, generatorStartup, reports);
        boolean forcedOutageRateUpdated = addForcedOutageRate(forcedOutageRate, generatorStartupAdder, generatorStartup, reports);

        if (plannedActivePowerSetPointUpdated ||
                marginalCostUpdated ||
                plannedOutageRateUpdated ||
                forcedOutageRateUpdated) {
            generatorStartupAdder.add();
            if (subReportNode != null) {
                ModificationUtils.getInstance().reportModifications(subReportNode, reports, "startUpAttributesModified", "Start up", Map.of());
            }
        }
    }

    private static boolean addForcedOutageRate(AttributeModification<Double> forcedOutageRate, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldForcedOutageRate = generatorStartup != null ? generatorStartup.getForcedOutageRate() : Double.NaN;
        if (forcedOutageRate != null) {
            generatorStartupAdder
                    .withForcedOutageRate(forcedOutageRate.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldForcedOutageRate,
                        forcedOutageRate.getValue(),
                        "Forced outage rate"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withForcedOutageRate(oldForcedOutageRate);
        }
        return false;
    }

    private static boolean addPlannedOutageRate(AttributeModification<Double> plannedOutageRate, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldPlannedOutageRate = generatorStartup != null ? generatorStartup.getPlannedOutageRate() : Double.NaN;
        if (plannedOutageRate != null) {
            generatorStartupAdder
                    .withPlannedOutageRate(plannedOutageRate.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldPlannedOutageRate,
                        plannedOutageRate.getValue(),
                        "Planning outage rate"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withPlannedOutageRate(oldPlannedOutageRate);
        }
        return false;
    }

    private static boolean addMarginalCost(AttributeModification<Double> marginalCost, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldMarginalCost = generatorStartup != null ? generatorStartup.getMarginalCost() : Double.NaN;
        if (marginalCost != null) {
            generatorStartupAdder
                    .withMarginalCost(marginalCost.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldMarginalCost,
                        marginalCost.getValue(),
                        "Marginal cost"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withMarginalCost(oldMarginalCost);
        }
        return false;
    }

    private static boolean addPlannedActivePowerSetPoint(AttributeModification<Double> plannedActivePowerSetPoint, GeneratorStartupAdder generatorStartupAdder,
                                                  GeneratorStartup generatorStartup, List<ReportNode> reports) {
        Double oldPlannedActivePowerSetPoint = generatorStartup != null ? generatorStartup.getPlannedActivePowerSetpoint() : Double.NaN;
        if (plannedActivePowerSetPoint != null) {
            generatorStartupAdder
                    .withPlannedActivePowerSetpoint(plannedActivePowerSetPoint.getValue());
            if (reports != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldPlannedActivePowerSetPoint,
                        plannedActivePowerSetPoint.getValue(),
                        "Planning active power set point"));
            }
            return true;
        } else {
            generatorStartupAdder
                    .withPlannedActivePowerSetpoint(oldPlannedActivePowerSetPoint);
        }
        return false;
    }

    private void modifyGeneratorRegulatingTerminal(GeneratorModificationInfos modificationInfos, Generator generator, List<ReportNode> modificationReports) {
        Terminal regulatingTerminal = generator.getRegulatingTerminal();

        String oldVoltageLevel = null;
        String oldEquipment = null;
        // If there is no regulating terminal in file, regulating terminal voltage level
        // is equal to generator voltage level
        if (regulatingTerminal != null
                && !regulatingTerminal.getVoltageLevel().equals(generator.getTerminal().getVoltageLevel())) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }

        if (modificationInfos.getRegulatingTerminalId() != null
                && modificationInfos.getRegulatingTerminalType() != null
                && modificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(generator.getNetwork(),
                    modificationInfos.getRegulatingTerminalId().getValue(),
                    modificationInfos.getRegulatingTerminalType().getValue(),
                    modificationInfos.getRegulatingTerminalVlId().getValue());
            generator.setRegulatingTerminal(terminal);

            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    modificationInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    modificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + modificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment"));
        }

        // if the voltageRegulationType is set to LOCAL, we set the regulatingTerminal
        // to null
        if (modificationInfos.getVoltageRegulationType() != null
                && modificationInfos.getVoltageRegulationType().getValue() == VoltageRegulationType.LOCAL
                && oldEquipment != null && oldVoltageLevel != null) {
            generator.setRegulatingTerminal(null);
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    null,
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    null,
                    "Equipment"));
        }
    }

    private ReportNode modifyGeneratorVoltageRegulatorAttributes(GeneratorModificationInfos modificationInfos,
                                                               Generator generator, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        List<ReportNode> voltageRegulationReports = new ArrayList<>();

        ReportNode reportVoltageSetpoint = modifyTargetV(generator, modificationInfos.getTargetV());

        voltageRegulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                modificationInfos.getVoltageRegulationOn(), "VoltageRegulationOn"));
        if (reportVoltageSetpoint != null) {
            voltageRegulationReports.add(reportVoltageSetpoint);
        }

        // We apply modifications to regulatingTerminal and QPercent
        // we apply modifications to the reactivepower setpoint
        modifyGeneratorRegulatingTerminal(modificationInfos, generator, voltageRegulationReports);
        if (modificationInfos.getQPercent() != null) {
            CoordinatedReactiveControl coordinatedReactiveControl = generator
                    .getExtension(CoordinatedReactiveControl.class);
            Double oldQPercent = coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent()
                    : Double.NaN;
            generator.newExtension(CoordinatedReactiveControlAdderImpl.class)
                    .withQPercent(modificationInfos.getQPercent().getValue())
                    .add();
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldQPercent,
                    modificationInfos.getQPercent().getValue(), "Reactive percentage"));
        }

        //TargetQ and TargetV are unset after voltage regulation have been dealt with otherwise it can cause unwanted validations exceptions
        if (modificationInfos.getTargetV() != null && modificationInfos.getTargetV().getOp() == OperationType.UNSET) {
            generator.setTargetV(Double.NaN);
        }

        if (modificationInfos.getTargetQ() != null && modificationInfos.getTargetQ().getOp() == OperationType.UNSET) {
            generator.setTargetQ(Double.NaN);
        }

        ReportNode subReportNodeSetpoints2 = subReportNodeSetpoints;
        if (subReportNodeSetpoints == null && !voltageRegulationReports.isEmpty()) {
            subReportNodeSetpoints2 = subReportNode.newReportNode()
                            .withMessageTemplate(SETPOINTS, SETPOINTS)
                            .add();
            subReportNodeSetpoints2.newReportNode()
                    .withMessageTemplate(SETPOINTS, SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        ModificationUtils.getInstance().reportModifications(subReportNodeSetpoints2, voltageRegulationReports, "voltageRegulationModified", "Voltage regulation", Map.of());
        return subReportNodeSetpoints2;
    }

    public static ReportNode modifyTargetV(Generator generator, AttributeModification<Double> modifTargetV) {
        ReportNode reportVoltageSetpoint = null;
        if (modifTargetV != null) {
            if (modifTargetV.getOp() == OperationType.SET) {
                reportVoltageSetpoint = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetV, generator::getTargetV,
                        modifTargetV, "Voltage");
            } else {
                reportVoltageSetpoint = ModificationUtils.getInstance().buildModificationReport(generator.getTargetV(), Double.NaN, "Voltage");
            }
        }
        return reportVoltageSetpoint;
    }

    private void modifyGeneratorSetpointsAttributes(GeneratorModificationInfos modificationInfos,
                                                    Generator generator, ReportNode subReportNode) {
        ReportNode reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetP, generator::getTargetP, modificationInfos.getTargetP(), "Active power");

        ReportNode reportReactivePower = modifyTargetQ(generator, modificationInfos.getTargetQ());

        ReportNode subReporterSetpoints = null;
        if (reportActivePower != null || reportReactivePower != null) {
            subReporterSetpoints = subReportNode.newReportNode().withMessageTemplate(SETPOINTS, SETPOINTS).add();
            subReporterSetpoints.newReportNode()
                    .withMessageTemplate(SETPOINTS, SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            if (reportActivePower != null) {
                insertReportNode(subReporterSetpoints, reportActivePower);
            }
            if (reportReactivePower != null) {
                insertReportNode(subReporterSetpoints, reportReactivePower);
            }
        }
        subReporterSetpoints = modifyGeneratorVoltageRegulatorAttributes(modificationInfos, generator, subReportNode, subReporterSetpoints);
        modifyGeneratorActivePowerControlAttributes(modificationInfos, generator, subReportNode, subReporterSetpoints);
    }

    public static ReportNode modifyTargetQ(Generator generator, AttributeModification<Double> modifTargetQ) {
        ReportNode reportReactivePower = null;
        if (modifTargetQ != null) {
            if (modifTargetQ.getOp() == OperationType.SET) {
                reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetQ, generator::getTargetQ, modifTargetQ, "Reactive power");
            } else {
                reportReactivePower = ModificationUtils.getInstance().buildModificationReport(generator.getTargetQ(), Double.NaN, "Reactive power");
            }
        }
        return reportReactivePower;
    }

    private void modifyGeneratorLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                 Generator generator, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyGeneratorActiveLimitsAttributes(modificationInfos.getMaxP(),
                modificationInfos.getMinP(),
                modificationInfos.getRatedS(),
                generator,
                subReportNode);
        modifyGeneratorReactiveLimitsAttributes(modificationInfos, generator, subReportNode, subReportNodeLimits);
    }

    private ReportNode modifyGeneratorConnectivityAttributes(GeneratorModificationInfos modificationInfos,
                                                             Generator generator, ReportNode subReportNode) {
        ConnectablePosition<Generator> connectablePosition = generator.getExtension(ConnectablePosition.class);
        ConnectablePositionAdder<Generator> connectablePositionAdder = generator.newExtension(ConnectablePositionAdder.class);
        return ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(connectablePosition, connectablePositionAdder, generator, modificationInfos, subReportNode);
    }
}
