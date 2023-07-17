/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_BATTERY_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BATTERY_ALREADY_EXISTS;
import static org.gridsuite.modification.server.modifications.ModificationUtils.nanIfNull;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class BatteryCreation extends AbstractModification {

    private final BatteryCreationInfos modificationInfos;
    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String REACTIVE_LIMITS = "Reactive limits";
    private static final String CONNECTIVITY = "Connectivity";

    public BatteryCreation(BatteryCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    private static NetworkModificationException makeBatteryException(String batteryId, String msgSuffix) {
        return new NetworkModificationException(CREATE_BATTERY_ERROR, "Battery '" + batteryId + "' : " + msgSuffix);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getBattery(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(BATTERY_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }

        // check connectivity
        ModificationUtils.getInstance()
                .controlConnectivity(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition());

        // check min max reactive limits
        if (modificationInfos.getMinimumReactivePower() != null && modificationInfos.getMaximumReactivePower() != null) {
            if (Double.isNaN(modificationInfos.getMinimumReactivePower())) {
                throw makeBatteryException(modificationInfos.getEquipmentId(), "minimum reactive power is not set");
            } else if (Double.isNaN(modificationInfos.getMaximumReactivePower())) {
                throw makeBatteryException(modificationInfos.getEquipmentId(), "maximum reactive power is not set");
            } else if (modificationInfos.getMaximumReactivePower() < modificationInfos.getMinimumReactivePower()) {
                throw makeBatteryException(modificationInfos.getEquipmentId(), "maximum reactive power is expected to be greater than or equal to minimum reactive power");
            }
        }

        // check reactive capability curve limits
        List<ReactiveCapabilityCurveCreationInfos> points = modificationInfos.getReactiveCapabilityCurvePoints();
        if (!CollectionUtils.isEmpty(points)) {
            if (points.size() < 2) {
                throw makeBatteryException(modificationInfos.getEquipmentId(), "a reactive capability curve should have at least two points");
            }
            IntStream.range(0, points.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurveCreationInfos newPoint = points.get(i);
                        if (Double.isNaN(newPoint.getP())) {
                            throw makeBatteryException(modificationInfos.getEquipmentId(), "P is not set in a reactive capability curve limits point");
                        } else if (Double.isNaN(newPoint.getQminP())) {
                            throw makeBatteryException(modificationInfos.getEquipmentId(), "min Q is not set in a reactive capability curve limits point");
                        } else if (Double.isNaN(newPoint.getQmaxP())) {
                            throw makeBatteryException(modificationInfos.getEquipmentId(), "max Q is not set in a reactive capability curve limits point");
                        }
                    });
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // create the battery in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createBatteryInNodeBreaker(voltageLevel, modificationInfos, network, subReporter);
        } else {
            createBatteryInBusBreaker(voltageLevel, modificationInfos, subReporter);
        }
    }

    private void createBatteryInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos, Network network, Reporter subReporter) {
        BatteryAdder batteryAdder = createBatteryAdderInNodeBreaker(voltageLevel, batteryCreationInfos);
        var position = ModificationUtils.getInstance().getPosition(batteryCreationInfos.getConnectionPosition(),
                batteryCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);

        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(batteryCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(batteryCreationInfos.getConnectionDirection())
                .withInjectionFeederName(batteryCreationInfos.getConnectionName() != null
                        ? batteryCreationInfos.getConnectionName()
                        : batteryCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(batteryAdder)
                .build();

        algo.apply(network, true, subReporter);

        var battery = ModificationUtils.getInstance().getBattery(network, batteryCreationInfos.getEquipmentId());
        addExtensionsToBattery(batteryCreationInfos, battery, voltageLevel, subReporter);
    }

    private BatteryAdder createBatteryAdderInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos) {

        return voltageLevel.newBattery()
                .setId(batteryCreationInfos.getEquipmentId())
                .setName(batteryCreationInfos.getEquipmentName())
                .setMinP(batteryCreationInfos.getMinActivePower())
                .setMaxP(batteryCreationInfos.getMaxActivePower())
                .setTargetP(batteryCreationInfos.getActivePowerSetpoint())
                .setTargetQ(nanIfNull(batteryCreationInfos.getReactivePowerSetpoint()));
    }

    private void createBatteryInBusBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos, Reporter subReporter) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, batteryCreationInfos.getBusOrBusbarSectionId());

        // creating the battery
        Battery battery = voltageLevel.newBattery()
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setId(batteryCreationInfos.getEquipmentId())
                .setName(batteryCreationInfos.getEquipmentName())
                .setMinP(batteryCreationInfos.getMinActivePower())
                .setMaxP(batteryCreationInfos.getMaxActivePower())
                .setTargetP(batteryCreationInfos.getActivePowerSetpoint())
                .setTargetQ(nanIfNull(batteryCreationInfos.getReactivePowerSetpoint()))
                .add();

        addExtensionsToBattery(batteryCreationInfos, battery, voltageLevel, subReporter);

        subReporter.report(Report.builder()
                .withKey("batteryCreated")
                .withDefaultMessage("New battery with id=${id} created")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void addExtensionsToBattery(BatteryCreationInfos batteryCreationInfos, Battery battery,
                                          VoltageLevel voltageLevel, Reporter subReporter) {
        if (batteryCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReporter, batteryCreationInfos.getEquipmentName(), "Name");
        }
        reportBatteryConnectivity(batteryCreationInfos, subReporter);
        Reporter subReporterLimits = reportBatteryActiveLimits(batteryCreationInfos, subReporter);
        createBatteryReactiveLimits(batteryCreationInfos, battery, subReporterLimits);
        Reporter subReporterSetpoints = reportBatterySetPoints(batteryCreationInfos, subReporter);
        createBatteryActivePowerControl(batteryCreationInfos, battery, subReporterSetpoints);
    }

    private void addToReports(List<Report> reports, Double newValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildCreationReport(newValue, fieldName));
        }
    }

    private void createBatteryReactiveLimits(BatteryCreationInfos batteryCreationInfos, Battery battery, Reporter subReporter) {
        if (Boolean.TRUE.equals(batteryCreationInfos.getReactiveCapabilityCurve())) {
            createReactiveCapabilityCurve(batteryCreationInfos, battery, subReporter);
        } else if (Boolean.FALSE.equals(batteryCreationInfos.getReactiveCapabilityCurve())) {
            createMinMaxReactiveLimits(batteryCreationInfos, battery, subReporter);
        }
    }

    private void createMinMaxReactiveLimits(BatteryCreationInfos batteryCreationInfos, Battery battery, Reporter subReporter) {
        List<Report> minMaxReactiveLimitsReports = new ArrayList<>();
        if (batteryCreationInfos.getMinimumReactivePower() != null && batteryCreationInfos.getMaximumReactivePower() != null) {
            battery.newMinMaxReactiveLimits().setMinQ(batteryCreationInfos.getMinimumReactivePower())
                .setMaxQ(batteryCreationInfos.getMaximumReactivePower())
                .add();
            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                batteryCreationInfos.getMinimumReactivePower(),
                "Minimum reactive power"));
            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                batteryCreationInfos.getMaximumReactivePower(),
                "Maximum reactive power"));
            Reporter subReporterReactiveLimits = subReporter.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
            subReporterReactiveLimits.report(Report.builder()
                .withKey(REACTIVE_LIMITS)
                .withDefaultMessage(REACTIVE_LIMITS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
            ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, minMaxReactiveLimitsReports, "minMaxReactiveLimitsCreated", "By range");
        }
    }

    private void createReactiveCapabilityCurve(BatteryCreationInfos batteryCreationInfos, Battery battery, Reporter subReporter) {
        List<Report> pointsReports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveCreationInfos> points = batteryCreationInfos.getReactiveCapabilityCurvePoints();
        IntStream.range(0, points.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurveCreationInfos newPoint = points.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (points.size() - 1)) {
                        fieldSuffix = "max";
                    } else {
                        fieldSuffix = Integer.toString(i);
                    }
                    createReactiveCapabilityCurvePoint(adder, newPoint, pointsReports, fieldSuffix);
                });
        adder.add();
        Reporter subReporterReactiveLimits = subReporter.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
        subReporterReactiveLimits.report(Report.builder()
                .withKey(REACTIVE_LIMITS)
                .withDefaultMessage(REACTIVE_LIMITS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, pointsReports, "curveReactiveLimitsCreated", "By diagram");
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder, ReactiveCapabilityCurveCreationInfos point,
                                                    List<Report> reports, String fieldSuffix) {
        adder.beginPoint()
                .setMaxQ(point.getQmaxP())
                .setMinQ(point.getQminP())
                .setP(point.getP())
                .endPoint();
        addToReports(reports, point.getP(), "P" + fieldSuffix);
        addToReports(reports, point.getQminP(), "QminP" + fieldSuffix);
        addToReports(reports, point.getQmaxP(), "QmaxP" + fieldSuffix);
    }

    private Reporter reportBatterySetPoints(BatteryCreationInfos batteryCreationInfos, Reporter subReporter) {
        List<Report> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getActivePowerSetpoint(), "Active power"));
        if (batteryCreationInfos.getReactivePowerSetpoint() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getReactivePowerSetpoint(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReporter, setPointReports, "SetPointCreated", "Setpoints");
    }

    private void reportBatteryConnectivity(BatteryCreationInfos batteryCreationInfos, Reporter subReporter) {
        if (batteryCreationInfos.getVoltageLevelId() == null || batteryCreationInfos.getBusOrBusbarSectionId() == null) {
            return;
        }

        if (batteryCreationInfos.getConnectionName() != null ||
            batteryCreationInfos.getConnectionDirection() != null ||
            batteryCreationInfos.getConnectionPosition() != null) {
            List<Report> connectivityReports = new ArrayList<>();
            if (batteryCreationInfos.getConnectionName() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(batteryCreationInfos.getConnectionName(), "Connection name"));
            }
            if (batteryCreationInfos.getConnectionDirection() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(batteryCreationInfos.getConnectionDirection(), "Connection direction"));
            }
            if (batteryCreationInfos.getConnectionPosition() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(batteryCreationInfos.getConnectionPosition(), "Connection position"));
            }
            ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "ConnectivityCreated", CONNECTIVITY);
        }
    }

    private Reporter reportBatteryActiveLimits(BatteryCreationInfos batteryCreationInfos, Reporter subReporter) {
        List<Report> limitsReports = new ArrayList<>();
        Reporter subReporterLimits = subReporter.createSubReporter(LIMITS, LIMITS);
        subReporterLimits.report(Report.builder()
            .withKey(LIMITS)
            .withDefaultMessage(LIMITS)
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMinActivePower(), "Min active power"));

        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMaxActivePower(), "Max active power"));

        ModificationUtils.getInstance().reportModifications(subReporterLimits, limitsReports, "ActiveLimitsCreated", ACTIVE_LIMITS);
        return subReporterLimits;
    }

    private void createBatteryActivePowerControl(BatteryCreationInfos batteryCreationInfos, Battery battery, Reporter subReporter) {
        if (batteryCreationInfos.getParticipate() != null && batteryCreationInfos.getDroop() != null) {
            List<Report> activePowerRegulationReports = new ArrayList<>();
            try {
                battery.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(batteryCreationInfos.getParticipate())
                        .withDroop(batteryCreationInfos.getDroop())
                        .add();
                activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(
                        batteryCreationInfos.getDroop(),
                        "Droop"));
            } catch (PowsyblException e) {
                activePowerRegulationReports.add(Report.builder()
                        .withKey("ActivePowerExtensionAddError")
                        .withDefaultMessage("cannot add active power extension on battery with id=${id} : " + e.getMessage())
                        .withValue("id", batteryCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, activePowerRegulationReports, "ActivePowerRegulationCreated", "Active power regulation");
        }
    }
}