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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BATTERY_ALREADY_EXISTS;
import static org.gridsuite.modification.server.modifications.ModificationUtils.nanIfNull;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class BatteryCreation extends AbstractModification {

    private final BatteryCreationInfos modificationInfos;
    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String CONNECTIVITY = "Connectivity";

    public BatteryCreation(BatteryCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
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

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(modificationInfos,
                modificationInfos.getErrorType(),
                modificationInfos.getEquipmentId(),
                "Battery");
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
        if (!modificationInfos.isConnected()) {
            network.getBattery(modificationInfos.getEquipmentId()).getTerminal().disconnect();
        }
        // properties
        Battery battery = network.getBattery(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(battery, subReporter, modificationInfos.getProperties());
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
        addExtensionsToBattery(batteryCreationInfos, battery, subReporter);
    }

    private BatteryAdder createBatteryAdderInNodeBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos) {

        return voltageLevel.newBattery()
                .setId(batteryCreationInfos.getEquipmentId())
                .setName(batteryCreationInfos.getEquipmentName())
                .setMinP(batteryCreationInfos.getMinP())
                .setMaxP(batteryCreationInfos.getMaxP())
                .setTargetP(batteryCreationInfos.getTargetP())
                .setTargetQ(nanIfNull(batteryCreationInfos.getTargetQ()));
    }

    private void createBatteryInBusBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos, Reporter subReporter) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, batteryCreationInfos.getBusOrBusbarSectionId());

        // creating the battery
        Battery battery = voltageLevel.newBattery()
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setId(batteryCreationInfos.getEquipmentId())
                .setName(batteryCreationInfos.getEquipmentName())
                .setMinP(batteryCreationInfos.getMinP())
                .setMaxP(batteryCreationInfos.getMaxP())
                .setTargetP(batteryCreationInfos.getTargetP())
                .setTargetQ(nanIfNull(batteryCreationInfos.getTargetQ()))
                .add();

        addExtensionsToBattery(batteryCreationInfos, battery, subReporter);

        subReporter.report(Report.builder()
                .withKey("batteryCreated")
                .withDefaultMessage("New battery with id=${id} created")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void addExtensionsToBattery(BatteryCreationInfos batteryCreationInfos, Battery battery, Reporter subReporter) {
        if (batteryCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReporter, batteryCreationInfos.getEquipmentName(), "Name");
        }
        reportBatteryConnectivity(batteryCreationInfos, subReporter);
        Reporter subReporterLimits = reportBatteryActiveLimits(batteryCreationInfos, subReporter);
        ModificationUtils.getInstance().createReactiveLimits(batteryCreationInfos, battery, subReporterLimits);
        Reporter subReporterSetpoints = reportBatterySetPoints(batteryCreationInfos, subReporter);
        createBatteryActivePowerControl(batteryCreationInfos, battery, subReporterSetpoints);
    }

    private Reporter reportBatterySetPoints(BatteryCreationInfos batteryCreationInfos, Reporter subReporter) {
        List<Report> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getTargetP(), "Active power"));
        if (batteryCreationInfos.getTargetQ() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getTargetQ(), "Reactive power"));
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
            if (!batteryCreationInfos.isConnected()) {
                connectivityReports.add(Report.builder()
                        .withKey("equipmentDisconnected")
                        .withDefaultMessage("    Equipment with id=${id} disconnected")
                        .withValue("id", batteryCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
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
            batteryCreationInfos.getMinP(), "Min active power"));

        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMaxP(), "Max active power"));

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
                        batteryCreationInfos.getParticipate(),
                        "Participate"));
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
