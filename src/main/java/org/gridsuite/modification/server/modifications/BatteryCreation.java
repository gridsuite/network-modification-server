/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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
    public void apply(Network network, ReportNode subReportNode) {
        // create the battery in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createBatteryInNodeBreaker(voltageLevel, modificationInfos, network, subReportNode);
        } else {
            createBatteryInBusBreaker(voltageLevel, modificationInfos, subReportNode);
        }
        if (!modificationInfos.isConnected()) {
            network.getBattery(modificationInfos.getEquipmentId()).getTerminal().disconnect();
        }
        // properties
        Battery battery = network.getBattery(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(battery, subReportNode, modificationInfos.getProperties());
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

    private void createBatteryInBusBreaker(VoltageLevel voltageLevel, BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
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

        addExtensionsToBattery(batteryCreationInfos, battery, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("batteryCreated", "New battery with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void addExtensionsToBattery(BatteryCreationInfos batteryCreationInfos, Battery battery, ReportNode subReportNode) {
        if (batteryCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, batteryCreationInfos.getEquipmentName(), "Name");
        }
        reportBatteryConnectivity(batteryCreationInfos, subReportNode);
        ReportNode subReportNodeLimits = reportBatteryActiveLimits(batteryCreationInfos, subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(batteryCreationInfos, battery, subReportNodeLimits);
        ReportNode subReportNodeSetpoints = reportBatterySetPoints(batteryCreationInfos, subReportNode);
        createBatteryActivePowerControl(batteryCreationInfos, battery, subReportNodeSetpoints);
    }

    private ReportNode reportBatterySetPoints(BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getTargetP(), "Active power"));
        if (batteryCreationInfos.getTargetQ() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(batteryCreationInfos.getTargetQ(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "SetPointCreated", "Setpoints", Map.of());
    }

    private void reportBatteryConnectivity(BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
        if (batteryCreationInfos.getVoltageLevelId() == null || batteryCreationInfos.getBusOrBusbarSectionId() == null) {
            return;
        }

        if (batteryCreationInfos.getConnectionName() != null ||
            batteryCreationInfos.getConnectionDirection() != null ||
            batteryCreationInfos.getConnectionPosition() != null) {
            List<ReportNode> connectivityReports = new ArrayList<>();
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
                connectivityReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentDisconnected", "    Equipment with id=${id} disconnected")
                        .withUntypedValue("id", batteryCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReportNode, connectivityReports, "ConnectivityCreated", CONNECTIVITY, Map.of());
        }
    }

    private ReportNode reportBatteryActiveLimits(BatteryCreationInfos batteryCreationInfos, ReportNode subReportNode) {
        List<ReportNode> limitsReports = new ArrayList<>();
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
        subReportNodeLimits.newReportNode()
            .withMessageTemplate(LIMITS, LIMITS)
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMinP(), "Min active power"));

        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            batteryCreationInfos.getMaxP(), "Max active power"));

        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "ActiveLimitsCreated", ACTIVE_LIMITS, Map.of());
        return subReportNodeLimits;
    }

    private void createBatteryActivePowerControl(BatteryCreationInfos batteryCreationInfos, Battery battery, ReportNode subReporter) {
        if (batteryCreationInfos.getParticipate() != null && batteryCreationInfos.getDroop() != null) {
            List<ReportNode> activePowerRegulationReports = new ArrayList<>();
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
                activePowerRegulationReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("ActivePowerExtensionAddError", "cannot add active power extension on battery with id=${id} : ${message}")
                        .withUntypedValue("id", batteryCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, activePowerRegulationReports, "ActivePowerRegulationCreated", "Active power regulation", Map.of());
        }
    }
}
