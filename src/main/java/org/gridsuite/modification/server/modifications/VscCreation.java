/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.VscConverterStation;
import com.powsybl.iidm.network.VscConverterStationAdder;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.VscCreationInfos;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_VSC_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.HVDC_LINE_ALREADY_EXISTS;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

public class VscCreation extends AbstractModification {
    public static final String CHARACTERISTICS = "Characteristics";
    public static final String SETPOINTS = "Setpoints";
    private final VscCreationInfos modificationInfos;

    public VscCreation(VscCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getHvdcLine(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(HVDC_LINE_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }

        checkConverterStation(network, modificationInfos.getConverterStation1());
        checkConverterStation(network, modificationInfos.getConverterStation2());
    }

    private void checkConverterStation(Network network,
                                       ConverterStationCreationInfos converterStation) {
        if (converterStation == null) {
            throw new NetworkModificationException(CREATE_VSC_ERROR, modificationInfos.getEquipmentId() + "Missing required converter station");
        }
        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network,
                converterStation.getVoltageLevelId(),
                converterStation.getBusOrBusbarSectionId(),
                converterStation.getConnectionPosition());

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(converterStation,
                CREATE_VSC_ERROR,
                modificationInfos.getEquipmentId(),
                "Vsc");
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        VscConverterStation converterStation1 = createConverterStation(network, modificationInfos.getConverterStation1(), subReporter, "Converter station 1");

        VscConverterStation converterStation2 = createConverterStation(network, modificationInfos.getConverterStation2(), subReporter, "Converter station 2");

        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(modificationInfos.getEquipmentId())
                .setName(modificationInfos.getEquipmentName())
                .setNominalV(modificationInfos.getNominalV())
                .setR(modificationInfos.getR())
                .setMaxP(modificationInfos.getMaxP())
                .setActivePowerSetpoint(modificationInfos.getActivePowerSetpoint())
                .setConvertersMode(modificationInfos.getConvertersMode())
                .setConverterStationId1(converterStation1 != null ? converterStation1.getId() : null)
                .setConverterStationId2(converterStation2 != null ? converterStation2.getId() : null)
                .add();

        if (modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2() != null ||
                modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1() != null) {
            hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                    .withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2())
                    .withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1())
                    .add();
        }

        if (modificationInfos.getDroop() != null ||
                modificationInfos.getP0() != null) {
            var activePowerControlExtension = hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class)
                    .withEnabled(modificationInfos.getAngleDroopActivePowerControl());
            if (modificationInfos.getP0() != null) {
                activePowerControlExtension.withP0(modificationInfos.getP0());
            }

            if (modificationInfos.getDroop() != null) {
                activePowerControlExtension.withDroop(modificationInfos.getDroop());
            }

            activePowerControlExtension.add();
        }
        reportHvdcLineInfos(subReporter);

        subReporter.report(Report.builder()
                .withKey("vscCreated")
                .withDefaultMessage("New vsc with id=${id} created")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReporter, modificationInfos.getEquipmentName(), "Name");
        }
    }

    private void reportHvdcLineInfos(Reporter subReporter) {
        List<Report> characteristicsReports = new ArrayList<>();
        Reporter characteristicReport = subReporter.createSubReporter("vscCharacteristics", CHARACTERISTICS);
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getNominalV(), "DC nominal voltage"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getR(), "DC resistance"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(characteristicReport, characteristicsReports, "vscCharacteristics", CHARACTERISTICS);

        List<Report> limitsReports = new ArrayList<>();
        Reporter limitsReport = subReporter.createSubReporter("vscLimits", "Limits");
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2(), "Operator active power limit (Side1 -> Side 2)"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1(), "Operator active power limit (Side2 -> Side 1)"));
        ModificationUtils.getInstance().reportModifications(limitsReport, limitsReports, "vscLimits", "Limits");

        List<Report> setPointsReports = new ArrayList<>();
        Reporter setPointsReporter = subReporter.createSubReporter("vscSetPoints", SETPOINTS);
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getActivePowerSetpoint(), "Active power"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(setPointsReporter, setPointsReports, "vscSetPoints", SETPOINTS);

        List<Report> angleDroopActivePowerControlReports = new ArrayList<>();
        angleDroopActivePowerControlReports.add(ModificationUtils.getInstance()
                .createEnabledDisabledReport("angleDroopActivePowerControl", modificationInfos.getAngleDroopActivePowerControl()));

        if (modificationInfos.getP0() != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getP0(), "P0"));
        }

        if (modificationInfos.getDroop() != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getDroop(), "Droop"));
        }

        ModificationUtils.getInstance().reportModifications(setPointsReporter, angleDroopActivePowerControlReports, "vscAngleDroop", "Angle droop active power control");
    }

    private VscConverterStation createConverterStation(Network network,
                                                       ConverterStationCreationInfos converterStationCreationInfos,
                                                       Reporter subReporter,
                                                       String logFieldName) {
        Reporter converterStationReporter = subReporter.createSubReporter("converterStationCreated" + logFieldName, logFieldName);
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, converterStationCreationInfos.getVoltageLevelId());
        VscConverterStation vscConverterStation = voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, converterStationCreationInfos, converterStationReporter) :
                createConverterStationInBusBreaker(voltageLevel, converterStationCreationInfos, converterStationReporter);

        converterStationReporter.report(Report.builder()
                .withKey("converterStationCreated" + logFieldName)
                .withDefaultMessage("New converter station with id=${id} created")
                .withValue("id", converterStationCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (!converterStationCreationInfos.isConnected()) {
            vscConverterStation.getTerminal().disconnect();
        }

        return vscConverterStation;
    }

    private VscConverterStation createConverterStationInNodeBreaker(Network network,
                                                                    VoltageLevel voltageLevel,
                                                                    ConverterStationCreationInfos converterStationCreationInfos,
                                                                    Reporter subReporter) {
        VscConverterStationAdder converterStationAdder = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationInfos.getEquipmentId())
                .setName(converterStationCreationInfos.getEquipmentName())
                .setVoltageRegulatorOn(converterStationCreationInfos.getVoltageRegulationOn());

        if (converterStationCreationInfos.getReactivePowerSetpoint() != null) {
            converterStationAdder.setReactivePowerSetpoint(converterStationCreationInfos.getReactivePowerSetpoint());
        }

        if (converterStationCreationInfos.getLossFactor() != null) {
            converterStationAdder.setLossFactor(converterStationCreationInfos.getLossFactor());
        }

        if (converterStationCreationInfos.getVoltageSetpoint() != null) {
            converterStationAdder.setVoltageSetpoint(converterStationCreationInfos.getVoltageSetpoint());
        }
        int position = ModificationUtils.getInstance().getPosition(converterStationCreationInfos.getConnectionPosition(),
                converterStationCreationInfos.getBusOrBusbarSectionId(),
                network,
                voltageLevel);

        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(converterStationCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(converterStationCreationInfos.getConnectionDirection())
                .withInjectionFeederName(converterStationCreationInfos.getConnectionName() != null
                        ? converterStationCreationInfos.getConnectionName()
                        : converterStationCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(converterStationAdder)
                .build();

        algo.apply(network, true, subReporter);
        VscConverterStation vscConverterStation = ModificationUtils.getInstance()
                .getVscConverterStation(network, converterStationCreationInfos.getEquipmentId());

        addExtensionsAndReports(vscConverterStation,
                converterStationCreationInfos,
                subReporter);

        return vscConverterStation;

    }

    private VscConverterStation createConverterStationInBusBreaker(VoltageLevel voltageLevel,
                                                                   ConverterStationCreationInfos converterStationCreationInfos,
                                                                   Reporter subReporter) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, converterStationCreationInfos.getBusOrBusbarSectionId());
        VscConverterStation vscConverterStation = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationInfos.getEquipmentId())
                .setName(converterStationCreationInfos.getEquipmentName())
                .setVoltageRegulatorOn(converterStationCreationInfos.getVoltageRegulationOn())
                .setReactivePowerSetpoint(converterStationCreationInfos.getReactivePowerSetpoint())
                .setBus(bus.getId())
                .setLossFactor(converterStationCreationInfos.getLossFactor())
                .setVoltageSetpoint(converterStationCreationInfos.getVoltageSetpoint())
                .add();

        addExtensionsAndReports(vscConverterStation, converterStationCreationInfos, subReporter);

        return vscConverterStation;
    }

    private void addExtensionsAndReports(VscConverterStation vscConverterStation,
                                         ConverterStationCreationInfos converterStationCreationInfos,
                                         Reporter subReporter) {
        reportConnectivity(converterStationCreationInfos, subReporter);

        ModificationUtils.getInstance().reportModifications(subReporter,
                List.of(ModificationUtils.getInstance().buildCreationReport(converterStationCreationInfos.getLossFactor(), "Loss Factor")),
                "converterStationCharacteristics",
                CHARACTERISTICS);

        ModificationUtils.getInstance().createReactiveLimits(converterStationCreationInfos, vscConverterStation, subReporter);

        reportConverterStationSetPoints(converterStationCreationInfos, subReporter);
    }

    private void reportConverterStationSetPoints(ConverterStationCreationInfos converterStationCreationInfos, Reporter subReporter) {
        Reporter setPointReporter = subReporter.createSubReporter("converterStationSetPoint", SETPOINTS);
        setPointReporter.report(Report.builder()
                .withKey(SETPOINTS)
                .withDefaultMessage(SETPOINTS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (converterStationCreationInfos.getReactivePowerSetpoint() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(setPointReporter,
                    converterStationCreationInfos.getReactivePowerSetpoint(),
                    "Reactive power");
        }

        List<Report> setPointsVoltageReports = new ArrayList<>();
        setPointsVoltageReports.add(ModificationUtils.getInstance().createEnabledDisabledReport("voltageRegulationOn",
                converterStationCreationInfos.getVoltageRegulationOn()));
        if (converterStationCreationInfos.getVoltageSetpoint() != null) {
            setPointsVoltageReports.add(ModificationUtils.getInstance().buildCreationReport(converterStationCreationInfos.getReactivePowerSetpoint(), "Voltage"));
        }

        ModificationUtils.getInstance().reportModifications(setPointReporter,
                setPointsVoltageReports,
                "converterStationSetPointsVoltageRegulation",
                "Voltage regulation");
    }

    private void reportConnectivity(ConverterStationCreationInfos converterStationCreationInfos, Reporter subReporter) {
        if (converterStationCreationInfos.getConnectionName() == null &&
                converterStationCreationInfos.getConnectionDirection() == null &&
                converterStationCreationInfos.getConnectionPosition() == null) {
            return;
        }

        List<Report> connectivityReports = new ArrayList<>();
        if (converterStationCreationInfos.getConnectionName() != null) {
            connectivityReports.add(ModificationUtils.getInstance()
                    .buildCreationReport(converterStationCreationInfos.getConnectionName(), "Connection name"));
        }
        if (converterStationCreationInfos.getConnectionDirection() != null) {
            connectivityReports.add(ModificationUtils.getInstance()
                    .buildCreationReport(converterStationCreationInfos.getConnectionDirection(), "Connection direction"));
        }
        if (converterStationCreationInfos.getConnectionPosition() != null) {
            connectivityReports.add(ModificationUtils.getInstance()
                    .buildCreationReport(converterStationCreationInfos.getConnectionPosition(), "Connection position"));
        }
        if (!converterStationCreationInfos.isConnected()) {
            connectivityReports.add(Report.builder()
                    .withKey("equipmentDisconnected")
                    .withDefaultMessage("    Equipment with id=${id} disconnected")
                    .withValue("id", converterStationCreationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "ConnectivityCreated", "Connectivity");
    }
}
