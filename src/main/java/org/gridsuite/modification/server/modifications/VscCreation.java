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
import static org.gridsuite.modification.server.modifications.ModificationUtils.createReactiveLimits;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

public class VscCreation extends AbstractModification {
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
        VscConverterStation converterStation1 = modificationInfos.getConverterStation1() == null ?
                null :
                createConvertStation(network, modificationInfos.getConverterStation1(), subReporter, "Converter station 1");

        VscConverterStation converterStation2 = modificationInfos.getConverterStation1() == null ?
                null :
                createConvertStation(network, modificationInfos.getConverterStation2(), subReporter, "Converter station 2");

        HvdcLine hvdcLine = network.newHvdcLine()
                .setId(modificationInfos.getEquipmentId())
                .setName(modificationInfos.getEquipmentName())
                .setNominalV(modificationInfos.getDcNominalVoltage())
                .setR(modificationInfos.getDcResistance())
                .setMaxP(modificationInfos.getMaximumActivePower())
                .setActivePowerSetpoint(modificationInfos.getActivePower())
                .setConvertersMode(modificationInfos.getConvertersMode())
                .setConverterStationId1(converterStation1 != null ? converterStation1.getId() : null)
                .setConverterStationId2(converterStation2 != null ? converterStation2.getId() : null)
                .add();

        if (modificationInfos.getOperatorActivePowerLimitSide1() != null ||
                modificationInfos.getOperatorActivePowerLimitSide2() != null) {
            hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                    .withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitSide1())
                    .withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitSide2())
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

        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReporter, modificationInfos.getEquipmentName(), "Name");
        }
        reportHvdcLineInfos(subReporter);

        subReporter.report(Report.builder()
                .withKey("vscCreated")
                .withDefaultMessage("New vsc with id=${id} created")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void reportHvdcLineInfos(Reporter subReporter) {
        List<Report> characteristicsReports = new ArrayList<>();
        Reporter characteristicReport = subReporter.createSubReporter("vscCharacteristics", "Characteristics");
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getDcNominalVoltage(), "DC nominal voltage"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getDcResistance(), "DC resistance"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaximumActivePower(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(characteristicReport, characteristicsReports, "vscCharacteristics", "Characteristics");

        List<Report> limitsReports = new ArrayList<>();
        Reporter limitsReport = subReporter.createSubReporter("vscLimits", "Limits");
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getOperatorActivePowerLimitSide1(), "Operator active power limit (Side1 -> Side 2)"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getOperatorActivePowerLimitSide2(), "Operator active power limit (Side2 -> Side 1)"));
        ModificationUtils.getInstance().reportModifications(limitsReport, limitsReports, "vscLimits", "Limits");

        List<Report> setPointsReports = new ArrayList<>();
        Reporter setPointsReporter = subReporter.createSubReporter("vscSetPoints", "Setpoints");
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getActivePower(), "Active power"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaximumActivePower(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(setPointsReporter, setPointsReports, "vscSetPoints", "Setpoints");

        List<Report> angleDroopActivePowerControlReports = new ArrayList<>();
        angleDroopActivePowerControlReports.add(Report.builder().withKey("angleDroopActivePowerControl")
                .withDefaultMessage(modificationInfos.getAngleDroopActivePowerControl() ? "enabled" : "disabled")
                        .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        if (modificationInfos.getP0() != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getP0(), "P0"));
        }

        if (modificationInfos.getDroop() != null) {
            angleDroopActivePowerControlReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getDroop(), "Droop"));
        }

        ModificationUtils.getInstance().reportModifications(setPointsReporter, angleDroopActivePowerControlReports, "vscAngleDroop", "Angle droop active power control");
    }

    private VscConverterStation createConvertStation(Network network,
                                                     ConverterStationCreationInfos converterStationCreationInfos,
                                                     Reporter subReporter,
                                                     String logFieldName) {
        Reporter converterStationReporter = subReporter.createSubReporter("converterStationCreated", logFieldName);
        List<Report> converterStationReports = new ArrayList<>();
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, converterStationCreationInfos.getVoltageLevelId());
        VscConverterStation vscConverterStation = voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConvertStationInNodeBreaker(network, voltageLevel, converterStationCreationInfos, converterStationReports, converterStationReporter) :
                createConvertStationInBusBreaker(voltageLevel, converterStationCreationInfos, converterStationReports, converterStationReporter);

        converterStationReporter.report(Report.builder()
                .withKey("converterStationCreated")
                .withDefaultMessage("New converter station with id=${id} created")
                .withValue("id", converterStationCreationInfos.getConverterStationId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        ModificationUtils.getInstance().reportModifications(converterStationReporter,
                converterStationReports,
                logFieldName,
                logFieldName);

        return vscConverterStation;
    }

    private VscConverterStation createConvertStationInNodeBreaker(Network network,
                                                                  VoltageLevel voltageLevel,
                                                                  ConverterStationCreationInfos converterStationCreationInfos,
                                                                  List<Report> converterStationReports,
                                                                  Reporter subReporter) {
        VscConverterStationAdder converterStationAdder = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationInfos.getConverterStationId())
                .setName(converterStationCreationInfos.getConverterStationName())
                .setVoltageRegulatorOn(converterStationCreationInfos.getVoltageRegulationOn());

        if (converterStationCreationInfos.getReactivePower() != null) {
            converterStationAdder.setReactivePowerSetpoint(converterStationCreationInfos.getReactivePower());
        }

        if (converterStationCreationInfos.getLossFactor() != null) {
            converterStationAdder.setLossFactor(converterStationCreationInfos.getLossFactor());
        }

        if (converterStationCreationInfos.getVoltage() != null) {
            converterStationAdder.setVoltageSetpoint(converterStationCreationInfos.getVoltage());
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
                        : converterStationCreationInfos.getConverterStationId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(converterStationAdder)
                .build();

        algo.apply(network, true, subReporter);
        VscConverterStation vscConverterStation = ModificationUtils.getInstance()
                .getVscConverterStation(network, converterStationCreationInfos.getConverterStationId());

        addExtensions(vscConverterStation,
                converterStationCreationInfos,
                converterStationReports,
                subReporter);

        return vscConverterStation;

    }

    private VscConverterStation createConvertStationInBusBreaker(VoltageLevel voltageLevel,
                                                                 ConverterStationCreationInfos converterStationCreationInfos,
                                                                 List<Report> converterStationReports,
                                                                 Reporter subReporter) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, converterStationCreationInfos.getBusOrBusbarSectionId());
        VscConverterStation vscConverterStation = voltageLevel.newVscConverterStation()
                .setId(converterStationCreationInfos.getConverterStationId())
                .setName(converterStationCreationInfos.getConverterStationName())
                .setVoltageRegulatorOn(converterStationCreationInfos.getVoltageRegulationOn())
                .setReactivePowerSetpoint(converterStationCreationInfos.getReactivePower())
                .setBus(bus.getId())
                .setLossFactor(converterStationCreationInfos.getLossFactor())
                .setVoltageSetpoint(converterStationCreationInfos.getVoltage())
                .add();

        addExtensions(vscConverterStation, converterStationCreationInfos, converterStationReports, subReporter);

        return vscConverterStation;
    }

    private void addExtensions(VscConverterStation vscConverterStation,
                               ConverterStationCreationInfos converterStationCreationInfos,
                               List<Report> converterStationReports,
                               Reporter subReporter) {
        reportConnectivity(converterStationCreationInfos, subReporter);

        ModificationUtils.getInstance().reportModifications(subReporter,
                List.of(ModificationUtils.getInstance().buildCreationReport(converterStationCreationInfos.getLossFactor(), "Loss Factor")),
                "converterStationCharacteristics",
                "Characteristics");

        createReactiveLimits(converterStationCreationInfos, vscConverterStation, subReporter);

        List<Report> setPointsReports = new ArrayList<>();
        setPointsReports.add(Report.builder().withKey("voltageRegulationOn")
                .withDefaultMessage(converterStationCreationInfos.getVoltageRegulationOn() ? "Enabled" : "Disables")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        if (converterStationCreationInfos.getReactivePower() != null) {
            setPointsReports.add(ModificationUtils.getInstance()
                    .buildCreationReport(converterStationCreationInfos.getReactivePower(), "Reactive power"));
        }

        if (converterStationCreationInfos.getVoltage() != null) {
            setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(converterStationCreationInfos.getReactivePower(), "Voltage"));
        }

        ModificationUtils.getInstance().reportModifications(subReporter, setPointsReports, "converterStationSetPoints", "Setpoints");
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
        ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "ConnectivityCreated", "Connectivity");
    }
}
