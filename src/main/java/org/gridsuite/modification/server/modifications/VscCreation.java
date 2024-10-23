/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.VscCreationInfos;

import java.util.ArrayList;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.modifications.ConstantUtil.*;
import static org.gridsuite.modification.server.modifications.ModificationUtils.createInjectionInNodeBreaker;
import static org.gridsuite.modification.server.modifications.ModificationUtils.reportInjectionCreationConnectivity;

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
        checkDroop();
    }

    private void checkDroop() {
        boolean isPresentAngleDroopActivePowerControl = modificationInfos.getAngleDroopActivePowerControl() != null;
        boolean isPresentDroop = modificationInfos.getDroop() != null;
        boolean isPresentP0 = modificationInfos.getP0() != null;
        // all fields are provided => OK extension will be created
        if (isPresentAngleDroopActivePowerControl && isPresentDroop && isPresentP0) {
            return;
        }
        // particular case, not enabling extension and others fields are not provided => OK extension will not be created
        if (Boolean.FALSE.equals(modificationInfos.getAngleDroopActivePowerControl()) && !isPresentDroop && !isPresentP0) {
            return;
        }
        // at least one field is provided but not for the others => NOT OK
        if (isPresentAngleDroopActivePowerControl || isPresentDroop || isPresentP0) {
            throw new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL, ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG);
        }
        // otherwise, i.e. none of the fields is not provided => OK extension will not be created
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
    public void apply(Network network, ReportNode subReportNode) {
        VscConverterStation converterStation1 = createConverterStation(network, modificationInfos.getConverterStation1(), subReportNode, "Converter station 1");

        VscConverterStation converterStation2 = createConverterStation(network, modificationInfos.getConverterStation2(), subReportNode, "Converter station 2");

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

        if (shouldCreateDroopActivePowerControlExtension()) {
            var activePowerControlExtension = hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class)
                    .withEnabled(modificationInfos.getAngleDroopActivePowerControl());
            activePowerControlExtension.withP0(modificationInfos.getP0());
            activePowerControlExtension.withDroop(modificationInfos.getDroop());

            activePowerControlExtension.add();
        }

        reportHvdcLineInfos(subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("vscCreated", "New vsc with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReportNode, modificationInfos.getEquipmentName(), "Name");
        }

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "VscProperties");
    }

    private boolean shouldCreateDroopActivePowerControlExtension() {
        return VscModification.shouldCreateDroopActivePowerControlExtension(
            modificationInfos.getAngleDroopActivePowerControl() != null, modificationInfos.getDroop() != null, modificationInfos.getP0() != null);
    }

    private void reportHvdcLineInfos(ReportNode subReportNode) {
        List<ReportNode> characteristicsReports = new ArrayList<>();
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getNominalV(), "DC nominal voltage"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getR(), "DC resistance"));
        characteristicsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getMaxP(), "Pmax"));
        ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReports, VSC_CHARACTERISTICS, CHARACTERISTICS);

        List<ReportNode> limitsReports = new ArrayList<>();
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2(), "Operator active power limit (Side1 -> Side 2)"));
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1(), "Operator active power limit (Side2 -> Side 1)"));
        ModificationUtils.getInstance().reportModifications(subReportNode, limitsReports, "vscLimits", "Limits");

        List<ReportNode> setPointsReports = new ArrayList<>();
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getConvertersMode(), "Converters mode"));
        setPointsReports.add(ModificationUtils.getInstance().buildCreationReport(modificationInfos.getActivePowerSetpoint(), "Active power"));
        ReportNode setPointsReporter = ModificationUtils.getInstance().reportModifications(subReportNode, setPointsReports, VSC_SETPOINTS, SETPOINTS);

        List<ReportNode> angleDroopActivePowerControlReports = new ArrayList<>();
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
                                                       ReportNode subReportNode,
                                                       String logFieldName) {
        ReportNode converterStationReporter = subReportNode.newReportNode()
            .withMessageTemplate("converterStationCreated", "${fieldName} with id=${id} created")
            .withUntypedValue("fieldName", logFieldName)
            .withUntypedValue("id", converterStationCreationInfos.getEquipmentId())
            .add();
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, converterStationCreationInfos.getVoltageLevelId());
        VscConverterStation vscConverterStation = voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER ?
                createConverterStationInNodeBreaker(network, voltageLevel, converterStationCreationInfos, converterStationReporter) :
                createConverterStationInBusBreaker(voltageLevel, converterStationCreationInfos, converterStationReporter);

        if (!converterStationCreationInfos.isTerminalConnected()) {
            vscConverterStation.getTerminal().disconnect();
        }

        return vscConverterStation;
    }

    private VscConverterStation createConverterStationInNodeBreaker(Network network,
                                                                    VoltageLevel voltageLevel,
                                                                    ConverterStationCreationInfos converterStationCreationInfos,
                                                                    ReportNode subReportNode) {
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
        createInjectionInNodeBreaker(voltageLevel, converterStationCreationInfos, network, converterStationAdder, subReportNode);
        VscConverterStation vscConverterStation = ModificationUtils.getInstance()
                .getVscConverterStation(network, converterStationCreationInfos.getEquipmentId());

        addExtensionsAndReports(vscConverterStation,
                converterStationCreationInfos,
                subReportNode);

        return vscConverterStation;

    }

    private VscConverterStation createConverterStationInBusBreaker(VoltageLevel voltageLevel,
                                                                   ConverterStationCreationInfos converterStationCreationInfos,
                                                                   ReportNode subReportNode) {
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

        addExtensionsAndReports(vscConverterStation, converterStationCreationInfos, subReportNode);

        return vscConverterStation;
    }

    private void addExtensionsAndReports(VscConverterStation vscConverterStation,
                                         ConverterStationCreationInfos converterStationCreationInfos,
                                         ReportNode subReporter) {
        reportInjectionCreationConnectivity(converterStationCreationInfos, subReporter);

        ModificationUtils.getInstance().reportModifications(subReporter,
                List.of(ModificationUtils.getInstance().buildCreationReport(converterStationCreationInfos.getLossFactor(), "Loss Factor")),
                "converterStationCharacteristics",
                CHARACTERISTICS);

        ModificationUtils.getInstance().createReactiveLimits(converterStationCreationInfos, vscConverterStation, subReporter);

        reportConverterStationSetPoints(converterStationCreationInfos, subReporter);
    }

    private void reportConverterStationSetPoints(ConverterStationCreationInfos converterStationCreationInfos, ReportNode subReportNode) {
        ReportNode setPointReporter = subReportNode.newReportNode().withMessageTemplate("converterStationSetPoint", SETPOINTS).add();

        if (converterStationCreationInfos.getReactivePowerSetpoint() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(setPointReporter,
                    converterStationCreationInfos.getReactivePowerSetpoint(),
                    "Reactive power");
        }

        List<ReportNode> setPointsVoltageReports = new ArrayList<>();
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
}
