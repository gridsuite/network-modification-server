/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRange;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import io.micrometer.common.lang.NonNull;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ConverterStationModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.gridsuite.modification.server.dto.VscModificationInfos;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_BATTERY_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_VSC_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.insertReportNode;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */

public class VscModification extends AbstractModification {
    private final VscModificationInfos modificationInfos;

    public VscModification(VscModificationInfos vscModificationInfos) {
        this.modificationInfos = vscModificationInfos;
    }

    protected void checkConverterStation(@NonNull ConverterStationModificationInfos converterStationModificationInfos, @NonNull VscConverterStation vscConverterStation) {
        String errorMessage = "Converter station '" + converterStationModificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkReactiveLimit(vscConverterStation, converterStationModificationInfos.getMinQ(), converterStationModificationInfos.getMaxQ(),
                converterStationModificationInfos.getReactiveCapabilityCurvePoints(), MODIFY_VSC_ERROR, errorMessage);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null
                || modificationInfos.getConverterStation1() == null
                || modificationInfos.getConverterStation2() == null) {
            throw new NetworkModificationException(MODIFY_BATTERY_ERROR, "Missing required attributes to modify the equipment");
        }
        VscConverterStation converterStation1 = ModificationUtils.getInstance().getVscConverterStation(network, modificationInfos.getConverterStation1().getEquipmentId());
        VscConverterStation converterStation2 = ModificationUtils.getInstance().getVscConverterStation(network, modificationInfos.getConverterStation2().getEquipmentId());
        checkConverterStation(modificationInfos.getConverterStation1(), converterStation1);
        checkConverterStation(modificationInfos.getConverterStation2(), converterStation2);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());
        modifyVsc(network, hvdcLine, modificationInfos, subReportNode);
    }

    private void modifyVsc(@NonNull Network network, @NonNull HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("VscModification", "Vsc with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setName, () -> hvdcLine.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        }
        if (modificationInfos.getNominalV() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setNominalV, hvdcLine::getNominalV, modificationInfos.getNominalV(), subReportNode, "dcNominalVoltage");
        }
        if (modificationInfos.getR() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setR, hvdcLine::getR, modificationInfos.getR(), subReportNode, "R");
        }
        if (modificationInfos.getActivePowerSetpoint() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setActivePowerSetpoint, hvdcLine::getActivePowerSetpoint, modificationInfos.getActivePowerSetpoint(), subReportNode, "ActivePowerSetpoint");
        }
        if (modificationInfos.getMaxP() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setMaxP, hvdcLine::getMaxP, modificationInfos.getMaxP(), subReportNode, "MaxP");
        }
        if (modificationInfos.getConvertersMode() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setConvertersMode, hvdcLine::getConvertersMode, modificationInfos.getConvertersMode(), subReportNode, "ConvertersMode");
        }

        operatorActivePowerLimit(hvdcLine, modificationInfos, subReportNode);

        hvdcAngleDroopActivePowerControlAdder(hvdcLine, subReportNode);

        modifyConverterStation(network, modificationInfos.getConverterStation1(), subReportNode);
        modifyConverterStation(network, modificationInfos.getConverterStation2(), subReportNode);

        PropertiesUtils.reportPropertiesInfos(hvdcLine, modificationInfos.getProperties(), "VscProperties", subReportNode);
    }

    private static void operatorActivePowerLimit(HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();
        if (modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2() != null ||
                modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1() != null) {
            var operatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
            if (operatorActivePowerRange != null) {
                modifyOperatorActiveRange(modificationInfos, operatorActivePowerRange, reports);

            } else {
                createOperatorActiveRangeExt(hvdcLine, modificationInfos, reports);
            }
        }
        reports.forEach(report -> insertReportNode(subReportNode, report));
    }

    private static void modifyOperatorActiveRange(VscModificationInfos modificationInfos, HvdcOperatorActivePowerRange operatorActivePowerRange, List<ReportNode> reports) {
        var oldCs1ToCs2 = operatorActivePowerRange.getOprFromCS1toCS2();
        var oldCs2ToCs1 = operatorActivePowerRange.getOprFromCS2toCS1();
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS1toCS2(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs1ToCs2, info.getValue(), "OprFromCS1toCS2"));
            }
        });
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS2toCS1(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs2ToCs1, info.getValue(), "OprFromCS2toCS1"));
            }
        });
    }

    private static void createOperatorActiveRangeExt(HvdcLine hvdcLine, VscModificationInfos modificationInfos, List<ReportNode> reports) {
        var hvdcOperatorActivePowerRangeAddr = hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class);
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "OprFromCS1toCS2"));
            }
        });
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "OprFromCS2toCS1"));
            }
        });
        hvdcOperatorActivePowerRangeAddr.add();
    }

    private void modifyExistingHvdcAngleDroopActivePowerControl(HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl, List<ReportNode> reports) {
        var isEnabled = hvdcAngleDroopActivePowerControl.isEnabled();
        var oldDroop = hvdcAngleDroopActivePowerControl.getDroop();
        var oldP0 = hvdcAngleDroopActivePowerControl.getP0();
        Optional.ofNullable(modificationInfos.getAngleDroopActivePowerControl()).ifPresent(info -> {
            if (info.getValue() == null) {
                return;
            }
            hvdcAngleDroopActivePowerControl.setEnabled(info.getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(isEnabled, info.getValue(), "AngleDroopActivePowerControl"));
        });

        Optional.ofNullable(modificationInfos.getDroop()).ifPresent(info -> {
            hvdcAngleDroopActivePowerControl.setDroop(info.getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldDroop, info.getValue(), "Droop"));
        });

        Optional.ofNullable(modificationInfos.getP0()).ifPresent(info -> {
            hvdcAngleDroopActivePowerControl.setP0(info.getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldP0, info.getValue(), "P0"));
        });
    }

    protected boolean checkIfChangeRequestedOnDropActiveControl() {
        return modificationInfos.getAngleDroopActivePowerControl() == null
                && modificationInfos.getDroop() == null
                && modificationInfos.getP0() == null;
    }

    private void hvdcAngleDroopActivePowerControlAdder(HvdcLine hvdcLine, ReportNode subReportNode) {
        List<ReportNode> reports = new ArrayList<>();

        var hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            modifyExistingHvdcAngleDroopActivePowerControl(hvdcAngleDroopActivePowerControl, reports);
        } else {
            var activePowerControlExtension = hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class);

            if (checkIfChangeRequestedOnDropActiveControl()) {
                return;
            }
            boolean isEnabled = modificationInfos.getAngleDroopActivePowerControl() != null && modificationInfos.getAngleDroopActivePowerControl().getValue();
            if (modificationInfos.getAngleDroopActivePowerControl() != null) {
                activePowerControlExtension.withEnabled(isEnabled);
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, isEnabled, "AngleDroopActivePowerControl"));
            }

            var droop = modificationInfos.getDroop() != null ? modificationInfos.getDroop().getValue() : Float.NaN;
            activePowerControlExtension.withDroop(droop);
            if (modificationInfos.getDroop() != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, droop, "Droop"));
            }
            var p0 = modificationInfos.getP0() != null ? modificationInfos.getP0().getValue() : Float.NaN;
            activePowerControlExtension.withP0(p0);
            if (modificationInfos.getP0() != null) {
                reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, p0, "P0"));
            }
            activePowerControlExtension.add();

        }
        reports.forEach(report -> insertReportNode(subReportNode, report));
    }

    private void modifyConverterStation(Network network, ConverterStationModificationInfos converterStationModificationInfos, ReportNode subReportNode) {
        if (converterStationModificationInfos == null) {
            return;
        }
        VscConverterStation converterStation = ModificationUtils.getInstance().getVscConverterStation(network, converterStationModificationInfos.getEquipmentId());
        if (!isConverterStationModified(converterStationModificationInfos)) {
            return;
        }
        ReportNode converterStationReportNode = subReportNode.newReportNode()
                .withMessageTemplate("Converter Station", "Converter station ${id} modified")
                .withUntypedValue("id", converterStation.getId())
                .add();
        converterStationReportNode.newReportNode()
                .withMessageTemplate("converter station modification", "Converter Station with id=${id} modified :")
                .withUntypedValue("id", converterStation.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setName, () -> converterStation.getOptionalName().orElse("No value"), converterStationModificationInfos.getEquipmentName(), converterStationReportNode, "Name");
        }

        if (converterStationModificationInfos.getLossFactor() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setLossFactor, converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), converterStationReportNode, "LossFactor");
        }

        if (converterStationModificationInfos.getReactivePowerSetpoint() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setReactivePowerSetpoint, converterStation::getReactivePowerSetpoint, converterStationModificationInfos.getReactivePowerSetpoint(), converterStationReportNode, "ReactivePower");
        }

        if (converterStationModificationInfos.getVoltageRegulationOn() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setVoltageRegulatorOn, converterStation::isVoltageRegulatorOn, converterStationModificationInfos.getVoltageRegulationOn(), converterStationReportNode, "VoltageRegulationOn");
        }

        if (converterStationModificationInfos.getVoltageSetpoint() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setVoltageSetpoint, converterStation::getVoltageSetpoint, converterStationModificationInfos.getVoltageSetpoint(), converterStationReportNode, "Voltage");
        }

        modifyVscReactiveLimitsAttributes(converterStationModificationInfos, converterStation, converterStationReportNode, converterStationReportNode);
    }

    private static boolean isConverterStationModified(ConverterStationModificationInfos converterStationModificationInfos) {
        return converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null || converterStationModificationInfos.getLossFactor() != null
                || converterStationModificationInfos.getReactivePowerSetpoint() != null
                || converterStationModificationInfos.getVoltageRegulationOn() != null
                || converterStationModificationInfos.getVoltageSetpoint() != null || converterStationModificationInfos.getReactiveCapabilityCurvePoints() != null
                || converterStationModificationInfos.getMinQ() != null || converterStationModificationInfos.getMaxQ() != null;
    }

    private void modifyVscReactiveCapabilityCurvePoints(ConverterStationModificationInfos modificationInfos,
                                                        VscConverterStation vscConverterStation, ReportNode subReporter, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = vscConverterStation.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? vscConverterStation.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReportNodeLimits);
    }

    private void modifyVscReactiveLimitsAttributes(ConverterStationModificationInfos modificationInfos,
                                                   VscConverterStation vscConverterStation, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyVscReactiveCapabilityCurvePoints(modificationInfos, vscConverterStation, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationInfos.getMinQ(), modificationInfos.getMaxQ(), vscConverterStation, subReportNode, subReportNodeLimits);
            }
        }
    }

}
