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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ConverterStationModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.gridsuite.modification.server.dto.VscModificationInfos;

import javax.annotation.Nonnull;
import java.util.*;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.modifications.VscCreation.VSC_CHARACTERISTICS;
import static org.gridsuite.modification.server.modifications.VscCreation.VSC_SETPOINTS;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */

public class VscModification extends AbstractModification {
    public static final String NO_VALUE = "No value";
    public static final String DROOP_FIELD = "Droop";
    public static final String DROOP_AND_P0_FIELD = "Droop and P0";
    public static final String P0_FIELD = "P0";
    public static final String ACTIVE_POWER_CONTROL_EXTENSION_CREATE_ERROR_MESSAGE = "Both %s are required when angle droop active power control is activated to modify the equipment with id=%s";
    public static final String ACTIVE_POWER_CONTROL_EXTENSION_MODIFY_ERROR_MESSAGE = "%s is required to modify the equipment with id=%s";

    private final VscModificationInfos modificationInfos;

    public VscModification(VscModificationInfos vscModificationInfos) {
        this.modificationInfos = vscModificationInfos;
    }

    protected void checkConverterStation(@Nonnull ConverterStationModificationInfos converterStationModificationInfos, @Nonnull VscConverterStation vscConverterStation) {
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
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());

        VscConverterStation converterStation1 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId());
        VscConverterStation converterStation2 = ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId());
        checkConverterStation(modificationInfos.getConverterStation1(), converterStation1);
        checkConverterStation(modificationInfos.getConverterStation2(), converterStation2);
        checkDroopModification(hvdcLine);
    }

    private void checkDroopModification(HvdcLine hvdcLine) {
        // the extension has already existed
        HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            // if droop is set p0 should be also
            if (modificationInfos.getP0() == null && modificationInfos.getDroop() != null) {
                throw new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL,
                        String.format(ACTIVE_POWER_CONTROL_EXTENSION_MODIFY_ERROR_MESSAGE, P0_FIELD, hvdcLine.getId()));
            }
            return;
        }

        // the extension has not yet existed and the modification want to enable the extension =>
        // should verify whether all fields have been filled
        boolean isEnabledAngleDroopActivePowerControl = modificationInfos.getAngleDroopActivePowerControl() != null
                                                        && modificationInfos.getAngleDroopActivePowerControl().getValue();
        if (!isEnabledAngleDroopActivePowerControl) {
            return;
        }

        if (modificationInfos.getDroop() == null || modificationInfos.getP0() == null) {
            throw new NetworkModificationException(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL,
                    String.format(ACTIVE_POWER_CONTROL_EXTENSION_CREATE_ERROR_MESSAGE, DROOP_AND_P0_FIELD, hvdcLine.getId()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());
        modifyVsc(network, hvdcLine, modificationInfos, subReportNode);
    }

    private void modifyVsc(@Nonnull Network network, @Nonnull HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("VscModification", "Vsc with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        // Characteristics
        characteristics(hvdcLine, modificationInfos, subReportNode);

        // Set Points
        //  Set Points
        List<ReportNode> setPointsReports = setPoints(hvdcLine);
        //  hvdc droop
        List<ReportNode> droopReports = hvdcAngleDroopActivePowerControlAdder(hvdcLine);

        if (!setPointsReports.isEmpty() || !droopReports.isEmpty()) {
            ReportNode setPointsReport = subReportNode.newReportNode().withMessageTemplate(VSC_SETPOINTS, SETPOINTS).add();
            if (!setPointsReports.isEmpty()) {
                ModificationUtils.getInstance().reportModifications(setPointsReport, setPointsReports, VSC_SETPOINTS, SETPOINTS, Map.of());
            }
            if (!droopReports.isEmpty()) {
                ModificationUtils.getInstance().reportModifications(setPointsReport, droopReports, "vscAngleDroop", "Angle droop active power control", Map.of());
            }
        }

        // limits
        operatorActivePowerLimit(hvdcLine, modificationInfos, subReportNode);

        // stations
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation1().getId()), modificationInfos.getConverterStation1(), subReportNode);
        modifyConverterStation(ModificationUtils.getInstance().getVscConverterStation(network, hvdcLine.getConverterStation2().getId()), modificationInfos.getConverterStation2(), subReportNode);

        PropertiesUtils.applyProperties(hvdcLine, subReportNode, modificationInfos.getProperties(), "VscProperties");
    }

    private static void characteristics(HvdcLine hvdcLine, VscModificationInfos modificationInfos, ReportNode subReportNode) {
        List<ReportNode> characteristicsReportsContainer = new ArrayList<>();
        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setName,
                () -> hvdcLine.getOptionalName().orElse(NO_VALUE),
                modificationInfos.getEquipmentName(), "Name"));
        }
        if (modificationInfos.getNominalV() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setNominalV, hvdcLine::getNominalV, modificationInfos.getNominalV(), "DC nominal voltage"));
        }
        if (modificationInfos.getR() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setR, hvdcLine::getR, modificationInfos.getR(), "DC resistance"));
        }
        if (modificationInfos.getMaxP() != null) {
            characteristicsReportsContainer.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setMaxP, hvdcLine::getMaxP, modificationInfos.getMaxP(), "Power max"));
        }
        if (!characteristicsReportsContainer.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, characteristicsReportsContainer, VSC_CHARACTERISTICS, CHARACTERISTICS, Map.of());
        }
    }

    private List<ReportNode> setPoints(HvdcLine hvdcLine) {

        List<ReportNode> setPointsReports = new ArrayList<>();
        if (modificationInfos.getActivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setActivePowerSetpoint, hvdcLine::getActivePowerSetpoint, modificationInfos.getActivePowerSetpoint(), "ActivePowerSetpoint"));
        }

        if (modificationInfos.getConvertersMode() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(hvdcLine::setConvertersMode, hvdcLine::getConvertersMode, modificationInfos.getConvertersMode(), "Converters mode"));
        }
        return setPointsReports;
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
        if (!reports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, reports, "vscLimits", "Limits", Map.of());
        }
    }

    private static void modifyOperatorActiveRange(VscModificationInfos modificationInfos, HvdcOperatorActivePowerRange operatorActivePowerRange, List<ReportNode> reports) {
        var oldCs1ToCs2 = operatorActivePowerRange.getOprFromCS1toCS2();
        var oldCs2ToCs1 = operatorActivePowerRange.getOprFromCS2toCS1();
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS1toCS2(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs1ToCs2, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                operatorActivePowerRange.setOprFromCS2toCS1(info.getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(oldCs2ToCs1, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
            }
        });
    }

    private static void createOperatorActiveRangeExt(HvdcLine hvdcLine, VscModificationInfos modificationInfos, List<ReportNode> reports) {
        var hvdcOperatorActivePowerRangeAddr = hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class);
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side1 -> Side 2)"));
            }
        });
        Optional.ofNullable(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1()).ifPresent(info -> {
            if (info.getValue() != null) {
                hvdcOperatorActivePowerRangeAddr.withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1().getValue());
                reports.add(ModificationUtils.getInstance().buildModificationReport(null, info.getValue(), "Operator active power limit (Side2 -> Side 1)"));
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
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldDroop, info.getValue(), DROOP_FIELD));
        });

        Optional.ofNullable(modificationInfos.getP0()).ifPresent(info -> {
            hvdcAngleDroopActivePowerControl.setP0(info.getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldP0, info.getValue(), P0_FIELD));
        });
    }

    protected boolean shouldCreateDroopActivePowerControlExtension() {
        return modificationInfos.getAngleDroopActivePowerControl() != null &&
               modificationInfos.getAngleDroopActivePowerControl().getValue() &&
               modificationInfos.getDroop() != null &&
               modificationInfos.getP0() != null;
    }

    private List<ReportNode> hvdcAngleDroopActivePowerControlAdder(HvdcLine hvdcLine) {
        List<ReportNode> reports = new ArrayList<>();
        var hvdcAngleDroopActivePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        if (hvdcAngleDroopActivePowerControl != null) {
            modifyExistingHvdcAngleDroopActivePowerControl(hvdcAngleDroopActivePowerControl, reports);
        } else if (shouldCreateDroopActivePowerControlExtension()) {
            HvdcAngleDroopActivePowerControlAdder hvdcAngleDroopActivePowerControlAdder =
                hvdcLine.newExtension(HvdcAngleDroopActivePowerControlAdder.class);

            Boolean isEnabled = modificationInfos.getAngleDroopActivePowerControl().getValue();
            hvdcAngleDroopActivePowerControlAdder.withEnabled(isEnabled);
            reports.add(ModificationUtils.getInstance().buildModificationReport(null, isEnabled, "AngleDroopActivePowerControl"));

            Float droop = modificationInfos.getDroop().getValue();
            hvdcAngleDroopActivePowerControlAdder.withDroop(droop);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, droop, DROOP_FIELD));

            Float p0 = modificationInfos.getP0().getValue();
            hvdcAngleDroopActivePowerControlAdder.withP0(p0);
            reports.add(ModificationUtils.getInstance().buildModificationReport(Float.NaN, p0, P0_FIELD));

            hvdcAngleDroopActivePowerControlAdder.add();
        }
        return reports;
    }

    private void modifyConverterStation(VscConverterStation converterStation, ConverterStationModificationInfos converterStationModificationInfos, ReportNode subReportNode) {
        if (converterStationModificationInfos == null || !isConverterStationModified(converterStationModificationInfos)) {
            return;
        }

        ReportNode converterStationReportNode = subReportNode.newReportNode()
            .withMessageTemplate("Converter Station", "Converter station ${id} modified")
            .withUntypedValue("id", converterStation.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .add();

        // characteristic
        List<ReportNode> characteristicReports = new ArrayList<>();
        if (converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setName,
                () -> converterStation.getOptionalName().orElse(NO_VALUE), converterStationModificationInfos.getEquipmentName(), "Name"));
        }

        if (converterStationModificationInfos.getLossFactor() != null) {
            characteristicReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setLossFactor,
                converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), "LossFactor"));
        }

        if (!characteristicReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                characteristicReports, "Characteristics", "Characteristics", Map.of());
        }

        // set points
        List<ReportNode> setPointsReports = new ArrayList<>();
        if (converterStationModificationInfos.getReactivePowerSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setReactivePowerSetpoint,
                converterStation::getReactivePowerSetpoint, converterStationModificationInfos.getReactivePowerSetpoint(), "Reactive Power"));
        }

        if (converterStationModificationInfos.getVoltageRegulationOn() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageRegulatorOn,
                converterStation::isVoltageRegulatorOn, converterStationModificationInfos.getVoltageRegulationOn(), "VoltageRegulationOn"));
        }

        if (converterStationModificationInfos.getVoltageSetpoint() != null) {
            setPointsReports.add(ModificationUtils.getInstance().applyAndBuildModificationReport(converterStation::setVoltageSetpoint,
                converterStation::getVoltageSetpoint, converterStationModificationInfos.getVoltageSetpoint(), "Voltage"));
        }
        if (!setPointsReports.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(converterStationReportNode,
                setPointsReports, SETPOINTS, SETPOINTS, Map.of());
        }

        // limits
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
