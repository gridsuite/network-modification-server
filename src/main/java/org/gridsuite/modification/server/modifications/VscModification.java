package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
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

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */

public class VscModification extends AbstractModification {
    private final VscModificationInfos modificationInfos;

    public VscModification(VscModificationInfos vscModificationInfos) {
        this.modificationInfos = vscModificationInfos;
    }

    public void checkConverterStation(@NonNull ConverterStationModificationInfos converterStationModificationInfos, @NonNull VscConverterStation vscConverterStation) {
        String errorMessage = "Converter station '" + converterStationModificationInfos.getEquipmentId() + "' : ";
        if (vscConverterStation.getReactiveLimits().getKind() == ReactiveLimitsKind.MIN_MAX && (converterStationModificationInfos.getMinimumReactivePower() != null || converterStationModificationInfos.getMaximumReactivePower() != null)) {
            MinMaxReactiveLimits minMaxReactiveLimits = vscConverterStation.getReactiveLimits(MinMaxReactiveLimits.class);
            ModificationUtils.getInstance().checkMaxReactivePowerGreaterThanMinReactivePower(minMaxReactiveLimits, converterStationModificationInfos.getMinimumReactivePower(), converterStationModificationInfos.getMaximumReactivePower(), MODIFY_VSC_ERROR, errorMessage);
        }

        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = converterStationModificationInfos.getReactiveCapabilityCurvePoints();
        if (modificationPoints != null) {
            ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(modificationPoints, MODIFY_BATTERY_ERROR, errorMessage);
        }
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
    public void apply(Network network, Reporter subReporter) {
        HvdcLine hvdcLine = ModificationUtils.getInstance().getHvdcLine(network, modificationInfos.getEquipmentId());
        modifyVsc(network, hvdcLine, modificationInfos, subReporter);
    }

    private void modifyVsc(@NonNull Network network, @NonNull HvdcLine hvdcLine, VscModificationInfos modificationInfos, Reporter subReporter) {
        subReporter.report(Report.builder()
                .withKey("VscModification")
                .withDefaultMessage("Vsc with id=${id} modified :")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setName, () -> hvdcLine.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReporter, "Name");
        }
        if (modificationInfos.getDcNominalVoltage() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setNominalV, hvdcLine::getNominalV, modificationInfos.getDcNominalVoltage(), subReporter, "dcNominalVoltage");
        }
        if (modificationInfos.getDcResistance() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setR, hvdcLine::getR, modificationInfos.getDcResistance(), subReporter, "R");
        }
        if (modificationInfos.getActivePower() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setActivePowerSetpoint, hvdcLine::getActivePowerSetpoint, modificationInfos.getActivePower(), subReporter, "ActivePowerSetpoint");
        }
        if (modificationInfos.getMaximumActivePower() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setMaxP, hvdcLine::getMaxP, modificationInfos.getMaximumActivePower(), subReporter, "MaxP");
        }
        if (modificationInfos.getConvertersMode() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(hvdcLine::setConvertersMode, hvdcLine::getConvertersMode, modificationInfos.getConvertersMode(), subReporter, "ConvertersMode");
        }

        if (modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2() != null ||
                modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1() != null) {
            hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                    .withOprFromCS1toCS2(modificationInfos.getOperatorActivePowerLimitFromSide1ToSide2().getValue())
                    .withOprFromCS2toCS1(modificationInfos.getOperatorActivePowerLimitFromSide2ToSide1().getValue())
                    .add();
        }

        hvdcAngleDroopActivePowerControlAdder(hvdcLine, subReporter);

        VscConverterStation converterStation1 = ModificationUtils.getInstance().getVscConverterStation(network, modificationInfos.getConverterStation1().getEquipmentId());
        VscConverterStation converterStation2 = ModificationUtils.getInstance().getVscConverterStation(network, modificationInfos.getConverterStation2().getEquipmentId());

        modifyConverterStation(converterStation1, modificationInfos.getConverterStation1(), subReporter);
        modifyConverterStation(converterStation2, modificationInfos.getConverterStation2(), subReporter);

        subReporter.report(Report.builder()
                .withKey("vscModification")
                .withDefaultMessage("Vsc with id=${id} modified :")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

    }

    private void modifyExistingHvdcAngleDroopActivePowerControl(HvdcAngleDroopActivePowerControl hvdcAngleDroopActivePowerControl, List<Report> reports) {
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

    private boolean checkIfChangeRequestedOnDropActiveControl() {
        return modificationInfos.getAngleDroopActivePowerControl() == null
                && modificationInfos.getDroop() == null
                && modificationInfos.getP0() == null;
    }

    private void hvdcAngleDroopActivePowerControlAdder(HvdcLine hvdcLine, Reporter subReporter) {
        List<Report> reports = new ArrayList<>();

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
        reports.forEach(subReporter::report);
    }

    private void modifyConverterStation(VscConverterStation converterStation, ConverterStationModificationInfos converterStationModificationInfos, Reporter subReporter) {
        if (converterStationModificationInfos == null) {
            return;
        }
        if (converterStationModificationInfos.getEquipmentName() != null && converterStationModificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setName, () -> converterStation.getOptionalName().orElse("No value"), converterStationModificationInfos.getEquipmentName(), subReporter, "Name");
        }

        if (converterStationModificationInfos.getLossFactor() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setLossFactor, converterStation::getLossFactor, converterStationModificationInfos.getLossFactor(), subReporter, "LossFactor");
        }

        if (converterStationModificationInfos.getReactivePower() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setReactivePowerSetpoint, converterStation::getReactivePowerSetpoint, converterStationModificationInfos.getReactivePower(), subReporter, "ReactivePower");
        }

        if (converterStationModificationInfos.getVoltageRegulationOn() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setVoltageRegulatorOn, converterStation::isVoltageRegulatorOn, converterStationModificationInfos.getVoltageRegulationOn(), subReporter, "VoltageRegulationOn");
        }

        if (converterStationModificationInfos.getVoltage() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(converterStation::setVoltageSetpoint, converterStation::getVoltageSetpoint, converterStationModificationInfos.getVoltage(), subReporter, "Voltage");
        }

        modifyVscReactiveLimitsAttributes(converterStationModificationInfos, converterStation, subReporter, subReporter);
    }

    private void modifyVscReactiveCapabilityCurvePoints(ConverterStationModificationInfos modificationInfos,
                                                        VscConverterStation vscConverterStation, Reporter subReporter, Reporter subReporterLimits) {

        ReactiveCapabilityCurveAdder adder = vscConverterStation.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? vscConverterStation.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReporterLimits);
    }

    private void modifyVscReactiveLimitsAttributes(ConverterStationModificationInfos modificationInfos,
                                                   VscConverterStation vscConverterStation, Reporter subReporter, Reporter subReporterLimits) {

        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyVscReactiveCapabilityCurvePoints(modificationInfos, vscConverterStation, subReporter, subReporterLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                modifyVscConverterStationMinMaxReactiveLimits(modificationInfos, vscConverterStation, subReporter, subReporterLimits);
            }
        }
    }

    private void modifyVscConverterStationMinMaxReactiveLimits(ConverterStationModificationInfos modificationInfos, VscConverterStation vscConverterStation,
                                                               Reporter subReporter, Reporter subReporterLimits) {
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = vscConverterStation.getReactiveLimits();
        MinMaxReactiveLimitsAdder newMinMaxReactiveLimitsAdder = vscConverterStation.newMinMaxReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = vscConverterStation.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }
        ModificationUtils.getInstance().modifyMinMaxReactiveLimits(minMaxReactiveLimits,
                newMinMaxReactiveLimitsAdder, subReporter, subReporterLimits,
                modificationInfos.getMinimumReactivePower(),
                modificationInfos.getMaximumReactivePower());
    }

}
