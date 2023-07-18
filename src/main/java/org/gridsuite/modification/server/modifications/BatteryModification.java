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
import com.powsybl.iidm.network.*;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_BATTERY_ERROR;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class BatteryModification extends AbstractModification {

    private final BatteryModificationInfos modificationInfos;
    private static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    private static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";
    private static final String LIMITS = "Limits";
    private static final String REACTIVE_LIMITS = "Reactive limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";

    public BatteryModification(BatteryModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    private static NetworkModificationException makeBatteryException(String batteryId, String msgSuffix) {
        return new NetworkModificationException(MODIFY_BATTERY_ERROR, "Battery '" + batteryId + "' : " + msgSuffix);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getBattery(modificationInfos.getEquipmentId()) != null) {
            Battery battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
            // check min max reactive limits
            if (battery.getReactiveLimits().getKind() == ReactiveLimitsKind.MIN_MAX && (modificationInfos.getMinimumReactivePower() != null || modificationInfos.getMaximumReactivePower() != null)) {
                checkMaxReactivePowerGreaterThanMinReactivePower(battery);
            }
            // check reactive capability curve limits
            Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
            List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
            List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
            if (!CollectionUtils.isEmpty(points) && modificationPoints != null) {
                checkMaxQGreaterThanMinQ(batteryPoints, modificationPoints);
            }
        }
    }

    private void checkMaxReactivePowerGreaterThanMinReactivePower(Battery battery) {
        MinMaxReactiveLimits minMaxReactiveLimits = battery.getReactiveLimits(MinMaxReactiveLimits.class);
        Double previousMinimumReactivePower = minMaxReactiveLimits.getMinQ();
        Double previousMaximumReactivePower = minMaxReactiveLimits.getMaxQ();
        Double minReactivePower = modificationInfos.getMinimumReactivePower() != null ? modificationInfos.getMinimumReactivePower().getValue() : previousMinimumReactivePower;
        Double maxReactivePower = modificationInfos.getMaximumReactivePower() != null ? modificationInfos.getMaximumReactivePower().getValue() : previousMaximumReactivePower;
        if (minReactivePower > maxReactivePower) {
            throw makeBatteryException(modificationInfos.getEquipmentId(), "maximum reactive power " + maxReactivePower + " is expected to be greater than or equal to minimum reactive power " + minReactivePower);
        }
    }

    private void checkMaxQGreaterThanMinQ(List<ReactiveCapabilityCurve.Point> batteryPoints, List<ReactiveCapabilityCurveModificationInfos> modificationPoints) {
        IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    ReactiveCapabilityCurve.Point oldPoint = batteryPoints.get(i);
                    ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                    Double oldMaxQ = Double.NaN;
                    Double oldMinQ = Double.NaN;
                    if (oldPoint != null) {
                        oldMaxQ = oldPoint.getMaxQ();
                        oldMinQ = oldPoint.getMinQ();
                    }
                    var maxQ = newPoint.getQmaxP() != null ? newPoint.getQmaxP() : oldMaxQ;
                    var minQ = newPoint.getQminP() != null ? newPoint.getQminP() : oldMinQ;
                    if (maxQ < minQ) {
                        throw makeBatteryException(modificationInfos.getEquipmentId(),
                                "maximum reactive power " + maxQ + " is expected to be greater than or equal to minimum reactive power " + minQ);
                    }
                });
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MODIFY_BATTERY_ERROR, "Missing required attributes to modify the equipment");
        }
        Battery battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        // modify the battery in the network
        modifyBattery(battery, modificationInfos, subReporter);
    }

    private void modifyBattery(Battery battery, BatteryModificationInfos modificationInfos, Reporter subReporter) {
        subReporter.report(Report.builder()
                .withKey("batteryModification")
                .withDefaultMessage("Battery with id=${id} modified :")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(battery::setName, () -> battery.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReporter, "Name");
        }

        modifyBatteryLimitsAttributes(modificationInfos, battery, subReporter);
        modifyBatterySetpointsAttributes(modificationInfos, battery, subReporter);
    }

    private void modifyBatterySetpointsAttributes(BatteryModificationInfos modificationInfos,
                                                  Battery battery, Reporter subReporter) {
        Report reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetP, battery::getTargetP, modificationInfos.getActivePowerSetpoint(), "Active power");
        Report reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetQ, battery::getTargetQ, modificationInfos.getReactivePowerSetpoint(), "Reactive power");

        Reporter subReporterSetpoints = null;
        if (reportActivePower != null || reportReactivePower != null) {
            subReporterSetpoints = subReporter.createSubReporter(SETPOINTS, SETPOINTS);
            subReporterSetpoints.report(Report.builder()
                    .withKey(SETPOINTS)
                    .withDefaultMessage(SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (reportActivePower != null) {
                subReporterSetpoints.report(reportActivePower);
            }
            if (reportReactivePower != null) {
                subReporterSetpoints.report(reportReactivePower);
            }
        }
        modifyBatteryActivePowerControlAttributes(modificationInfos, battery, subReporter, subReporterSetpoints);
    }

    private void modifyBatteryLimitsAttributes(BatteryModificationInfos modificationInfos,
                                               Battery battery, Reporter subReporter) {
        Reporter subReporterLimits = modifyBatteryActiveLimitsAttributes(modificationInfos, battery, subReporter);
        modifyBatteryReactiveLimitsAttributes(modificationInfos, battery, subReporter, subReporterLimits);
    }
/*
    private void modifyBatteryMinMaxReactiveLimits(BatteryModificationInfos modificationInfos, Battery battery,
                                                   Reporter subReporter, Reporter subReporterLimits) {
        List<Report> reports = new ArrayList<>();
        // we get previous min max values if they exist
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = battery.getReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = battery.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }

        // (if the min and max reactive limits are null and there is no previous min max
        // limits set we set them to Double max and Double min values)
        // The user can change the value of MinimumReactivePower, MaximumReactivePower or both
        if (modificationInfos.getMinimumReactivePower() != null
                && modificationInfos.getMaximumReactivePower() != null) {
            battery.newMinMaxReactiveLimits().setMinQ(modificationInfos.getMinimumReactivePower().getValue())
                    .setMaxQ(modificationInfos.getMaximumReactivePower().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    modificationInfos.getMinimumReactivePower().getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    modificationInfos.getMaximumReactivePower().getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (modificationInfos.getMinimumReactivePower() != null) {
            battery.newMinMaxReactiveLimits().setMinQ(modificationInfos.getMinimumReactivePower().getValue())
                    .setMaxQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    modificationInfos.getMinimumReactivePower().getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
        } else if (modificationInfos.getMaximumReactivePower() != null) {
            battery.newMinMaxReactiveLimits()
                    .setMinQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : -Double.MAX_VALUE)
                    .setMaxQ(modificationInfos.getMaximumReactivePower().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    modificationInfos.getMaximumReactivePower().getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (minMaxReactiveLimits == null) {
            battery.newMinMaxReactiveLimits().setMinQ(-Double.MAX_VALUE)
                    .setMaxQ(Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(Double.NaN,
                    -Double.MAX_VALUE,
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(Double.NaN,
                    Double.MAX_VALUE,
                    MAX_REACTIVE_POWER_FIELDNAME));
        }

        Reporter subReporterReactiveLimits = null;
        Reporter subReporterLimits2 = subReporterLimits;
        if (subReporterLimits == null && !reports.isEmpty()) {
            subReporterLimits2 = subReporter.createSubReporter(LIMITS, LIMITS);
            subReporterLimits2.report(Report.builder()
                    .withKey(LIMITS)
                    .withDefaultMessage(LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        if (subReporterLimits2 != null && !reports.isEmpty()) {
            subReporterReactiveLimits = subReporterLimits2.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
            subReporterReactiveLimits.report(Report.builder()
                    .withKey(REACTIVE_LIMITS)
                    .withDefaultMessage(REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, reports, "minMaxReactiveLimitsModified", "By range");
    }*/

    private void modifyBatteryReactiveCapabilityCurvePoints(BatteryModificationInfos modificationInfos,
                                                            Battery battery, Reporter subReporter, Reporter subReporterLimits) {
        List<Report> reports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();

        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();

        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);

        IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurve.Point oldPoint = i < batteryPoints.size() - 1 ? batteryPoints.get(i) : null;
                    ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (modificationPoints.size() - 1)) {
                        fieldSuffix = "max";
                        if (!CollectionUtils.isEmpty(batteryPoints)) {
                            oldPoint = batteryPoints.get(batteryPoints.size() - 1);
                        }
                    } else {
                        fieldSuffix = Integer.toString(i);
                    }

                    createReactiveCapabilityCurvePoint(adder, newPoint, oldPoint, reports, fieldSuffix);
                });
        adder.add();

        Reporter subReporterReactiveLimits = null;
        Reporter subReporterLimits2 = subReporterLimits;
        if (subReporterLimits == null && !reports.isEmpty()) {
            subReporterLimits2 = subReporter.createSubReporter(LIMITS, LIMITS);
            subReporterLimits2.report(Report.builder()
                    .withKey(LIMITS)
                    .withDefaultMessage(LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        if (subReporterLimits2 != null && !reports.isEmpty()) {
            subReporterReactiveLimits = subReporterLimits2.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
            subReporterReactiveLimits.report(Report.builder()
                    .withKey(REACTIVE_LIMITS)
                    .withDefaultMessage(REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, reports, "curveReactiveLimitsModified", "By diagram");
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                    ReactiveCapabilityCurveModificationInfos newPoint,
                                                    ReactiveCapabilityCurve.Point oldPoint,
                                                    List<Report> reports,
                                                    String fieldSuffix) {
        Double oldMaxQ = Double.NaN;
        Double oldMinQ = Double.NaN;
        Double oldP = Double.NaN;

        if (oldPoint != null) {
            oldMaxQ = oldPoint.getMaxQ();
            oldMinQ = oldPoint.getMinQ();
            oldP = oldPoint.getP();
        }

        var maxQ = newPoint.getQmaxP() != null ? newPoint.getQmaxP() : oldMaxQ;
        var minQ = newPoint.getQminP() != null ? newPoint.getQminP() : oldMinQ;
        var p = newPoint.getP() != null ? newPoint.getP() : oldP;

        adder.beginPoint()
                .setMaxQ(maxQ)
                .setMinQ(minQ)
                .setP(p)
                .endPoint();

        addToReports(reports, p, oldP, "P" + fieldSuffix);
        addToReports(reports, minQ, oldMinQ, "QminP" + fieldSuffix);
        addToReports(reports, maxQ, oldMaxQ, "QmaxP" + fieldSuffix);
    }

    private void addToReports(List<Report> reports, Double newValue, Double oldValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldValue,
                    newValue,
                    fieldName));
        }
    }

    private Reporter modifyBatteryActiveLimitsAttributes(BatteryModificationInfos modificationInfos,
                                                         Battery battery, Reporter subReporter) {
        Reporter subReporterLimits = null;

        Report reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setMaxP, battery::getMaxP, modificationInfos.getMaxActivePower(), "Max active power");
        Report reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setMinP, battery::getMinP, modificationInfos.getMinActivePower(), "Min active power");
        if (reportMaxActivePower != null || reportMinActivePower != null) {
            subReporterLimits = subReporter.createSubReporter(LIMITS, LIMITS);
            subReporterLimits.report(Report.builder()
                    .withKey(LIMITS)
                    .withDefaultMessage(LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            Reporter subReporterActiveLimits = subReporterLimits.createSubReporter(ACTIVE_LIMITS, ACTIVE_LIMITS);
            subReporterActiveLimits.report(Report.builder()
                    .withKey(ACTIVE_LIMITS)
                    .withDefaultMessage(ACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (reportMaxActivePower != null) {
                subReporterActiveLimits.report(reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                subReporterActiveLimits.report(reportMinActivePower);
            }
        }
        return subReporterLimits;
    }

    private void modifyBatteryReactiveLimitsAttributes(BatteryModificationInfos modificationInfos,
                                                       Battery battery, Reporter subReporter, Reporter subReporterLimits) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyBatteryReactiveCapabilityCurvePoints(modificationInfos, battery, subReporter, subReporterLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                modifyBatteryMinMaxReactiveLimits(modificationInfos, battery, subReporter, subReporterLimits);
            }
        }
    }

    private Reporter modifyBatteryActivePowerControlAttributes(BatteryModificationInfos modificationInfos,
                                                               Battery battery, Reporter subReporter, Reporter subReporterSetpoints) {
        List<Report> reports = new ArrayList<>();

        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        double oldDroop = activePowerControl != null ? activePowerControl.getDroop() : Double.NaN;
        Boolean participate = null;
        // if participate is null and droop was modified, we consider that participate
        // is true
        if (modificationInfos.getParticipate() != null) {
            participate = modificationInfos.getParticipate().getValue();
            reports.add(ModificationUtils.getInstance().buildModificationReport(activePowerControl != null ? activePowerControl.isParticipate() : null,
                    participate,
                    "ON/OFF"));
        } else if (modificationInfos.getDroop() != null) {
            participate = true;
        }
        // if no modification were done to ActivePowerControl or if neither the old nor the new droop values are valid,
        // we don't apply modifications
        if (participate != null) {
            if (Boolean.TRUE.equals(participate)) {
                if (modificationInfos.getDroop() != null) {
                    battery.newExtension(ActivePowerControlAdder.class)
                            .withParticipate(true)
                            .withDroop(modificationInfos.getDroop().getValue())
                            .add();
                    reports.add(ModificationUtils.getInstance().buildModificationReport(oldDroop,
                            modificationInfos.getDroop().getValue(),
                            "Droop"));
                } else {
                    battery.newExtension(ActivePowerControlAdder.class)
                            .withParticipate(true).withDroop(oldDroop)
                            .add();
                }
            } else {
                battery.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(participate).add();
            }
        }
        Reporter subReporterSetpoints2 = subReporterSetpoints;
        if (subReporterSetpoints == null && !reports.isEmpty()) {
            subReporterSetpoints2 = subReporter.createSubReporter(SETPOINTS, SETPOINTS);
            subReporterSetpoints2.report(Report.builder()
                    .withKey(SETPOINTS)
                    .withDefaultMessage(SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }

        ModificationUtils.getInstance().reportModifications(subReporterSetpoints2, reports, "activePowerRegulationModified", "Active power regulation");
        return subReporterSetpoints2;
    }
}

