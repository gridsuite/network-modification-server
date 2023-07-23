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
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
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

    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";
    public static final String REACTIVE_LIMITS = "Reactive limits";

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
            if (battery.getReactiveLimits().getKind() == ReactiveLimitsKind.MIN_MAX && (modificationInfos.getMinimumReactivePower() != null || modificationInfos.getMaximumReactivePower() != null)) {
                checkMaxReactivePowerGreaterThanMinReactivePower(battery);
            }
            Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
            List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
            List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
            if (!CollectionUtils.isEmpty(points) && modificationPoints != null) {
                checkMaxQGreaterThanMinQ(batteryPoints, modificationPoints);
            }
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

    private void modifyBatteryReactiveCapabilityCurvePoints(BatteryModificationInfos modificationInfos,
                                                            Battery battery, Reporter subReporter, Reporter subReporterLimits) {

        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReporterLimits);
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

    private void modifyBatteryMinMaxReactiveLimits(BatteryModificationInfos modificationInfos, Battery battery,
                                                   Reporter subReporter, Reporter subReporterLimits) {
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = battery.getReactiveLimits();
        MinMaxReactiveLimitsAdder newMinMaxReactiveLimitsAdder = battery.newMinMaxReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = battery.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }
        ModificationUtils.getInstance().modifyMinMaxReactiveLimits(minMaxReactiveLimits,
                newMinMaxReactiveLimitsAdder, subReporter, subReporterLimits,
                modificationInfos.getMinimumReactivePower(),
                modificationInfos.getMaximumReactivePower());
    }

    private Reporter modifyBatteryActivePowerControlAttributes(BatteryModificationInfos modificationInfos,
                                                               Battery battery, Reporter subReporter, Reporter subReporterSetpoints) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder activePowerControlAdder = battery.newExtension(ActivePowerControlAdder.class);
        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder, modificationInfos.getParticipate(), modificationInfos.getDroop(), subReporter, subReporterSetpoints);
    }
}

