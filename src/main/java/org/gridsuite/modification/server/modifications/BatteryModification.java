/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurve;
import com.powsybl.iidm.network.ReactiveCapabilityCurveAdder;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BatteryModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;

import java.util.Collection;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_BATTERY_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.insertReportNode;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class BatteryModification extends AbstractModification {

    private final BatteryModificationInfos modificationInfos;

    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";

    public BatteryModification(BatteryModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MODIFY_BATTERY_ERROR, "Missing required attributes to modify the equipment");
        }
        Battery battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        String errorMessage = "Battery '" + modificationInfos.getEquipmentId() + "' : ";
        ModificationUtils.getInstance().checkReactiveLimit(battery, modificationInfos.getMinQ(), modificationInfos.getMaxQ(),
                modificationInfos.getReactiveCapabilityCurvePoints(), MODIFY_BATTERY_ERROR, errorMessage);
        checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(modificationInfos, battery, MODIFY_BATTERY_ERROR, errorMessage);
    }

    private void checkActivePowerZeroOrBetweenMinAndMaxActivePowerBattery(BatteryModificationInfos modificationInfos, Battery battery, NetworkModificationException.Type exceptionType, String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                modificationInfos.getTargetP(),
                modificationInfos.getMinP(),
                modificationInfos.getMaxP(),
                battery.getMinP(),
                battery.getMaxP(),
                battery.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        Battery battery = ModificationUtils.getInstance().getBattery(network, modificationInfos.getEquipmentId());
        // modify the battery in the network
        modifyBattery(battery, modificationInfos, subReportNode);
    }

    private void modifyBattery(Battery battery, BatteryModificationInfos modificationInfos, ReportNode subReportNode) {
        subReportNode.newReportNode()
                .withMessageTemplate("batteryModification", "Battery with id=${id} modified :")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(battery::setName, () -> battery.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReportNode, "Name");
        }

        modifyBatteryLimitsAttributes(modificationInfos, battery, subReportNode);
        modifyBatterySetpointsAttributes(modificationInfos, battery, subReportNode);
        ModificationUtils.getInstance().modifyInjectionConnectivityAttributes(modificationInfos, battery, subReportNode);
        PropertiesUtils.applyProperties(battery, subReportNode, modificationInfos.getProperties(), "BatteryProperties");
    }

    private void modifyBatterySetpointsAttributes(BatteryModificationInfos modificationInfos,
                                                  Battery battery, ReportNode subReportNode) {
        ReportNode reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetP, battery::getTargetP, modificationInfos.getTargetP(), "Active power");
        ReportNode reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setTargetQ, battery::getTargetQ, modificationInfos.getTargetQ(), "Reactive power");
        ReportNode subReporterSetpoints = null;
        if (reportActivePower != null || reportReactivePower != null) {
            subReporterSetpoints = subReportNode.newReportNode().withMessageTemplate(SETPOINTS, SETPOINTS).add();
            subReporterSetpoints.newReportNode()
                    .withMessageTemplate(SETPOINTS, SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            if (reportActivePower != null) {
                insertReportNode(subReporterSetpoints, reportActivePower);
            }
            if (reportReactivePower != null) {
                insertReportNode(subReporterSetpoints, reportReactivePower);
            }
        }
        modifyBatteryActivePowerControlAttributes(modificationInfos, battery, subReportNode, subReporterSetpoints);
    }

    private void modifyBatteryLimitsAttributes(BatteryModificationInfos modificationInfos,
                                               Battery battery, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = modifyBatteryActiveLimitsAttributes(modificationInfos, battery, subReportNode);
        modifyBatteryReactiveLimitsAttributes(modificationInfos, battery, subReportNode, subReportNodeLimits);
    }

    private void modifyBatteryReactiveCapabilityCurvePoints(BatteryModificationInfos modificationInfos,
                                                            Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        ReactiveCapabilityCurveAdder adder = battery.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReportNode, subReportNodeLimits);
    }

    private ReportNode modifyBatteryActiveLimitsAttributes(BatteryModificationInfos modificationInfos,
                                                         Battery battery, ReportNode subReportNode) {
        ReportNode subReportNodeLimits = null;
        ReportNode reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setMaxP, battery::getMaxP, modificationInfos.getMaxP(), "Max active power");
        ReportNode reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(battery::setMinP, battery::getMinP, modificationInfos.getMinP(), "Min active power");
        if (reportMaxActivePower != null || reportMinActivePower != null) {
            subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            subReportNodeLimits.newReportNode()
                    .withMessageTemplate(LIMITS, LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

            ReportNode subReporterActiveLimits = subReportNodeLimits.newReportNode().withMessageTemplate(ACTIVE_LIMITS, ACTIVE_LIMITS).add();
            subReporterActiveLimits.newReportNode()
                    .withMessageTemplate(ACTIVE_LIMITS, ACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            if (reportMaxActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                insertReportNode(subReporterActiveLimits, reportMinActivePower);
            }
        }
        return subReportNodeLimits;
    }

    private void modifyBatteryReactiveLimitsAttributes(BatteryModificationInfos modificationInfos,
                                                       Battery battery, ReportNode subReportNode, ReportNode subReportNodeLimits) {

        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyBatteryReactiveCapabilityCurvePoints(modificationInfos, battery, subReportNode, subReportNodeLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                ModificationUtils.getInstance().modifyMinMaxReactiveLimits(modificationInfos.getMinQ(), modificationInfos.getMaxQ(), battery, subReportNode, subReportNodeLimits);
            }
        }
    }

    private ReportNode modifyBatteryActivePowerControlAttributes(BatteryModificationInfos modificationInfos,
                                                               Battery battery, ReportNode subReportNode, ReportNode subReportNodeSetpoints) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder<Battery> activePowerControlAdder = battery.newExtension(ActivePowerControlAdder.class);
        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder, modificationInfos.getParticipate(), modificationInfos.getDroop(), subReportNode, subReportNodeSetpoints);
    }
}

