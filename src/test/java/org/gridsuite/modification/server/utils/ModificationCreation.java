/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.dto.*;
import java.util.Arrays;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class ModificationCreation {

    private ModificationCreation() {
    }

    public static VoltageLevelCreationInfos getCreationVoltageLevel(String substationId, String voltageLevelId, String voltageLevelName) {
        return VoltageLevelCreationInfos.builder()
            .stashed(false)
            .equipmentId(voltageLevelId)
            .equipmentName(voltageLevelName)
            .substationId(substationId)
            .nominalVoltage(379.1)
            .lowVoltageLimit(0.0)
            .highVoltageLimit(10.0)
            .ipMin(0.0)
            .ipMax(10.0)
            .busbarCount(2)
            .sectionCount(2)
            .switchKinds(Arrays.asList(SwitchKind.BREAKER))
            .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("1A").busbarSectionId2("1B").build())).build();
    }

    public static BatteryCreationInfos getCreationBattery(String vlId, String batteryId, String batteryName, String busOrBusbarSectionId) {
        return BatteryCreationInfos.builder()
                .stashed(false)
                .equipmentId(batteryId)
                .equipmentName(batteryName)
                .voltageLevelId(vlId)
                .busOrBusbarSectionId(busOrBusbarSectionId)
                .minActivePower(100.0)
                .maxActivePower(600.0)
                .activePowerSetpoint(400.)
                .reactivePowerSetpoint(50.)
                .minimumReactivePower(20.0)
                .maximumReactivePower(25.0)
                .droop(5f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurveCreationInfos(5.6, 9.8, 10.8)))
                .connectionName("top")
                .connectionDirection(ConnectablePosition.Direction.TOP)
                .build();
    }

    public static GeneratorCreationInfos getCreationGenerator(String vlId, String generatorId, String generatorName, String busOrBusbarSectionId,
                                                              String regulatingTerminalId, String regulatingTerminalType, String regulatingTerminalVlId) {
        return GeneratorCreationInfos.builder()
            .stashed(false)
            .equipmentId(generatorId)
            .equipmentName(generatorName)
            .voltageLevelId(vlId)
            .busOrBusbarSectionId(busOrBusbarSectionId)
            .energySource(EnergySource.HYDRO)
            .minActivePower(100.0)
            .maxActivePower(600.0)
            .ratedNominalPower(10.)
            .activePowerSetpoint(400.)
            .reactivePowerSetpoint(50.)
            .voltageRegulationOn(true)
            .voltageSetpoint(225.)
            .stepUpTransformerReactance(60.0)
            .transientReactance(61.0)
            .minimumReactivePower(20.0)
            .maximumReactivePower(25.0)
            .droop(5f)
            .participate(true)
            .regulatingTerminalId(regulatingTerminalId)
            .regulatingTerminalType(regulatingTerminalType)
            .regulatingTerminalVlId(regulatingTerminalVlId)
            .qPercent(25.)
            .reactiveCapabilityCurve(true)
            .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurveCreationInfos(2.0, 3.0, 3.1),
                new ReactiveCapabilityCurveCreationInfos(5.6, 9.8, 10.8)))
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
    }

    public static LoadCreationInfos getCreationLoad(String vlId, String loadId, String loadName, String busOrBusBarSectionId, LoadType loadType) {
        return LoadCreationInfos.builder()
            .stashed(false)
            .equipmentId(loadId)
            .equipmentName(loadName)
            .voltageLevelId(vlId)
            .busOrBusbarSectionId(busOrBusBarSectionId)
            .loadType(loadType)
            .activePower(100.0)
            .reactivePower(20.0)
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
    }

    public static LoadModificationInfos getModificationLoad(String loadId, String vlId, String loadName, String busOrBusbarSectionId, LoadType loadType, Long activePower, Long reactivePower) {
        LoadModificationInfos.LoadModificationInfosBuilder builder = LoadModificationInfos.builder()
            .stashed(false)
            .equipmentId(loadId);

        if (loadName != null) {
            builder.equipmentName(AttributeModification.toAttributeModification(loadName, OperationType.SET));
        }

        if (vlId != null) {
            builder.voltageLevelId(AttributeModification.toAttributeModification(vlId, OperationType.SET));
        }

        if (busOrBusbarSectionId != null) {
            builder.busOrBusbarSectionId(AttributeModification.toAttributeModification(busOrBusbarSectionId, OperationType.SET));
        }

        if (loadType != null) {
            builder.loadType(AttributeModification.toAttributeModification(LoadType.UNDEFINED, OperationType.SET));
        }

        if (activePower != null) {
            builder.constantActivePower(AttributeModification.toAttributeModification(activePower, OperationType.SET));
        }

        if (reactivePower != null) {
            builder.constantReactivePower(AttributeModification.toAttributeModification(reactivePower, OperationType.SET));
        }

        return builder.build();
    }

    public static VoltageLevelModificationInfos getModificationVoltageLevel(String vlId, String vlName) {
        VoltageLevelModificationInfos.VoltageLevelModificationInfosBuilder builder = VoltageLevelModificationInfos.builder()
            .stashed(false)
            .equipmentId(vlId);

        builder.equipmentName(AttributeModification.toAttributeModification(vlName, OperationType.SET));

        return builder.build();
    }
}
