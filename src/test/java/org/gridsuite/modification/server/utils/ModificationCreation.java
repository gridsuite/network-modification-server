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
import org.gridsuite.modification.dto.*;

import java.util.Arrays;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class ModificationCreation {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    private ModificationCreation() {
        throw new IllegalStateException("Utility class");
    }

    public static VoltageLevelCreationInfos getCreationVoltageLevel(String substationId, String voltageLevelId, String voltageLevelName) {
        return VoltageLevelCreationInfos.builder()
            .stashed(false)
            .equipmentId(voltageLevelId)
            .equipmentName(voltageLevelName)
            .substationId(substationId)
            .nominalV(379.1)
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
                .minP(100.0)
                .maxP(600.0)
                .targetP(400.)
                .targetQ(50.)
                .minQ(20.0)
                .maxQ(25.0)
                .droop(5f)
                .participate(true)
                .reactiveCapabilityCurve(true)
                .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(2.0, 3.0, 3.1),
                        new ReactiveCapabilityCurvePointsInfos(5.6, 9.8, 10.8)))
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
            .minP(100.0)
            .maxP(600.0)
            .ratedS(10.)
            .targetP(400.)
            .targetQ(50.)
            .voltageRegulationOn(true)
            .targetV(225.)
            .stepUpTransformerX(60.0)
            .directTransX(61.0)
            .minQ(20.0)
            .maxQ(25.0)
            .droop(5f)
            .participate(true)
            .regulatingTerminalId(regulatingTerminalId)
            .regulatingTerminalType(regulatingTerminalType)
            .regulatingTerminalVlId(regulatingTerminalVlId)
            .qPercent(25.)
            .reactiveCapabilityCurve(true)
            .reactiveCapabilityCurvePoints(Arrays.asList(new ReactiveCapabilityCurvePointsInfos(2.0, 3.0, 3.1),
                new ReactiveCapabilityCurvePointsInfos(5.6, 9.8, 10.8)))
            .connectionName("top")
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .build();
    }

    public static GeneratorModificationInfos getModificationGenerator(String generatorId, String generatorName) {
        GeneratorModificationInfos.GeneratorModificationInfosBuilder builder = GeneratorModificationInfos.builder()
                .stashed(false)
                .equipmentId(generatorId);

        if (generatorName != null) {
            builder.equipmentName(AttributeModification.toAttributeModification(generatorName, OperationType.SET));
        }

        return builder.build();
    }

    public static LoadCreationInfos getCreationLoad(String vlId, String loadId, String loadName, String busOrBusBarSectionId, LoadType loadType) {
        return LoadCreationInfos.builder()
            .stashed(false)
            .equipmentId(loadId)
            .equipmentName(loadName)
            .voltageLevelId(vlId)
            .busOrBusbarSectionId(busOrBusBarSectionId)
            .loadType(loadType)
            .p0(100.0)
            .q0(20.0)
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
            builder.p0(AttributeModification.toAttributeModification(activePower, OperationType.SET));
        }

        if (reactivePower != null) {
            builder.q0(AttributeModification.toAttributeModification(reactivePower, OperationType.SET));
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

    public static FreePropertyInfos getFreeProperty() {
        return getFreeProperty(PROPERTY_NAME, PROPERTY_VALUE);
    }

    public static FreePropertyInfos getFreeProperty(String name, String value) {
        return FreePropertyInfos.builder().name(name).value(value).build();
    }
}
