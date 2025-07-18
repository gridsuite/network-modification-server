/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import java.util.HashMap;
import java.util.Map;

import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.entities.equipment.creation.*;
import org.gridsuite.modification.server.entities.equipment.deletion.*;
import org.gridsuite.modification.server.entities.equipment.modification.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public final class EntityRegistry {

    // Private constructor to prevent instantiation
    private EntityRegistry() {
        throw new UnsupportedOperationException("Utility class");
    }

    private static final Map<Class<? extends ModificationInfos>, Class<? extends ModificationEntity>> REGISTRY = new HashMap<>();
    private static final Map<Class<?>, Class<? extends EquipmentAttributeModificationEntity<?>>> ATTRIBUTE_REGISTRY = new HashMap<>();

    static {
        register(SubstationCreationInfos.class, SubstationCreationEntity.class);
        register(VoltageLevelCreationInfos.class, VoltageLevelCreationEntity.class);
        register(LineCreationInfos.class, LineCreationEntity.class);
        register(TwoWindingsTransformerCreationInfos.class, TwoWindingsTransformerCreationEntity.class);
        register(GeneratorCreationInfos.class, GeneratorCreationEntity.class);
        register(LoadCreationInfos.class, LoadCreationEntity.class);
        register(BatteryCreationInfos.class, BatteryCreationEntity.class);
        register(ShuntCompensatorCreationInfos.class, ShuntCompensatorCreationEntity.class);
        register(StaticVarCompensatorCreationInfos.class, StaticCompensatorCreationEntity.class);
        register(VscCreationInfos.class, VscCreationEntity.class);
        register(ConverterStationCreationInfos.class, ConverterStationCreationEntity.class);
        register(LccCreationInfos.class, LccCreationEntity.class);
        register(LccConverterStationCreationInfos.class, LccConverterStationCreationEntity.class);
        register(TabularCreationInfos.class, TabularCreationEntity.class);

        // // modification
        register(SubstationModificationInfos.class, SubstationModificationEntity.class);
        register(VoltageLevelModificationInfos.class, VoltageLevelModificationEntity.class);
        register(LineModificationInfos.class, LineModificationEntity.class);
        register(TwoWindingsTransformerModificationInfos.class, TwoWindingsTransformerModificationEntity.class);
        register(GeneratorModificationInfos.class, GeneratorModificationEntity.class);
        register(LoadModificationInfos.class, LoadModificationEntity.class);
        register(BatteryModificationInfos.class, BatteryModificationEntity.class);
        register(ShuntCompensatorModificationInfos.class, ShuntCompensatorModificationEntity.class);
        register(VscModificationInfos.class, VscModificationEntity.class);
        register(ConverterStationModificationInfos.class, ConverterStationModificationEntity.class);
        register(TabularModificationInfos.class, TabularModificationEntity.class);
        register(LimitSetsTabularModificationInfos.class, TabularModificationEntity.class);
        register(ByFormulaModificationInfos.class, ByFormulaModificationEntity.class);
        register(ModificationByAssignmentInfos.class, ModificationByAssignmentEntity.class);
        register(EquipmentAttributeModificationInfos.class, EquipmentAttributeModificationEntity.class);
        register(LccModificationInfos.class, LccModificationEntity.class);
        register(LccConverterStationModificationInfos.class, LccConverterStationModificationEntity.class);
        register(VoltageLevelTopologyModificationInfos.class, VoltageLevelTopologyModificationEntity.class);
        register(CreateCouplingDeviceInfos.class, CreateCouplingDeviceEntity.class);

        // // attatching and splitting
        register(LineAttachToVoltageLevelInfos.class, LineAttachToVoltageLevelEntity.class);
        register(LineSplitWithVoltageLevelInfos.class, LineSplitWithVoltageLevelEntity.class);
        register(LinesAttachToSplitLinesInfos.class, LinesAttachToSplitLinesEntity.class);
        register(DeleteAttachingLineInfos.class, DeleteAttachingLineEntity.class);
        register(DeleteVoltageLevelOnLineInfos.class, DeleteVoltageLevelOnLineEntity.class);

        // // generation and load
        register(GenerationDispatchInfos.class, GenerationDispatchEntity.class);
        register(LoadScalingInfos.class, LoadScalingEntity.class);
        register(GeneratorScalingInfos.class, GeneratorScalingEntity.class);

        register(OperatingStatusModificationInfos.class, OperatingStatusModificationEntity.class);

        register(CompositeModificationInfos.class, CompositeModificationEntity.class);

        register(VoltageInitModificationInfos.class, VoltageInitModificationEntity.class);

        register(GroovyScriptInfos.class, GroovyScriptEntity.class);

        register(BalancesAdjustmentModificationInfos.class, BalancesAdjustmentEntity.class);

        // // deletion
        register(ByFilterDeletionInfos.class, ByFilterDeletionEntity.class);
        register(EquipmentDeletionInfos.class, EquipmentDeletionEntity.class);

        // Register attribute modification entity classes
        registerAttribute(String.class, StringEquipmentAttributeModificationEntity.class);
        registerAttribute(Boolean.class, BooleanEquipmentAttributeModificationEntity.class);
        registerAttribute(Integer.class, IntegerEquipmentAttributeModificationEntity.class);
        registerAttribute(Float.class, FloatEquipmentAttributeModificationEntity.class);
        registerAttribute(Double.class, DoubleEquipmentAttributeModificationEntity.class);
    }

    public static void register(Class<? extends ModificationInfos> dtoClass, Class<? extends ModificationEntity> entityClass) {
        REGISTRY.put(dtoClass, entityClass);
    }

    public static void registerAttribute(Class<?> attributeValueClass, Class<? extends EquipmentAttributeModificationEntity<?>> entityClass) {
        ATTRIBUTE_REGISTRY.put(attributeValueClass, entityClass);
    }

    public static Class<? extends ModificationEntity> getEntityClass(Class<? extends ModificationInfos> dtoClass) {
        return REGISTRY.get(dtoClass);
    }

    public static Class<? extends EquipmentAttributeModificationEntity<?>> getAttributeEntityClass(Class<?> attributeValueClass) {
        return ATTRIBUTE_REGISTRY.get(attributeValueClass);
    }

}
