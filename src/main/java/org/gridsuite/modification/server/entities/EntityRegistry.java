/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.*;
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
import org.gridsuite.modification.server.entities.tabular.TabularModificationsEntity;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public final class EntityRegistry {

    // Private constructor to prevent instantiation
    private EntityRegistry() {
        throw new UnsupportedOperationException("Utility class");
    }

    private static final Map<Class<? extends ModificationInfos>, Function<ModificationInfos, ? extends ModificationEntity>> REGISTRY = new HashMap<>();
    private static final Map<ModificationType, Class<? extends ModificationInfos>> DTO_CLASSES = new EnumMap<>(ModificationType.class);
    private static final Map<Class<?>, Function<EquipmentAttributeModificationInfos, ? extends EquipmentAttributeModificationEntity<?>>> ATTRIBUTE_REGISTRY = new HashMap<>();

    static {
        register(ModificationMetadataInfos.class, ModificationMetadataInfos::new, ModificationEntity::new);
        register(SubstationCreationInfos.class, SubstationCreationInfos::new, dto -> new SubstationCreationEntity((ModificationInfos) dto));
        register(VoltageLevelCreationInfos.class, VoltageLevelCreationInfos::new, dto -> new VoltageLevelCreationEntity((ModificationInfos) dto));
        register(LineCreationInfos.class, LineCreationInfos::new, dto -> new LineCreationEntity((ModificationInfos) dto));
        register(TwoWindingsTransformerCreationInfos.class, TwoWindingsTransformerCreationInfos::new, TwoWindingsTransformerCreationEntity::new);
        register(GeneratorCreationInfos.class, GeneratorCreationInfos::new, GeneratorCreationEntity::new);
        register(LoadCreationInfos.class, LoadCreationInfos::new, LoadCreationEntity::new);
        register(BatteryCreationInfos.class, BatteryCreationInfos::new, BatteryCreationEntity::new);
        register(ShuntCompensatorCreationInfos.class, ShuntCompensatorCreationInfos::new, ShuntCompensatorCreationEntity::new);
        register(StaticVarCompensatorCreationInfos.class, StaticVarCompensatorCreationInfos::new, StaticCompensatorCreationEntity::new);
        register(VscCreationInfos.class, VscCreationInfos::new, VscCreationEntity::new);
        register(ConverterStationCreationInfos.class, ConverterStationCreationInfos::new, dto -> new ConverterStationCreationEntity((ModificationInfos) dto));
        register(LccCreationInfos.class, LccCreationInfos::new, LccCreationEntity::new);
        register(LccConverterStationCreationInfos.class, LccConverterStationCreationInfos::new, dto -> new LccConverterStationCreationEntity((ModificationInfos) dto));
        register(CreateVoltageLevelSectionInfos.class, CreateVoltageLevelSectionInfos::new, CreateVoltageLevelSectionEntity::new);

        // // modification
        register(SubstationModificationInfos.class, SubstationModificationInfos::new, SubstationModificationEntity::new);
        register(VoltageLevelModificationInfos.class, VoltageLevelModificationInfos::new, VoltageLevelModificationEntity::new);
        register(LineModificationInfos.class, LineModificationInfos::new, LineModificationEntity::new);
        register(TwoWindingsTransformerModificationInfos.class, TwoWindingsTransformerModificationInfos::new, TwoWindingsTransformerModificationEntity::new);
        register(GeneratorModificationInfos.class, GeneratorModificationInfos::new, GeneratorModificationEntity::new);
        register(LoadModificationInfos.class, LoadModificationInfos::new, LoadModificationEntity::new);
        register(BatteryModificationInfos.class, BatteryModificationInfos::new, BatteryModificationEntity::new);
        register(ShuntCompensatorModificationInfos.class, ShuntCompensatorModificationInfos::new, ShuntCompensatorModificationEntity::new);
        register(VscModificationInfos.class, VscModificationInfos::new, VscModificationEntity::new);
        register(ConverterStationModificationInfos.class, ConverterStationModificationInfos::new, dto -> new ConverterStationModificationEntity((ModificationInfos) dto));
        register(ByFormulaModificationInfos.class, ByFormulaModificationInfos::new, ByFormulaModificationEntity::new);
        register(ModificationByAssignmentInfos.class, ModificationByAssignmentInfos::new, ModificationByAssignmentEntity::new);
        register(EquipmentAttributeModificationInfos.class, EquipmentAttributeModificationInfos::new, dto -> new EquipmentAttributeModificationEntity<>((ModificationInfos) dto));
        register(LccModificationInfos.class, LccModificationInfos::new, LccModificationEntity::new);
        register(LccConverterStationModificationInfos.class, LccConverterStationModificationInfos::new, dto -> new LccConverterStationModificationEntity((ModificationInfos) dto));
        register(VoltageLevelTopologyModificationInfos.class, VoltageLevelTopologyModificationInfos::new, VoltageLevelTopologyModificationEntity::new);
        register(CreateCouplingDeviceInfos.class, CreateCouplingDeviceInfos::new, CreateCouplingDeviceEntity::new);
        register(CreateVoltageLevelTopologyInfos.class, CreateVoltageLevelTopologyInfos::new, CreateVoltageLevelTopologyEntity::new);
        register(MoveVoltageLevelFeederBaysInfos.class, MoveVoltageLevelFeederBaysInfos::new, MoveVoltageLevelFeederBaysEntity::new);

        // tabular
        register(TabularCreationInfos.class, TabularCreationInfos::new, TabularModificationsEntity::new);
        register(TabularModificationInfos.class, TabularModificationInfos::new, TabularModificationsEntity::new);
        register(LimitSetsTabularModificationInfos.class, LimitSetsTabularModificationInfos::new, TabularModificationsEntity::new);

        // // attatching and splitting
        register(LineAttachToVoltageLevelInfos.class, LineAttachToVoltageLevelInfos::new, LineAttachToVoltageLevelEntity::new);
        register(LineSplitWithVoltageLevelInfos.class, LineSplitWithVoltageLevelInfos::new, LineSplitWithVoltageLevelEntity::new);
        register(LinesAttachToSplitLinesInfos.class, LinesAttachToSplitLinesInfos::new, LinesAttachToSplitLinesEntity::new);
        register(DeleteAttachingLineInfos.class, DeleteAttachingLineInfos::new, DeleteAttachingLineEntity::new);
        register(DeleteVoltageLevelOnLineInfos.class, DeleteVoltageLevelOnLineInfos::new, DeleteVoltageLevelOnLineEntity::new);

        // // generation and load
        register(GenerationDispatchInfos.class, GenerationDispatchInfos::new, GenerationDispatchEntity::new);
        register(LoadScalingInfos.class, LoadScalingInfos::new, LoadScalingEntity::new);
        register(GeneratorScalingInfos.class, GeneratorScalingInfos::new, GeneratorScalingEntity::new);

        register(OperatingStatusModificationInfos.class, OperatingStatusModificationInfos::new, OperatingStatusModificationEntity::new);

        register(CompositeModificationInfos.class, CompositeModificationInfos::new, CompositeModificationEntity::new);

        register(VoltageInitModificationInfos.class, VoltageInitModificationInfos::new, VoltageInitModificationEntity::new);

        register(GroovyScriptInfos.class, GroovyScriptInfos::new, GroovyScriptEntity::new);

        register(BalancesAdjustmentModificationInfos.class, BalancesAdjustmentModificationInfos::new, BalancesAdjustmentEntity::new);

        // // deletion
        register(ByFilterDeletionInfos.class, ByFilterDeletionInfos::new, ByFilterDeletionEntity::new);
        register(EquipmentDeletionInfos.class, EquipmentDeletionInfos::new, EquipmentDeletionEntity::new);

        // Register attribute modification entity classes
        registerAttribute(String.class, StringEquipmentAttributeModificationEntity::new);
        registerAttribute(Boolean.class, dto -> new BooleanEquipmentAttributeModificationEntity((ModificationInfos) dto));
        registerAttribute(Integer.class, IntegerEquipmentAttributeModificationEntity::new);
        registerAttribute(Float.class, FloatEquipmentAttributeModificationEntity::new);
        registerAttribute(Double.class, DoubleEquipmentAttributeModificationEntity::new);
    }

    private static <T extends ModificationInfos> void register(Class<T> dtoClass, Supplier<T> dtoFactory,
                                                               Function<T, ? extends ModificationEntity> entityFactory) {
        REGISTRY.put(dtoClass, dto -> entityFactory.apply(dtoClass.cast(dto)));
        ModificationType modificationType = dtoFactory.get().getType();
        DTO_CLASSES.put(modificationType, dtoClass);
    }

    private static void registerAttribute(Class<?> attributeValueClass,
                                          Function<EquipmentAttributeModificationInfos, ? extends EquipmentAttributeModificationEntity<?>> entityFactory) {
        ATTRIBUTE_REGISTRY.put(attributeValueClass, entityFactory);
    }

    public static ModificationEntity createEntity(ModificationInfos dto) {
        Function<ModificationInfos, ? extends ModificationEntity> factory = REGISTRY.get(dto.getClass());
        if (factory == null) {
            throw new IllegalArgumentException("No entity factory registered for DTO class: " + dto.getClass());
        }
        return factory.apply(dto);
    }

    public static Class<? extends ModificationInfos> getDtoClass(ModificationType modificationType) {
        Class<? extends ModificationInfos> dtoClass = DTO_CLASSES.get(modificationType);
        if (dtoClass == null) {
            throw new IllegalArgumentException("No DTO class registered for modification type: " + modificationType);
        }
        return dtoClass;
    }

    public static EquipmentAttributeModificationEntity<?> createAttributeEntity(Class<?> attributeValueClass,
                                                                                  EquipmentAttributeModificationInfos dto) {
        Function<EquipmentAttributeModificationInfos, ? extends EquipmentAttributeModificationEntity<?>> factory = ATTRIBUTE_REGISTRY.get(attributeValueClass);
        if (factory == null) {
            throw new IllegalArgumentException("No entity factory registered for attribute value class: " + attributeValueClass);
        }
        return factory.apply(dto);
    }

}
