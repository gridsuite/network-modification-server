package org.gridsuite.modification.server.mapper;

import java.util.HashMap;
import java.util.Map;

import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.byfilter.assignment.*;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.springframework.stereotype.Component;

@Component
public final class MapperRegistry {

    private static final MapperRegistry INSTANCE = new MapperRegistry();

    private final Map<Class<?>, EntityMapper<?, ?>> mappers;

    private MapperRegistry() {
        mappers = new HashMap<>();
        // creation
        mappers.put(SubstationCreationInfos.class, new SubstationCreationMapper());
        mappers.put(VoltageLevelCreationInfos.class, new VoltageLevelCreationMapper());
        mappers.put(LineCreationInfos.class, new LineCreationMapper());
        mappers.put(TwoWindingsTransformerCreationInfos.class, new TwoWindingsTransformerCreationMapper());
        mappers.put(GeneratorCreationInfos.class, new GeneratorCreationMapper());
        mappers.put(LoadCreationInfos.class, new LoadCreationMapper());
        mappers.put(BatteryCreationInfos.class, new BatteryCreationMapper());
        mappers.put(ShuntCompensatorCreationInfos.class, new ShuntCompensatorCreationMapper());
        mappers.put(StaticVarCompensatorCreationInfos.class, new StaticVarCompensatorCreationMapper());
        mappers.put(VscCreationInfos.class, new VscCreationMapper());
        mappers.put(ConverterStationCreationInfos.class, new ConverterStationCreationMapper());
        mappers.put(TabularCreationInfos.class, new TabularCreationMapper());

        // modification
        mappers.put(SubstationModificationInfos.class, new SubstationModificationMapper());
        mappers.put(VoltageLevelModificationInfos.class, new VoltageLevelModificationMapper());
        mappers.put(LineModificationInfos.class, new LineModificationMapper());
        mappers.put(TwoWindingsTransformerModificationInfos.class, new TwoWindingsTransformerModificationMapper());
        mappers.put(GeneratorModificationInfos.class, new GeneratorModificationMapper());
        mappers.put(LoadModificationInfos.class, new LoadModificationMapper());
        mappers.put(BatteryModificationInfos.class, new BatteryModificationMapper());
        mappers.put(ShuntCompensatorModificationInfos.class, new ShuntCompensatorModificationMapper());
        mappers.put(VscModificationInfos.class, new VscModificationMapper());
        mappers.put(ConverterStationModificationInfos.class, new ConverterStationModificationMapper());
        mappers.put(TabularModificationInfos.class, new TabularModificationMapper());
        mappers.put(ByFormulaModificationInfos.class, new ByFormulaModificationMapper());
        mappers.put(ModificationByAssignmentInfos.class, new ModificationByAssignmentMapper());

        // attatching and splitting
        mappers.put(LineAttachToVoltageLevelInfos.class, new LineAttachToVoltageLevelMapper());
        mappers.put(LineSplitWithVoltageLevelInfos.class, new LineSplitWithVoltageLevelMapper());
        mappers.put(LinesAttachToSplitLinesInfos.class, new LinesAttachToSplitLinesMapper());
        mappers.put(DeleteAttachingLineInfos.class, new DeleteAttachingLineMapper());
        mappers.put(DeleteVoltageLevelOnLineInfos.class, new DeleteVoltageLevelOnLineMapper());

        // generation and load
        mappers.put(GenerationDispatchInfos.class, new GenerationDispatchMapper());
        mappers.put(LoadScalingInfos.class, new LoadScalingMapper());
        mappers.put(GeneratorScalingInfos.class, new GeneratorScalingMapper());
        mappers.put(ScalingVariationInfos.class, new ScalingVariationMapper());

        mappers.put(OperatingStatusModificationInfos.class, new OperatingStatusModificationMapper());

        mappers.put(CompositeModificationInfos.class, new CompositeModificationMapper());

        mappers.put(VoltageInitModificationInfos.class, new VoltageInitModificationMapper());

        mappers.put(GroovyScriptInfos.class, new GroovyScriptMapper());

        // deletion
        mappers.put(ByFilterDeletionInfos.class, new ByFilterDeletionMapper());
        mappers.put(EquipmentDeletionInfos.class, new EquipmentDeletionMapper());
        mappers.put(HvdcLccDeletionInfos.class, new HvdcLccDeletionMapper());

        // other mappers
        mappers.put(FilterInfos.class, new VariationFilterMapper());
        mappers.put(FormulaInfos.class, new FormulaMapper());
        mappers.put(FreePropertyInfos.class, new FreePropertyMapper());
        mappers.put(CurrentLimitsInfos.class, new CurrentLimitsMapper());
        mappers.put(CurrentLimitsModificationInfos.class, new CurrentLimitsModificationMapper());
        mappers.put(EquipmentAttributeModificationInfos.class, new EquipmentAttributeModificationMapper());

        mappers.put(DoubleAssignmentInfos.class, new AssignmentMapper());
        mappers.put(BooleanAssignmentInfos.class, new AssignmentMapper());
        mappers.put(EnumAssignmentInfos.class, new AssignmentMapper());
        mappers.put(IntegerAssignmentInfos.class, new AssignmentMapper());
        mappers.put(PropertyAssignmentInfos.class, new PropertyAssignmentMapper());

    }

    public static MapperRegistry getInstance() {
        return INSTANCE;
    }

    @SuppressWarnings("unchecked")
    public <D, E> EntityMapper<D, E> getMapper(Class<D> dtoClass) {
        return (EntityMapper<D, E>) mappers.get(dtoClass);
    }
}
