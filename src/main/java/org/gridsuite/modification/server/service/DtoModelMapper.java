package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.model.*;
import org.gridsuite.modification.model.byfilter.assignment.AssignmentModel;
import org.gridsuite.modification.model.byfilter.formula.FormulaModel;
import org.gridsuite.modification.model.tabular.LimitSetsTabularModificationModel;
import org.gridsuite.modification.model.tabular.TabularCreationModel;
import org.gridsuite.modification.model.tabular.TabularModificationModel;
import org.springframework.stereotype.Service;

@Service
public class DtoModelMapper {

    private final ObjectMapper objectMapper;

    public DtoModelMapper(ObjectMapper objectMapper) {
        this.objectMapper = objectMapper;
    }

    private FilterModel map(FilterInfos filterInfos) {
        return FilterModel.builder()
            .id(filterInfos.getId())
            .name(filterInfos.getName())
            .build();
    }

    private FormulaModel map(org.gridsuite.modification.dto.assignment.FormulaInfos formulaInfos) {
        return FormulaModel.builder()
            .id(formulaInfos.getId())
            .filters(formulaInfos.getFilters().stream().map(this::map).toList())
            .editedField(formulaInfos.getEditedField())
            .fieldOrValue1(formulaInfos.getFieldOrValue1())
            .fieldOrValue2(formulaInfos.getFieldOrValue2())
            .operator(formulaInfos.getOperator())
            .build();
    }

    private GeneratorsFrequencyReserveModel map(GeneratorsFrequencyReserveInfos generatorsFrequencyReserveInfos) {
        return GeneratorsFrequencyReserveModel.builder()
            .frequencyReserve(generatorsFrequencyReserveInfos.getFrequencyReserve())
            .generatorsFilters(generatorsFrequencyReserveInfos.getGeneratorsFilters().stream().map(this::map).toList())
            .build();
    }

    public ModificationModel map(ModificationInfos modificationInfos) {
        if (modificationInfos instanceof GenerationDispatchInfos generationDispatchInfos) {
            return GenerationDispatchModel.builder()
                .lossCoefficient(generationDispatchInfos.getLossCoefficient())
                .defaultOutageRate(generationDispatchInfos.getDefaultOutageRate())
                .substationsGeneratorsOrdering(generationDispatchInfos.getSubstationsGeneratorsOrdering())
                .generatorsWithFixedSupply(generationDispatchInfos.getGeneratorsWithFixedSupply().stream().map(this::map).toList())
                .substationsGeneratorsOrdering(generationDispatchInfos.getSubstationsGeneratorsOrdering())
                .generatorsFrequencyReserve(generationDispatchInfos.getGeneratorsFrequencyReserve().stream().map(this::map).toList())
                .build();
        } else if (modificationInfos instanceof GeneratorScalingInfos generatorScalingInfos) {
            return GeneratorScalingModel.builder()
                .variationType(generatorScalingInfos.getVariationType())
                .variations(generatorScalingInfos.getVariations().stream().map(
                    v -> (ScalingVariationModel) ScalingVariationModel.builder()
                        .filters(v.getFilters().stream().map(this::map).toList())
                        .variationMode(v.getVariationMode())
                        .build()).toList())
                .build();
        } else if (modificationInfos instanceof LoadScalingInfos loadScalingInfos) {
            return LoadScalingModel.builder()
                .variationType(loadScalingInfos.getVariationType())
                .variations(loadScalingInfos.getVariations().stream().map(
                    v -> (ScalingVariationModel) ScalingVariationModel.builder()
                        .filters(v.getFilters().stream().map(this::map).toList())
                        .variationMode(v.getVariationMode())
                        .build()).toList())
                .build();
        } else if (modificationInfos instanceof ModificationByAssignmentInfos modificationByAssignmentInfos) {
            return ModificationByAssignmentModel.builder()
                .equipmentType(modificationByAssignmentInfos.getEquipmentType())
                .assignmentModelList(modificationByAssignmentInfos.getAssignmentInfosList().stream().map(
                    a -> AssignmentModel.builder()
                        .value(a.getValue())
                        .editedField(a.getEditedField())
                        .filters(a.getFilters().stream().map(this::map).toList())
                        .build()).toList())
                .build();
        } else if (modificationInfos instanceof ByFilterDeletionInfos byFilterDeletionInfos) {
            return ByFilterDeletionModel.builder()
                .equipmentType(byFilterDeletionInfos.getEquipmentType())
                .filters(byFilterDeletionInfos.getFilters().stream().map(this::map).toList())
                .build();
        } else if (modificationInfos instanceof ByFormulaModificationInfos byFormulaModificationInfos) {
            return ByFormulaModificationModel.builder()
                .identifiableType(byFormulaModificationInfos.getIdentifiableType())
                .formulaModelList(byFormulaModificationInfos.getFormulaInfosList().stream().map(this::map).toList())
                .build();
        } else if (modificationInfos instanceof CompositeModificationInfos compositeModificationInfos) {
            return CompositeModificationModel.builder()
                .name(compositeModificationInfos.getName())
                .maxDepth(compositeModificationInfos.getMaxDepth())
                .modificationsInfos(compositeModificationInfos.getModificationsInfos().stream().map(this::map).toList())
                .build();
        } else if (modificationInfos instanceof LimitSetsTabularModificationInfos limitSetsTabularModificationInfos) {
            return LimitSetsTabularModificationModel.builder()
                .properties(limitSetsTabularModificationInfos.getProperties())
                .csvFilename(limitSetsTabularModificationInfos.getCsvFilename())
                .modificationType(limitSetsTabularModificationInfos.getModificationType())
                .modifications(limitSetsTabularModificationInfos.getModifications().stream().map(this::map).toList())
                .build();
        } else if (modificationInfos instanceof TabularModificationInfos tabularModificationInfos) {
            return TabularModificationModel.builder()
                .properties(tabularModificationInfos.getProperties())
                .csvFilename(tabularModificationInfos.getCsvFilename())
                .modificationType(tabularModificationInfos.getModificationType())
                .modifications(tabularModificationInfos.getModifications().stream().map(this::map).toList())
                .build();
        } else if (modificationInfos instanceof TabularCreationInfos tabularCreationInfos) {
            return TabularCreationModel.builder()
                .properties(tabularCreationInfos.getProperties())
                .csvFilename(tabularCreationInfos.getCsvFilename())
                .modificationType(tabularCreationInfos.getModificationType())
                .modifications(tabularCreationInfos.getModifications().stream().map(this::map).toList())
                .build();
        }
        return objectMapper.convertValue(modificationInfos, getModelClass(modificationInfos));
    }

    @SuppressWarnings("unchecked")
    private Class<? extends ModificationModel> getModelClass(ModificationInfos modificationInfos) {
        String dtoSimpleName = modificationInfos.getClass().getSimpleName();
        String modelSimpleName = dtoSimpleName.substring(0, dtoSimpleName.length() - "Infos".length()) + "Model";
        try {
            return (Class<? extends ModificationModel>) Class.forName("org.gridsuite.modification.model." + modelSimpleName);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("No model class found for DTO class: " + modificationInfos.getClass(), e);
        }
    }
}
