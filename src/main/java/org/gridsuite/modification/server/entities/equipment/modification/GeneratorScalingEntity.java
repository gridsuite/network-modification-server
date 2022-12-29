package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "GeneratorScaling")
public class GeneratorScalingEntity extends ModificationEntity {

    @Column(name = "isIterative")
    private boolean isIterative;

    @Column(name = "VariationType")
    @Enumerated(EnumType.STRING)
    private VariationType variationType;

    @OneToMany(cascade = CascadeType.ALL)
    private List<GeneratorScalingVariationEntity> variations;

    public GeneratorScalingEntity(@NotNull GeneratorScalingInfos generatorScalingInfos) {
        super(ModificationType.GENERATOR_SCALING);
        assignAttributes(generatorScalingInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((GeneratorScalingInfos) modificationInfos);
    }

    private void assignAttributes(GeneratorScalingInfos generatorScalingInfos) {
        isIterative = generatorScalingInfos.isIterative();
        variationType = generatorScalingInfos.getVariationType();
        variations = generatorScalingInfos.getGeneratorScalingVariations()
                .stream()
                .map(GeneratorScalingVariation::toEntity)
                .collect(Collectors.toList());
    }

    @Override
    public GeneratorScalingInfos toModificationInfos() {
        return GeneratorScalingInfos.builder()
                .type(ModificationType.GENERATOR_SCALING)
                .date(getDate())
                .uuid(getId())
                .isIterative(isIterative())
                .variationType(getVariationType())
                .generatorScalingVariations(getVariations().stream()
                        .map(GeneratorScalingVariationEntity::toGeneratorScalingVariation)
                        .collect(Collectors.toList()))
                .build();
    }

    @Override
    public void cloneWithIdsToNull() {
        setId(null);
        getVariations().forEach(variation -> {
            variation.setId(null);
            variation.getFilters().forEach(filter -> filter.setId(null));
        });
        this.variations = new ArrayList<>(variations.stream()
                .map(variation -> {
                    variation.setFilters(new ArrayList<>(variation.getFilters()));
                    return variation;
                }).collect(Collectors.toList()));
    }
}
