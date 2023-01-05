package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
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
public class GeneratorScalingEntity extends BasicScalingEntity {

    @Column(name = "isIterative")
    private boolean isIterative;

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
        setVariationType(generatorScalingInfos.getVariationType());
        setVariations(generatorScalingInfos.getVariations()
                .stream()
                .map(variation -> GeneratorScalingVariationInfos.builder()
                        .variationMode(variation.getVariationMode())
                        .variationValue(variation.getVariationValue())
                        .filters(variation.getFilters())
                        .build()
                        .toEntity())
                .collect(Collectors.toList()));
    }

    @Override
    public GeneratorScalingInfos toModificationInfos() {
        return GeneratorScalingInfos.builder()
                .type(ModificationType.GENERATOR_SCALING)
                .date(getDate())
                .uuid(getId())
                .isIterative(isIterative())
                .variationType(getVariationType())
                .variations(getVariations().stream()
                        .map(GeneratorScalingVariationEntity::toScalingVariation)
                        .collect(Collectors.toList()))
                .build();
    }

    @Override
    public void cloneWithIdsToNull() {
        setId(null);
        this.variations = getVariations()
                .stream()
                .peek(variation -> {
                    variation.setId(null);
                    variation.setFilters(new ArrayList<>(variation.getFilters()
                            .stream()
                            .peek(filter -> filter.setId(null))
                            .collect(Collectors.toList())));
                }).collect(Collectors.toList());
    }
}
