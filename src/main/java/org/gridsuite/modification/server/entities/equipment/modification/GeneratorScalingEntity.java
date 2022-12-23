package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;

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

    public static GeneratorScalingEntity toEntity() {
        return new GeneratorScalingEntity();
    }
}
