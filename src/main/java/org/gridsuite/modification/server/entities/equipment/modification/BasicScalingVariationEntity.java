package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.AbstractScalingVariationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@SuperBuilder
@MappedSuperclass
public class BasicScalingVariationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @ManyToMany(cascade = CascadeType.ALL)
    @JoinTable(
            joinColumns = @JoinColumn(name = "id"),
            inverseJoinColumns = @JoinColumn(name = "filterId"))
    private List<VariationFilterEntity> filters;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "VariationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;

    public AbstractScalingVariationInfos toScalingVariation() {
        return null;
    }
}
