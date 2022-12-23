package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationMode;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Id;
import javax.persistence.Table;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "GeneratorScalingVariation")
public class GeneratorScalingVariationEntity {

    @Id
    @Column(name = "id", nullable = false)
    private String id;

    @Column(name = "filterId")
    private String filterId;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "VariationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;
}
