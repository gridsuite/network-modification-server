package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationMode;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import java.util.List;

@NoArgsConstructor
@Getter
@Setter
@Entity
public class GeneratorScalingVariationEntity {
    @Id
    @Column(name = "id", nullable = false)
    private String id;

    private String filterId;

    private double coefficient;

    private VariationMode variationMode;
}
