package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.Entity;
import java.util.List;

@NoArgsConstructor
@Getter
@Entity
public class GeneratorScalingEntity extends ModificationEntity {

    private String id;

    private boolean isIterative;

    private boolean isDeltaP;

    public static GeneratorScalingEntity toEntity() {
        return new GeneratorScalingEntity();
    }
}
