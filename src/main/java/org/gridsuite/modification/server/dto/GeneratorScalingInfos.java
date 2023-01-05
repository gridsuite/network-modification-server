package org.gridsuite.modification.server.dto;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.GeneratorScaling;

@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Generator scaling creation")
public class GeneratorScalingInfos extends ScalingInfos {

    @Schema(description = "is iterative")
    private boolean isIterative;

    @Override
    public GeneratorScalingEntity toEntity() {
        return new GeneratorScalingEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new GeneratorScaling(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.GENERATOR_SCALING_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        System.out.println();
        return reporter.createSubReporter(ModificationType.GENERATOR_SCALING.name(), "Generator scaling");
    }
}
