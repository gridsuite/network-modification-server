package org.gridsuite.modification.server.dto;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
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

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Generator scaling creation")
public class GeneratorScalableInfos extends ModificationInfos{

    private List<GeneratorScalingVariation> generatorScalingVariations;
    private boolean isIterative;
    private boolean isDeltaP;

    @Override
    public GeneratorScalingEntity toEntity() {
        return GeneratorScalingEntity.toEntity();
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
        return reporter.createSubReporter(ModificationType.GENERATOR_SCALING.name(), "Generator scaling");
    }
}
