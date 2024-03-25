package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.HvdcLine;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.VscModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.VscModification;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "VSC modification")
@JsonTypeName("VSC_MODIFICATION")
@ModificationErrorTypeName("MODIFY_VSC_ERROR")
public class VscModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "DC nominal voltage")
    private AttributeModification<Double> nominalV;

    @Schema(description = "DC resistance")
    private AttributeModification<Double> r;

    @Schema(description = "Maximum active power ")
    private AttributeModification<Double> maxP;

    @Schema(description = "Operator active power limit (Side1->Side2)")
    private AttributeModification<Float> operatorActivePowerLimitFromSide1ToSide2;

    @Schema(description = "Operator active power limit (Side2->Side1)")
    private AttributeModification<Float> operatorActivePowerLimitFromSide2ToSide1;

    @Schema(description = "Converters mode")
    private AttributeModification<HvdcLine.ConvertersMode> convertersMode;

    @Schema(description = "Active power setpoint")
    private AttributeModification<Double> activePowerSetpoint;

    @Schema(description = "Angle droop active power control ")
    private AttributeModification<Boolean> angleDroopActivePowerControl;

    @Schema(description = "p0")
    private AttributeModification<Float> p0;

    @Schema(description = "droop")
    private AttributeModification<Float> droop;

    @Schema(description = "Converter station 1")
    private ConverterStationModificationInfos converterStation1;

    @Schema(description = "Converter station 2")
    private ConverterStationModificationInfos converterStation2;

    @Override
    public VscModificationEntity toEntity() {
        return new VscModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new VscModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Vsc modification ${vscId}", "vscId", this.getEquipmentId());
    }
}
