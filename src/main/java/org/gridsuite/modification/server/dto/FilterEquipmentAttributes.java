package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class FilterEquipmentAttributes {
    @Schema(description = "Equipment ID")
    private String equipmentID;

    @Schema(description = "Distribution Key")
    private Double distributionKey;
}
