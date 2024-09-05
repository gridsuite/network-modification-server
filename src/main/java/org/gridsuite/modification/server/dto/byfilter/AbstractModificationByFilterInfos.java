package org.gridsuite.modification.server.dto.byfilter;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.FilterInfos;

import java.util.List;
import java.util.UUID;

@SuperBuilder
@NoArgsConstructor
@Data
public abstract class AbstractModificationByFilterInfos {
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "List of filters")
    private List<FilterInfos> filters;

    @Schema(description = "Edited field")
    private String editedField;
}
