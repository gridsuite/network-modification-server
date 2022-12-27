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
@Schema(description = "Filter Infos")
public class FilterInfos {
    @Schema(description = "id of filter")
    private String id;

    @Schema(description = "name of filter")
    private String name;

    public FilterInfos(String id) {
        this.id = id;
    }
}
