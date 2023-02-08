package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.powsybl.iidm.network.extensions.ConnectablePosition;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Connectable position infos")
public class ConnectablePositionInfos {

    private String label;

    private Integer order;

    private ConnectablePosition.Direction direction;
}
