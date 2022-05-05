/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.LoadType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Load modification")
public class LoadModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Load type modification")
    private AttributeModification<LoadType> loadType;

    @Schema(description = "Active power modification")
    private AttributeModification<Double> activePower;

    @Schema(description = "Reactive power modification")
    private AttributeModification<Double> reactivePower;
}
