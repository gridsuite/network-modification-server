/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.Set;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@ToString(callSuper = true)
@ApiModel("Elementary modification attributes")
public class ElementaryModificationInfos extends ModificationInfos {

    @ApiModelProperty("Equipment ID")
    private String equipmentId;

    @ApiModelProperty("Substations ID")
    private Set<String> substationIds;

    @ApiModelProperty("Equipment attribute name")
    private String equipmentAttributeName;

    @ApiModelProperty("Equipment attribute value")
    private Object equipmentAttributeValue;
}
