/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.elasticsearch;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Setting;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
@Document(indexName = "#{@environment.getProperty('powsybl-ws.elasticsearch.index.prefix')}tombstoned-equipments")
@Setting(settingPath = "elasticsearch_settings.json")
@TypeAlias(value = "TombstonedEquipmentInfos")
public class TombstonedEquipmentInfos extends BasicEquipmentInfos {
}
