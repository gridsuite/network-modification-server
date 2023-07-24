/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.TableEquipmentModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.TableEquipmentModification;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Table equipment modification infos")
@JsonTypeName("TABLE_EQUIPMENT_MODIFICATION")
@ModificationErrorTypeName("TABLE_EQUIPMENT_MODIFICATION_ERROR")
public class TableEquipmentModificationInfos extends ModificationInfos {
    @Schema(description = "equipment modifications")
    private List<ModificationInfos> modifications;

    @Override
    public ModificationEntity toEntity() {
        return new TableEquipmentModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new TableEquipmentModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.TABLE_EQUIPMENT_MODIFICATION.name(), "Table equipment modification");
    }
}
