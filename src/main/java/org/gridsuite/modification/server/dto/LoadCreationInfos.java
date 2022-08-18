/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.LoadType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.creation.EquipmentCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;
import org.gridsuite.modification.server.modifications.LoadCreation;
import org.gridsuite.modification.server.modifications.Modification;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Load creation")
public class LoadCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Load type")
    private LoadType loadType;

    @Schema(description = "Active power")
    private double activePower;

    @Schema(description = "Reactive power")
    private double reactivePower;

    @Override
    public EquipmentCreationEntity toEntity() {
        return new LoadCreationEntity(getEquipmentId(),
            getEquipmentName(),
            getLoadType(),
            getVoltageLevelId(),
            getBusOrBusbarSectionId(),
            getActivePower(),
            getReactivePower());
    }

    @Override
    public Modification toModification() {
        return new LoadCreation(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.CREATE_LOAD_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        String subReportId = "Load creation " + getEquipmentId();
        return reporter.createSubReporter(subReportId, subReportId);
    }
}
