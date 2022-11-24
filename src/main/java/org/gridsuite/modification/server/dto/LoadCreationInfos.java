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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LoadCreation;

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

    @Schema(description = "Connection Name")
    private String connectionName;

    @Schema(description = "Connection Direction")
    private ConnectablePosition.Direction connectionDirection;

    @Schema(description = "Connection Position")
    private Integer connectionPosition;

    @Override
    public LoadCreationEntity toEntity() {
        return new LoadCreationEntity(getEquipmentId(), getEquipmentName(), getLoadType(),
            getVoltageLevelId(), getBusOrBusbarSectionId(),
            getActivePower(), getReactivePower(), getConnectionName(), getConnectionDirection(), getConnectionPosition());
    }

    @Override
    public AbstractModification toModification() {
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
