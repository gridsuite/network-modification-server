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
import org.gridsuite.modification.server.ModificationType;
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
    private double p0;

    @Schema(description = "Reactive power")
    private double q0;

    @Schema(description = "Connectable position (for substation diagram)")
    private ConnectablePositionInfos position;

    @Override
    public LoadCreationEntity toEntity() {
        return new LoadCreationEntity(this);
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
        return reporter.createSubReporter(ModificationType.LOAD_CREATION.name(), "Load creation ${loadId}", "loadId", getEquipmentId());
    }
}
