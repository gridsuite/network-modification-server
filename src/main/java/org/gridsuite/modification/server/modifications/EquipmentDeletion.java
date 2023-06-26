/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.RemoveFeederBay;
import com.powsybl.iidm.modification.topology.RemoveHvdcLineBuilder;
import com.powsybl.iidm.modification.topology.RemoveHvdcLine;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorSelectionInfos;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.EQUIPMENT_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class EquipmentDeletion extends AbstractModification {

    private final EquipmentDeletionInfos modificationInfos;

    public EquipmentDeletion(EquipmentDeletionInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Identifiable<?> identifiable = ModificationUtils.getInstance().getEquipmentByIdentifiableType(network, modificationInfos.getEquipmentType(), modificationInfos.getEquipmentId());
        if (identifiable == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + modificationInfos.getEquipmentId() + " not found or of bad type");
        }

        if (identifiable instanceof Connectable) {
            new RemoveFeederBay(modificationInfos.getEquipmentId()).apply(network, true, subReporter);
        } else if (identifiable instanceof HvdcLine) {
            removeHvdcLine(network, subReporter);
        } else if (identifiable instanceof VoltageLevel) {
            ((VoltageLevel) identifiable).remove();
        } else if (identifiable instanceof Substation) {
            ((Substation) identifiable).remove();
        }

        subReporter.report(Report.builder()
            .withKey("equipmentDeleted")
            .withDefaultMessage("equipment of type=${type} and id=${id} deleted")
            .withValue("type", modificationInfos.getEquipmentType())
            .withValue("id", modificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    private void removeHvdcLine(Network network, Reporter subReporter) {
        List<String> shuntCompensatorIds = Stream.concat(
                        modificationInfos.getMcsOnSide1() != null ? modificationInfos.getMcsOnSide1().stream() : Stream.of(),
                        modificationInfos.getMcsOnSide2() != null ? modificationInfos.getMcsOnSide2().stream() : Stream.of())
                .filter(mcsInfo -> {
                    if (mcsInfo.isSelected() && network.getShuntCompensator(mcsInfo.getId()) == null) {
                        subReporter.report(Report.builder()
                                .withKey("shuntCompensatorNotDeleted")
                                .withDefaultMessage("Shunt compensator with id=${id} not found in the network")
                                .withValue("id", mcsInfo.getId())
                                .withSeverity(TypedValue.WARN_SEVERITY)
                                .build());
                        return false;
                    } else {
                        return mcsInfo.isSelected();
                    }
                })
                .map(ShuntCompensatorSelectionInfos::getId)
                .collect(Collectors.toList());
        RemoveHvdcLine algo = new RemoveHvdcLineBuilder()
                .withHvdcLineId(modificationInfos.getEquipmentId())
                .withShuntCompensatorIds(shuntCompensatorIds)
                .build();
        algo.apply(network, true, subReporter);
    }
}
