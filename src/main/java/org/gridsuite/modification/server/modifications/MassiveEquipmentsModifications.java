/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.MassiveEquipmentsModificationsInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MASSIVE_EQUIPMENTS_MODIFICATIONS_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

public class MassiveEquipmentsModifications extends AbstractModification {
    private static final String EQUIPMENT_MODIFICATION = "EquipmentModification";

    private MassiveEquipmentsModificationsInfos massiveEquipmentsModificationsInfos;

    public MassiveEquipmentsModifications(MassiveEquipmentsModificationsInfos massiveEquipmentsModificationsInfos) {
        this.massiveEquipmentsModificationsInfos = massiveEquipmentsModificationsInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (massiveEquipmentsModificationsInfos == null) {
            throw new NetworkModificationException(MASSIVE_EQUIPMENTS_MODIFICATIONS_ERROR, "No equipment modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        massiveEquipmentsModificationsInfos.getModifications().forEach(m -> {
            Reporter equipmentReporter = subReporter.createSubReporter(EQUIPMENT_MODIFICATION, EQUIPMENT_MODIFICATION);
            m.toModification().apply(network, equipmentReporter);
        });
    }
}
