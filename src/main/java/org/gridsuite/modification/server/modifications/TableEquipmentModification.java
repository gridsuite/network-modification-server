/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.TableEquipmentModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABLE_EQUIPMENT_MODIFICATION_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

public class TableEquipmentModification extends AbstractModification {
    private TableEquipmentModificationInfos tableEquipmentModificationInfos;

    public TableEquipmentModification(TableEquipmentModificationInfos tableEquipmentModificationInfos) {
        this.tableEquipmentModificationInfos = tableEquipmentModificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (tableEquipmentModificationInfos == null) {
            throw new NetworkModificationException(TABLE_EQUIPMENT_MODIFICATION_ERROR, "No equipment modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        tableEquipmentModificationInfos.getModifications().forEach(m -> {
            Reporter equipmentReporter = m.createSubReporter((ReporterModel) subReporter);
            m.toModification().apply(network, equipmentReporter);
        });
    }
}
