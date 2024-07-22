/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
public class VoltageLevelCreation extends AbstractModification {

    private final VoltageLevelCreationInfos modificationInfos;

    public VoltageLevelCreation(VoltageLevelCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        ModificationUtils.getInstance().controlVoltageLevelCreation(modificationInfos, network);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        ModificationUtils.getInstance().createVoltageLevel(modificationInfos, subReportNode, network);
        // properties
        VoltageLevel voltageLevel = network.getVoltageLevel(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(voltageLevel, subReportNode, modificationInfos.getProperties(), "VlProperties");
    }
}
