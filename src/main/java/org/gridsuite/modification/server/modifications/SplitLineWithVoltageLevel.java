/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.ConnectVoltageLevelOnLine;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class SplitLineWithVoltageLevel extends AbstractModification {

    private final LineSplitWithVoltageLevelInfos modificationInfos;

    public SplitLineWithVoltageLevel(LineSplitWithVoltageLevelInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Line line = network.getLine(modificationInfos.getLineToSplitId());
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToSplitId());
        }

        VoltageLevelCreationInfos mayNewVL = modificationInfos.getMayNewVoltageLevelInfos();
        String voltageLeveId;
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevelAction(mayNewVL, subReporter, network);
            voltageLeveId = mayNewVL.getEquipmentId();
        } else {
            voltageLeveId = modificationInfos.getExistingVoltageLevelId();
        }

        ConnectVoltageLevelOnLine algo = new ConnectVoltageLevelOnLine(
            modificationInfos.getPercent(),
            voltageLeveId,
            modificationInfos.getBbsOrBusId(),
            modificationInfos.getNewLine1Id(),
            modificationInfos.getNewLine1Name(),
            modificationInfos.getNewLine2Id(),
            modificationInfos.getNewLine2Name(),
            line);

        algo.apply(network, false, subReporter);
    }
}
