/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.ConnectVoltageLevelOnLine;
import com.powsybl.iidm.modification.topology.ConnectVoltageLevelOnLineBuilder;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class LineSplitWithVoltageLevel extends AbstractModification {

    private final LineSplitWithVoltageLevelInfos modificationInfos;

    public LineSplitWithVoltageLevel(LineSplitWithVoltageLevelInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void control(Network network) throws NetworkModificationException {
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(modificationInfos.getMayNewVoltageLevelInfos(),
                modificationInfos.getExistingVoltageLevelId(), modificationInfos.getBbsOrBusId(), network);
        // check future lines don't exist
        if (network.getLine(modificationInfos.getNewLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getNewLine1Id());
        }
        if (network.getLine(modificationInfos.getNewLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getNewLine2Id());
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Line line = network.getLine(modificationInfos.getLineToSplitId());
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToSplitId());
        }

        VoltageLevelCreationInfos mayNewVL = modificationInfos.getMayNewVoltageLevelInfos();
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL, subReporter, network);
        }

        ConnectVoltageLevelOnLine algo = new ConnectVoltageLevelOnLineBuilder()
                .withPositionPercent(modificationInfos.getPercent())
                .withBusbarSectionOrBusId(modificationInfos.getBbsOrBusId())
                .withLine1Id(modificationInfos.getNewLine1Id())
                .withLine1Name(modificationInfos.getNewLine1Name())
                .withLine2Id(modificationInfos.getNewLine2Id())
                .withLine2Name(modificationInfos.getNewLine2Name())
                .withLine(line)
                .build();

        algo.apply(network, true, subReporter);
    }
}
