/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.RevertConnectVoltageLevelOnLine;
import com.powsybl.iidm.modification.topology.RevertConnectVoltageLevelOnLineBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteVoltageLevelOnLine extends AbstractModification {

    private final DeleteVoltageLevelOnLineInfos modificationInfos;

    public DeleteVoltageLevelOnLine(DeleteVoltageLevelOnLineInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        RevertConnectVoltageLevelOnLineBuilder builder = new RevertConnectVoltageLevelOnLineBuilder();
        RevertConnectVoltageLevelOnLine algo = builder.withLine1Id(modificationInfos.getLineToAttachTo1Id())
                .withLine2Id(modificationInfos.getLineToAttachTo2Id())
                .withLineId(modificationInfos.getReplacingLine1Id())
                .withLineName(modificationInfos.getReplacingLine1Name())
                .build();
        algo.apply(network, true, subReporter);
    }
}
