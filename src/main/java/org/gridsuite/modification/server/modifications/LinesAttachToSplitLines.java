/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.ReplaceTeePointByVoltageLevelOnLine;
import com.powsybl.iidm.modification.topology.ReplaceTeePointByVoltageLevelOnLineBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.LinesAttachToSplitLinesInfos;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LinesAttachToSplitLines extends AbstractModification {

    private final LinesAttachToSplitLinesInfos modificationInfos;

    public LinesAttachToSplitLines(LinesAttachToSplitLinesInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        String voltageLevelId = modificationInfos.getVoltageLevelId();
        ReplaceTeePointByVoltageLevelOnLineBuilder builder = new ReplaceTeePointByVoltageLevelOnLineBuilder();
        ReplaceTeePointByVoltageLevelOnLine algo = builder.withLine1ZId(modificationInfos.getLineToAttachTo1Id())
                .withLineZ2Id(modificationInfos.getLineToAttachTo2Id())
                .withLineZPId(modificationInfos.getAttachedLineId())
                .withVoltageLevelId(voltageLevelId)
                .withBbsOrBusId(modificationInfos.getBbsBusId())
                .withLine1CId(modificationInfos.getReplacingLine1Id())
                .withLine1CName(modificationInfos.getReplacingLine1Name())
                .withLineC2Id(modificationInfos.getReplacingLine2Id())
                .withLineC2Name(modificationInfos.getReplacingLine2Name())
                .build();
        algo.apply(network, true, subReporter);
    }
}
