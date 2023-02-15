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
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LinesAttachToSplitLinesInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LinesAttachToSplitLines extends AbstractModification {

    private final LinesAttachToSplitLinesInfos modificationInfos;

    public LinesAttachToSplitLines(LinesAttachToSplitLinesInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines, vl and busbar
        if (network.getLine(modificationInfos.getLineToAttachTo1Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachTo1Id());
        }
        if (network.getLine(modificationInfos.getLineToAttachTo2Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachTo2Id());
        }
        if (network.getLine(modificationInfos.getAttachedLineId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getAttachedLineId());
        }
        VoltageLevel vl = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        ModificationUtils.getInstance().controlBus(network, vl, modificationInfos.getBbsBusId());
        // check future lines don't exist
        if (network.getLine(modificationInfos.getReplacingLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getReplacingLine1Id());
        }
        if (network.getLine(modificationInfos.getReplacingLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getReplacingLine2Id());
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        ReplaceTeePointByVoltageLevelOnLine algo = new ReplaceTeePointByVoltageLevelOnLineBuilder()
                .withTeePointLine1(modificationInfos.getLineToAttachTo1Id())
                .withTeePointLine2(modificationInfos.getLineToAttachTo2Id())
                .withTeePointLineToRemove(modificationInfos.getAttachedLineId())
                .withBbsOrBusId(modificationInfos.getBbsBusId())
                .withNewLine1Id(modificationInfos.getReplacingLine1Id())
                .withNewLine1Name(modificationInfos.getReplacingLine1Name())
                .withNewLine2Id(modificationInfos.getReplacingLine2Id())
                .withNewLine2Name(modificationInfos.getReplacingLine2Name())
                .build();
        algo.apply(network, true, subReporter);
    }
}
