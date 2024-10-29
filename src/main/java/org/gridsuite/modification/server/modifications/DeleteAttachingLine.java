/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.RevertCreateLineOnLine;
import com.powsybl.iidm.modification.topology.RevertCreateLineOnLineBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteAttachingLine extends AbstractModification {

    private final DeleteAttachingLineInfos modificationInfos;

    public DeleteAttachingLine(DeleteAttachingLineInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        // check existing lines
        if (network.getLine(modificationInfos.getLineToAttachTo1Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachTo1Id());
        }
        if (network.getLine(modificationInfos.getLineToAttachTo2Id()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachTo2Id());
        }
        if (network.getLine(modificationInfos.getAttachedLineId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getAttachedLineId());
        }
        // check future line does not exist
        if (network.getLine(modificationInfos.getReplacingLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getReplacingLine1Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        RevertCreateLineOnLineBuilder builder = new RevertCreateLineOnLineBuilder();
        RevertCreateLineOnLine algo = builder.withLineToBeMerged1Id(modificationInfos.getLineToAttachTo1Id())
                .withLineToBeMerged2Id(modificationInfos.getLineToAttachTo2Id())
                .withLineToBeDeletedId(modificationInfos.getAttachedLineId())
                .withMergedLineId(modificationInfos.getReplacingLine1Id())
                .withMergedLineName(modificationInfos.getReplacingLine1Name())
                .build();
        algo.apply(network, true, subReportNode);
    }

    @Override
    public String getName() {
        return "DeleteAttachingLine";
    }
}
