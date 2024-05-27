/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.modification.topology.CreateLineOnLine;
import com.powsybl.iidm.modification.topology.CreateLineOnLineBuilder;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LineAttachToVoltageLevel extends AbstractModification {

    private final LineAttachToVoltageLevelInfos modificationInfos;

    public LineAttachToVoltageLevel(LineAttachToVoltageLevelInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getLine(modificationInfos.getLineToAttachToId()) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachToId());
        }
        LineCreationInfos attachmentLineInfos = modificationInfos.getAttachmentLine();
        ModificationUtils.getInstance().controlNewOrExistingVoltageLevel(modificationInfos.getMayNewVoltageLevelInfos(),
                modificationInfos.getExistingVoltageLevelId(), modificationInfos.getBbsOrBusId(), network);
        // new fictitious VL
        if (network.getVoltageLevel(modificationInfos.getAttachmentPointId()) != null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, modificationInfos.getAttachmentPointId());
        }
        // check future lines don't exist
        if (network.getLine(attachmentLineInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, attachmentLineInfos.getEquipmentId());
        }
        if (network.getLine(modificationInfos.getNewLine1Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getNewLine1Id());
        }
        if (network.getLine(modificationInfos.getNewLine2Id()) != null) {
            throw new NetworkModificationException(LINE_ALREADY_EXISTS, modificationInfos.getNewLine2Id());
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        VoltageLevelCreationInfos mayNewVL = modificationInfos.getMayNewVoltageLevelInfos();
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL, subReportNode, network);
        }

        LineCreationInfos attachmentLineInfos = modificationInfos.getAttachmentLine();
        LineAdder lineAdder = network.newLine()
                .setId(attachmentLineInfos.getEquipmentId())
                .setName(attachmentLineInfos.getEquipmentName())
                .setR(attachmentLineInfos.getR())
                .setX(attachmentLineInfos.getX())
                .setG1(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getG1()))
                .setB1(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getB1()))
                .setG2(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getG2()))
                .setB2(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getB2()));

        CreateLineOnLine algo = new CreateLineOnLineBuilder()
                .withPositionPercent(modificationInfos.getPercent())
                .withBusbarSectionOrBusId(modificationInfos.getBbsOrBusId())
                .withFictitiousVoltageLevelId(modificationInfos.getAttachmentPointId())
                .withFictitiousVoltageLevelName(modificationInfos.getAttachmentPointName())
                .withCreateFictitiousSubstation(true)
                .withFictitiousSubstationId(modificationInfos.getAttachmentPointId() + "_substation")
                .withLine1Id(modificationInfos.getNewLine1Id())
                .withLine1Name(modificationInfos.getNewLine1Name())
                .withLine2Id(modificationInfos.getNewLine2Id())
                .withLine2Name(modificationInfos.getNewLine2Name())
                .withLine(network.getLine(modificationInfos.getLineToAttachToId()))
                .withLineAdder(lineAdder)
                .build();

        algo.apply(network, true, subReportNode);
    }
}
