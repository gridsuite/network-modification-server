/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.topology.CreateLineOnLine;
import com.powsybl.iidm.modification.topology.CreateLineOnLineBuilder;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.LineAdder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ATTACH_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LineAttachToVoltageLevel extends AbstractModification {

    private final LineAttachToVoltageLevelInfos modificationInfos;

    public LineAttachToVoltageLevel(LineAttachToVoltageLevelInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        LineCreationInfos attachmentLineInfos = modificationInfos.getAttachmentLine();
        if (attachmentLineInfos == null) {
            throw new NetworkModificationException(LINE_ATTACH_ERROR, "Missing required attachment line description");
        }

        Line line = network.getLine(modificationInfos.getLineToAttachToId());
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, modificationInfos.getLineToAttachToId());
        }

        VoltageLevelCreationInfos mayNewVL = modificationInfos.getMayNewVoltageLevelInfos();
        if (mayNewVL != null) {
            ModificationUtils.getInstance().createVoltageLevel(mayNewVL, subReporter, network);
        }

        LineAdder lineAdder = network.newLine()
                .setId(attachmentLineInfos.getId())
                .setName(attachmentLineInfos.getName())
                .setR(attachmentLineInfos.getSeriesResistance())
                .setX(attachmentLineInfos.getSeriesReactance())
                .setG1(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getShuntConductance1()))
                .setB1(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getShuntSusceptance1()))
                .setG2(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getShuntConductance2()))
                .setB2(ModificationUtils.getInstance().zeroIfNull(attachmentLineInfos.getShuntSusceptance2()));

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
                .withLine(line)
                .withLineAdder(lineAdder)
                .build();

        algo.apply(network, true, subReporter);
    }
}
