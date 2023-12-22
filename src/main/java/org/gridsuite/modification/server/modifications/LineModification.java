/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchModificationInfos;
import org.gridsuite.modification.server.dto.LineModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class LineModification extends AbstractBranchModification {

    public LineModification(LineModificationInfos modificationInfos) {
        super(modificationInfos);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        Line line = network.getLine(modificationInfos.getEquipmentId());
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND,
                    "Line " + modificationInfos.getEquipmentId() + " does not exist in network");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Line line = network.getLine(modificationInfos.getEquipmentId());
        // modify the line in the network
        modifyLine(line, modificationInfos, subReporter);
    }

    private void modifyLine(Line line, BranchModificationInfos lineModificationInfos, Reporter subReporter) {
        modifyBranch(line, lineModificationInfos, subReporter, "lineModification", "Line with id=${id} modified :");
    }

    @Override
    protected void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos, Reporter subReporter) {
        Line line = (Line) branch;
        Reporter characteristicsReporter = subReporter.createSubReporter("characteristics", "Characteristics");
        characteristicsReporter.report(Report.builder()
            .withKey("characteristicsModification")
            .withDefaultMessage("Characteristics")
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
        if (branchModificationInfos.getSeriesResistance() != null && branchModificationInfos.getSeriesResistance().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getR(),
                    branchModificationInfos.getSeriesResistance().getValue(), "Series resistance", 1));
            line.setR(branchModificationInfos.getSeriesResistance().getValue());
        }
        if (branchModificationInfos.getSeriesReactance() != null && branchModificationInfos.getSeriesReactance().getValue() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getX(),
                    branchModificationInfos.getSeriesReactance().getValue(), "Series reactance", 1));
            line.setX(branchModificationInfos.getSeriesReactance().getValue());
        }

        LineModificationInfos lineModificationInfos = (LineModificationInfos) branchModificationInfos;
        modifySide1Characteristics(line, lineModificationInfos, characteristicsReporter);
        modifySide2Characteristics(line, lineModificationInfos, characteristicsReporter);

    }

    private void modifySide1Characteristics(Line line, LineModificationInfos lineModificationInfos,
            Reporter characteristicsReporter) {
        if (lineModificationInfos.getShuntConductance1() != null && lineModificationInfos.getShuntConductance1().getValue() != null
                || lineModificationInfos.getShuntSusceptance1() != null && lineModificationInfos.getShuntSusceptance1().getValue() != null) {
            Reporter side1Reporter = characteristicsReporter.createSubReporter("side1Characteristics", "Side 1");
            side1Reporter.report(Report.builder()
                    .withKey("side1CharacteristicsModification")
                    .withDefaultMessage("    Side 1")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (lineModificationInfos.getShuntConductance1() != null && lineModificationInfos.getShuntConductance1().getValue() != null) {
                //convert reported value from siemens to microsiemens
                double shuntConductance1ToReport = lineModificationInfos.getShuntConductance1().getValue() * Math.pow(10, 6);
                double oldShuntConductance1ToReport = line.getG1() * Math.pow(10, 6);
                side1Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldShuntConductance1ToReport,
                    shuntConductance1ToReport, "Shunt conductance", 2));
                line.setG1(lineModificationInfos.getShuntConductance1().getValue());
            }
            if (lineModificationInfos.getShuntSusceptance1() != null && lineModificationInfos.getShuntSusceptance1().getValue() != null) {
                //convert reported value from siemens to microsiemens
                double shuntSusceptance1ToReport = lineModificationInfos.getShuntSusceptance1().getValue() * Math.pow(10, 6);
                double oldShuntSusceptance1ToReport = line.getB1() * Math.pow(10, 6);
                side1Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldShuntSusceptance1ToReport,
                    shuntSusceptance1ToReport, "Shunt susceptance", 2));
                line.setB1(lineModificationInfos.getShuntSusceptance1().getValue());
            }
        }
    }

    private void modifySide2Characteristics(Line line, LineModificationInfos lineModificationInfos,
            Reporter characteristicsReporter) {
        if (lineModificationInfos.getShuntConductance2() != null && lineModificationInfos.getShuntConductance2().getValue() != null
                || lineModificationInfos.getShuntSusceptance2() != null && lineModificationInfos.getShuntSusceptance2().getValue() != null) {
            Reporter side2Reporter = characteristicsReporter.createSubReporter("side2Characteristics", "Side 2");
            side2Reporter.report(Report.builder()
                    .withKey("side2CharacteristicsModification")
                    .withDefaultMessage("    Side 2")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (lineModificationInfos.getShuntConductance2() != null && lineModificationInfos.getShuntConductance2().getValue() != null) {
                // convert reported value from siemens to microsiemens
                double shuntConductance2ToReport = lineModificationInfos.getShuntConductance2().getValue() * Math.pow(10, 6);
                double oldShuntConductance2ToReport = line.getG2() * Math.pow(10, 6);
                side2Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldShuntConductance2ToReport,
                    shuntConductance2ToReport, "Shunt conductance", 2));
                line.setG2(lineModificationInfos.getShuntConductance2().getValue());
            }
            if (lineModificationInfos.getShuntSusceptance2() != null && lineModificationInfos.getShuntSusceptance2().getValue() != null) {
                // convert reported value from siemens to microsiemens
                double shuntSusceptance2ToReport = lineModificationInfos.getShuntSusceptance2().getValue() * Math.pow(10, 6);
                double oldShuntSusceptance2ToReport = line.getB2() * Math.pow(10, 6);
                side2Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(oldShuntSusceptance2ToReport,
                    shuntSusceptance2ToReport, "Shunt susceptance", 2));
                line.setB2(lineModificationInfos.getShuntSusceptance2().getValue());
            }
        }
    }

    @Override
    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        LineModificationInfos lineModificationInfos = (LineModificationInfos) branchModificationInfos;
        return super.characteristicsModified(branchModificationInfos)
                || lineModificationInfos.getShuntConductance1() != null
                        && lineModificationInfos.getShuntConductance1().getValue() != null
                || lineModificationInfos.getShuntSusceptance1() != null
                        && lineModificationInfos.getShuntSusceptance1().getValue() != null
                || lineModificationInfos.getShuntConductance2() != null
                        && lineModificationInfos.getShuntConductance2().getValue() != null
                || lineModificationInfos.getShuntSusceptance2() != null
                        && lineModificationInfos.getShuntSusceptance2().getValue() != null;
    }

}
