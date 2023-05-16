/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

import java.util.ArrayList;
import java.util.List;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitModificationInfos;
import org.gridsuite.modification.server.dto.LineModificationInfos;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.CurrentLimits;
import com.powsybl.iidm.network.CurrentLimitsAdder;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.LoadingLimits.TemporaryLimit;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class LineModification extends AbstractModification {

    private static final String DURATION = "duration";
    private static final String NAME = "name";
    private final LineModificationInfos modificationInfos;

    public LineModification(LineModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Line line = network.getLine(modificationInfos.getEquipmentId());
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND,
                    "Line " + modificationInfos.getEquipmentId() + " does not exist in network");
        }
        // modify the line in the network
        modifyLine(line, modificationInfos, subReporter);
    }

    private void modifyLine(Line line, LineModificationInfos lineModificationInfos, Reporter subReporter) {
        subReporter.report(Report.builder()
            .withKey("lineModification")
            .withDefaultMessage("Line with id=${id} modified :")
            .withValue("id", lineModificationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
        if (lineModificationInfos.getEquipmentName() != null) {
            subReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getNameOrId(),
                    lineModificationInfos.getEquipmentName().getValue(), "Name", 0));
            line.setName(lineModificationInfos.getEquipmentName().getValue());
        }

        if (characteristicsModified(lineModificationInfos)) {
            modifyCharacteristics(line, lineModificationInfos, subReporter);
        }

        CurrentLimitsModificationInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsModificationInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        List<Report> side1LimitsReports = new ArrayList<>();
        if (currentLimitsInfos1 != null) {
            CurrentLimits currentLimits1 = line.getCurrentLimits1().orElse(null);
            modifyCurrentLimits(currentLimitsInfos1, line.newCurrentLimits1(), currentLimits1, side1LimitsReports);
        }
        List<Report> side2LimitsReports = new ArrayList<>();
        if (currentLimitsInfos2 != null) {
            CurrentLimits currentLimits2 = line.getCurrentLimits2().orElse(null);
            modifyCurrentLimits(currentLimitsInfos2, line.newCurrentLimits2(), currentLimits2, side2LimitsReports);
        }
        if (!side1LimitsReports.isEmpty() || !side2LimitsReports.isEmpty()) {
            Reporter limitsReporter = subReporter.createSubReporter("limits", "Limits");
            limitsReporter.report(Report.builder()
                    .withKey("limitsModification")
                    .withDefaultMessage("Limits")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            ModificationUtils.getInstance().reportModifications(limitsReporter, side1LimitsReports, "side1LimitsModification",
                    "    Side 1");
            ModificationUtils.getInstance().reportModifications(limitsReporter, side2LimitsReports, "side2LimitsModification",
                    "    Side 2");
        }
    }

    private void modifyCharacteristics(Line line, LineModificationInfos lineModificationInfos, Reporter subReporter) {
        Reporter characteristicsReporter = subReporter.createSubReporter("characteristics", "Characteristics");
        characteristicsReporter.report(Report.builder()
            .withKey("characteristicsModification")
            .withDefaultMessage("Characteristics")
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
        if (lineModificationInfos.getSeriesResistance() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getR(),
                    lineModificationInfos.getSeriesResistance().getValue(), "Series resistance", 1));
            line.setR(lineModificationInfos.getSeriesResistance().getValue());
        }
        if (lineModificationInfos.getSeriesReactance() != null) {
            characteristicsReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getX(),
                    lineModificationInfos.getSeriesReactance().getValue(), "Series reactance", 1));
            line.setX(lineModificationInfos.getSeriesReactance().getValue());
        }
        if (lineModificationInfos.getShuntConductance1() != null
                || lineModificationInfos.getShuntSusceptance1() != null) {
            Reporter side1Reporter = characteristicsReporter.createSubReporter("side1Characteristics", "Side 1");
            side1Reporter.report(Report.builder()
                    .withKey("side1CharacteristicsModification")
                    .withDefaultMessage("    Side 1")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (lineModificationInfos.getShuntConductance1() != null) {
                side1Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getG1(),
                        lineModificationInfos.getShuntConductance1().getValue(), "Shunt conductance", 2));
                line.setG1(lineModificationInfos.getShuntConductance1().getValue());
            }
            if (lineModificationInfos.getShuntSusceptance1() != null) {
                side1Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getB1(),
                        lineModificationInfos.getShuntSusceptance1().getValue(), "Shunt susceptance", 2));
                line.setB1(lineModificationInfos.getShuntSusceptance1().getValue());
            }
        }

        if (lineModificationInfos.getShuntConductance2() != null
                || lineModificationInfos.getShuntSusceptance2() != null) {
            Reporter side2Reporter = characteristicsReporter.createSubReporter("side2Characteristics", "Side 2");
            side2Reporter.report(Report.builder()
                    .withKey("side2CharacteristicsModification")
                    .withDefaultMessage("    Side 2")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (lineModificationInfos.getShuntConductance2() != null) {
                side2Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getG2(),
                        lineModificationInfos.getShuntConductance2().getValue(), "Shunt conductance", 2));
                line.setG2(lineModificationInfos.getShuntConductance2().getValue());
            }
            if (lineModificationInfos.getShuntSusceptance2() != null) {
                side2Reporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(line.getB2(),
                        lineModificationInfos.getShuntSusceptance2().getValue(), "Shunt susceptance", 2));
                line.setB2(lineModificationInfos.getShuntSusceptance2().getValue());
            }
        }
    }

    private void modifyCurrentLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, List<Report> limitsReports) {
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        boolean hasTemporary = currentLimitsInfos.getTemporaryLimits() != null
                && !currentLimitsInfos.getTemporaryLimits().isEmpty();
        if (hasPermanent) {
            limitsReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN,
                    currentLimitsInfos.getPermanentLimit(), "IST", 2));
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        if (hasTemporary) {
            modifyTemporaryLimits(currentLimitsInfos, limitsAdder, currentLimits, limitsReports);
        }
        if (hasPermanent || hasTemporary) {
            limitsAdder.add();
        }
    }

    private void modifyTemporaryLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder,
            CurrentLimits currentLimits, List<Report> limitsReports) {
        // we create a mutable list of temporary limits to be able to remove the limits that are modified
        List<TemporaryLimit> lineTemporaryLimits = null;
        if (currentLimits != null) {
            lineTemporaryLimits = new ArrayList<>(currentLimits.getTemporaryLimits());
        }
        List<Report> temporaryLimitsReports = new ArrayList<>();
        for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
            int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
            double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
            TemporaryLimit limitToModify = null;
            if (currentLimits != null) {
                limitToModify = currentLimits.getTemporaryLimit(limitAcceptableDuration);
                // we remove the limit to modify from the list of temporary limits so we can log the remaining ones (deleted)
                lineTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
            }
            if (limitToModify == null) {
                temporaryLimitsReports.add(Report.builder().withKey("temporaryLimitAdded" + limit.getName())
                        .withDefaultMessage("            ${name} (${duration}) added with ${value}")
                        .withValue(NAME, limit.getName())
                        .withValue(DURATION, limitAcceptableDuration)
                        .withValue("value", limitValue)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

            } else if (Double.compare(limitToModify.getValue(), limitValue) != 0) {
                temporaryLimitsReports.add(Report.builder()
                        .withKey("temporaryLimitModified" + limit.getName())
                        .withDefaultMessage("            ${name} (${duration}) : ${oldValue} -> ${value}")
                        .withValue(NAME, limit.getName())
                        .withValue(DURATION, limitAcceptableDuration)
                        .withValue("value", limitValue)
                        .withValue("oldValue", limitToModify.getValue())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
            limitsAdder
                    .beginTemporaryLimit()
                    .setName(limit.getName())
                    .setValue(limitValue)
                    .setAcceptableDuration(limitAcceptableDuration)
                    .endTemporaryLimit();
        }
        if (lineTemporaryLimits != null) {
            for (TemporaryLimit limit : lineTemporaryLimits) {
                temporaryLimitsReports.add(Report.builder()
                        .withKey("temporaryLimitDeleted" + limit.getName())
                        .withDefaultMessage("            ${name} (${duration}) deleted")
                        .withValue(NAME, limit.getName())
                        .withValue(DURATION, limit.getAcceptableDuration())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }
        if (!temporaryLimitsReports.isEmpty()) {
            temporaryLimitsReports.add(0, Report.builder()
                    .withKey("temporaryLimitsModification")
                    .withDefaultMessage("        Temporary current limits :")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            limitsReports.addAll(temporaryLimitsReports);
        }
    }

    private boolean characteristicsModified(LineModificationInfos lineModificationInfos) {
        return lineModificationInfos.getSeriesReactance() != null
                || lineModificationInfos.getSeriesResistance() != null
                || lineModificationInfos.getShuntConductance1() != null
                || lineModificationInfos.getShuntSusceptance1() != null
                || lineModificationInfos.getShuntConductance2() != null
                || lineModificationInfos.getShuntSusceptance2() != null;
    }

}
