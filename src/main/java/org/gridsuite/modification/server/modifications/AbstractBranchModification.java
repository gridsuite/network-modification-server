/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.CurrentLimits;
import com.powsybl.iidm.network.CurrentLimitsAdder;
import com.powsybl.iidm.network.LoadingLimits;
import org.gridsuite.modification.server.dto.BranchModificationInfos;
import org.gridsuite.modification.server.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitModificationInfos;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public abstract class AbstractBranchModification extends AbstractModification {

    private static final String DURATION = "duration";
    private static final String NAME = "name";
    protected final BranchModificationInfos modificationInfos;

    public AbstractBranchModification(BranchModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected void modifyBranch(Branch branch, BranchModificationInfos branchModificationInfos, Reporter subReporter, String reporterKey, String reporterDefaultMessage) {

        subReporter.report(Report.builder()
                .withKey(reporterKey)
                .withDefaultMessage(reporterDefaultMessage)
                .withValue("id", branchModificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        if (branchModificationInfos.getEquipmentName() != null) {
            subReporter.report(ModificationUtils.getInstance().buildModificationReportWithIndentation(branch.getNameOrId(),
                    branchModificationInfos.getEquipmentName().getValue(), "Name", 0));
            branch.setName(branchModificationInfos.getEquipmentName().getValue());
        }

        if (characteristicsModified(branchModificationInfos)) {
            modifyCharacteristics(branch, branchModificationInfos, subReporter);
        }

        CurrentLimitsModificationInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsModificationInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        List<Report> side1LimitsReports = new ArrayList<>();
        //TODO FM why need to cast ???
        CurrentLimits currentLimits1 = (CurrentLimits) branch.getCurrentLimits1().orElse(null);
        modifyCurrentLimits(currentLimitsInfos1, branch.newCurrentLimits1(), currentLimits1, side1LimitsReports);
        List<Report> side2LimitsReports = new ArrayList<>();
        //TODO FM why need to cast ???
        CurrentLimits currentLimits2 = (CurrentLimits) branch.getCurrentLimits2().orElse(null);
        modifyCurrentLimits(currentLimitsInfos2, branch.newCurrentLimits2(), currentLimits2, side2LimitsReports);

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

    protected void modifyCurrentLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, List<Report> limitsReports) {
        boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
        if (hasPermanent) {
            limitsReports.add(ModificationUtils.getInstance().buildModificationReportWithIndentation(currentLimits != null ? currentLimits.getPermanentLimit() : Double.NaN,
                    currentLimitsInfos.getPermanentLimit(), "IST", 2));
            limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
        } else {
            if (currentLimits != null) {
                limitsAdder.setPermanentLimit(currentLimits.getPermanentLimit());
            }
        }
        modifyTemporaryLimits(currentLimitsInfos, limitsAdder, currentLimits, limitsReports);
        limitsAdder.add();
    }

    protected void modifyTemporaryLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder,
                                         CurrentLimits currentLimits, List<Report> limitsReports) {
        // we create a mutable list of temporary limits to be able to remove the limits that are modified
        List<LoadingLimits.TemporaryLimit> lineTemporaryLimits = null;
        if (currentLimits != null) {
            lineTemporaryLimits = new ArrayList<>(currentLimits.getTemporaryLimits());
        }
        List<Report> temporaryLimitsReports = new ArrayList<>();
        for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
            int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
            double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
            String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
            String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
            LoadingLimits.TemporaryLimit limitToModify = null;
            if (currentLimits != null) {
                limitToModify = currentLimits.getTemporaryLimit(limitAcceptableDuration);
                // we remove the limit to modify from the list of temporary limits so we can log the remaining ones (deleted)
                lineTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
            }
            if (limitToModify == null) {
                temporaryLimitsReports.add(Report.builder().withKey("temporaryLimitAdded" + limit.getName())
                        .withDefaultMessage("            ${name} (${duration}) added with ${value}")
                        .withValue(NAME, limit.getName())
                        .withValue(DURATION, limitDurationToReport)
                        .withValue("value", limitValueToReport)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

            } else if (Double.compare(limitToModify.getValue(), limitValue) != 0) {
                temporaryLimitsReports.add(Report.builder()
                        .withKey("temporaryLimitModified" + limit.getName())
                        .withDefaultMessage("            ${name} (${duration}) : ${oldValue} -> ${value}")
                        .withValue(NAME, limit.getName())
                        .withValue(DURATION, limitDurationToReport)
                        .withValue("value", limitValueToReport)
                        .withValue("oldValue", limitToModify.getValue() == Double.MAX_VALUE ? "no value" : String.valueOf(limitToModify.getValue()))
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
            for (LoadingLimits.TemporaryLimit limit : lineTemporaryLimits) {
                temporaryLimitsReports.add(Report.builder()
                        .withKey("temporaryLimitDeleted" + limit.getName())
                        .withDefaultMessage("            ${name} (${duration}) deleted")
                        .withValue(NAME, limit.getName())
                        .withValue(DURATION, limit.getAcceptableDuration() == Integer.MAX_VALUE ? " " : String.valueOf(limit.getAcceptableDuration()))
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

    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        return branchModificationInfos.getSeriesReactance() != null
                && branchModificationInfos.getSeriesReactance().getValue() != null
                || branchModificationInfos.getSeriesResistance() != null
                && branchModificationInfos.getSeriesResistance().getValue() != null;
    }

    protected abstract void modifyCharacteristics(Branch branch, BranchModificationInfos lineModificationInfos, Reporter subReporter);
}
