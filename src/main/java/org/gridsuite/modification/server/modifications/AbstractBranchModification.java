/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchModificationInfos;
import org.gridsuite.modification.server.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitModificationInfos;
import org.gridsuite.modification.server.dto.TemporaryLimitModificationType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_MODIFICATION_ERROR;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public abstract class AbstractBranchModification extends AbstractModification {

    private static final String DURATION = "duration";
    private static final String NAME = "name";
    protected final BranchModificationInfos modificationInfos;

    protected AbstractBranchModification(BranchModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    protected void modifyBranch(Branch<?> branch, BranchModificationInfos branchModificationInfos, ReportNode subReportNode, String reporterKey, String reporterDefaultMessage) {

        subReportNode.newReportNode()
                .withMessageTemplate(reporterKey, reporterDefaultMessage)
                .withUntypedValue("id", branchModificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        if (branchModificationInfos.getEquipmentName() != null) {
            subReportNode.include(ModificationUtils.getInstance().buildModificationReportWithIndentation(branch.getOptionalName().isEmpty() ? null : branch.getOptionalName().get(), branchModificationInfos.getEquipmentName().getValue(), "Name", 0));

            branch.setName(branchModificationInfos.getEquipmentName().getValue());
        }

        if (characteristicsModified(branchModificationInfos)) {
            modifyCharacteristics(branch, branchModificationInfos, subReportNode);
        }

        CurrentLimitsModificationInfos currentLimitsInfos1 = modificationInfos.getCurrentLimits1();
        CurrentLimitsModificationInfos currentLimitsInfos2 = modificationInfos.getCurrentLimits2();
        List<ReportNode> side1LimitsReports = new ArrayList<>();
        CurrentLimits currentLimits1 = branch.getCurrentLimits1().orElse(null);
        if (currentLimitsInfos1 != null) {
            modifyCurrentLimits(currentLimitsInfos1, branch.newCurrentLimits1(), currentLimits1, side1LimitsReports);
        }
        List<ReportNode> side2LimitsReports = new ArrayList<>();
        CurrentLimits currentLimits2 = branch.getCurrentLimits2().orElse(null);
        if (currentLimitsInfos2 != null) {
            modifyCurrentLimits(currentLimitsInfos2, branch.newCurrentLimits2(), currentLimits2, side2LimitsReports);
        }
        if (!side1LimitsReports.isEmpty() || !side2LimitsReports.isEmpty()) {
            ReportNode limitsReportNode = subReportNode.newReportNode().withMessageTemplate("limits", "Limits").add();
            limitsReportNode.newReportNode()
                    .withMessageTemplate("limitsModification", "Limits")
                    .withMessageTemplate("limitsModification", "Limits")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side1LimitsReports, "side1LimitsModification",
                    "    Side 1", Map.of());
            ModificationUtils.getInstance().reportModifications(limitsReportNode, side2LimitsReports, "side2LimitsModification",
                    "    Side 2", Map.of());
        }

        updateConnections(branch, branchModificationInfos);
    }

    private void updateConnections(Branch<?> branch, BranchModificationInfos branchModificationInfos) {
        List<TwoSides> errorSides = new ArrayList<>();
        List<String> errorTypes = new ArrayList<>();
        if (branchModificationInfos.getConnected1() != null && !updateConnection(branch, TwoSides.ONE, modificationInfos.getConnected1().getValue())) {
            errorSides.add(TwoSides.ONE);
            errorTypes.add(Boolean.TRUE.equals(modificationInfos.getConnected1().getValue()) ? "connect" : "disconnect");
        }
        if (branchModificationInfos.getConnected2() != null && !updateConnection(branch, TwoSides.TWO, modificationInfos.getConnected2().getValue())) {
            errorSides.add(TwoSides.TWO);
            errorTypes.add(Boolean.TRUE.equals(modificationInfos.getConnected2().getValue()) ? "connect" : "disconnect");
        }
        if (!errorSides.isEmpty()) {
            throw new NetworkModificationException(BRANCH_MODIFICATION_ERROR,
                String.format("Could not %s equipment '%s' on side %s",
                    errorTypes.stream().distinct().collect(Collectors.joining("/")),
                    branch.getId(),
                    errorSides.stream().map(Enum::toString).collect(Collectors.joining("/"))));
        }
    }

    private boolean updateConnection(Branch<?> branch, TwoSides side, Boolean connectionChange) {
        boolean done = true;
        if (branch.getTerminal(side).isConnected() && Boolean.FALSE.equals(connectionChange)) {
            branch.getTerminal(side).disconnect();
            if (branch.getTerminal(side).isConnected()) {
                done = false;
            }
        } else if (!branch.getTerminal(side).isConnected() && Boolean.TRUE.equals(connectionChange)) {
            branch.getTerminal(side).connect();
            if (!branch.getTerminal(side).isConnected()) {
                done = false;
            }
        }
        return done;
    }

    protected void modifyCurrentLimits(CurrentLimitsModificationInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder, CurrentLimits currentLimits, List<ReportNode> limitsReports) {
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
                                         CurrentLimits currentLimits, List<ReportNode> limitsReports) {
        // we create a mutable list of temporary limits to be able to remove the limits that are modified in current modification
        List<LoadingLimits.TemporaryLimit> branchTemporaryLimits = new ArrayList<>();
        if (currentLimits != null) {
            branchTemporaryLimits.addAll(currentLimits.getTemporaryLimits());
        }
        List<ReportNode> temporaryLimitsReports = new ArrayList<>();
        for (CurrentTemporaryLimitModificationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
            int limitAcceptableDuration = limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration();
            double limitValue = limit.getValue() == null ? Double.MAX_VALUE : limit.getValue();
            String limitDurationToReport = limitAcceptableDuration == Integer.MAX_VALUE ? " " : String.valueOf(limitAcceptableDuration);
            String limitValueToReport = limitValue == Double.MAX_VALUE ? "no value" : String.valueOf(limitValue);
            LoadingLimits.TemporaryLimit limitToModify = null;
            if (currentLimits != null) {
                limitToModify = currentLimits.getTemporaryLimit(limitAcceptableDuration);
                if (limitToModify != null && !limitToModify.getName().equals(limit.getName())) {
                    throw new PowsyblException("2temporary limits have the same duration " + limitAcceptableDuration);
                }
                // we remove the limit to modify from the list of temporary limits so we can get the list of temporary limits comming from previous modifications
                branchTemporaryLimits.removeIf(temporaryLimit -> temporaryLimit.getAcceptableDuration() == limitAcceptableDuration);
            }
            if (limitToModify == null && limit.getModificationType() == TemporaryLimitModificationType.ADDED) {
                temporaryLimitsReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("temporaryLimitAdded" + limit.getName(), "            ${name} (${duration}) added with ${value}")
                        .withUntypedValue(NAME, limit.getName())
                        .withUntypedValue(DURATION, limitDurationToReport)
                        .withUntypedValue("value", limitValueToReport)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());

            } else if (limitToModify != null) {
                if (limit.getModificationType() == TemporaryLimitModificationType.DELETED) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("temporaryLimitDeleted" + limit.getName(), "            ${name} (${duration}) deleted")
                            .withUntypedValue(NAME, limit.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                    continue;
                } else if (Double.compare(limitToModify.getValue(), limitValue) != 0 && limit.getModificationType() != null) {
                    temporaryLimitsReports.add(ReportNode.newRootReportNode()
                            .withMessageTemplate("temporaryLimitModified" + limit.getName(), "            ${name} (${duration}) : ${oldValue} -> ${value}")
                            .withUntypedValue(NAME, limit.getName())
                            .withUntypedValue(DURATION, limitDurationToReport)
                            .withUntypedValue("value", limitValueToReport)
                            .withUntypedValue("oldValue",
                                    limitToModify.getValue() == Double.MAX_VALUE ? "no value"
                                            : String.valueOf(limitToModify.getValue()))
                            .withSeverity(TypedValue.INFO_SEVERITY)
                            .build());
                } else {
                    limitValue = limitToModify.getValue();
                }
            } else {
                continue;
            }
            limitsAdder
                    .beginTemporaryLimit()
                    .setName(limit.getName())
                    .setValue(limitValue)
                    .setAcceptableDuration(limitAcceptableDuration)
                    .endTemporaryLimit();
        }
        // we add the temporary limits comming from previous modifications
        if (!branchTemporaryLimits.isEmpty()) {
            for (LoadingLimits.TemporaryLimit limit : branchTemporaryLimits) {
                limitsAdder
                        .beginTemporaryLimit()
                        .setName(limit.getName())
                        .setValue(limit.getValue())
                        .setAcceptableDuration(limit.getAcceptableDuration())
                        .endTemporaryLimit();
            }
        }
        if (!temporaryLimitsReports.isEmpty()) {
            temporaryLimitsReports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("temporaryLimitsModification", "            Temporary current limits :")
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            limitsReports.addAll(temporaryLimitsReports);
        }
    }

    protected boolean characteristicsModified(BranchModificationInfos branchModificationInfos) {
        return branchModificationInfos.getX() != null
                && branchModificationInfos.getX().getValue() != null
                || branchModificationInfos.getR() != null
                && branchModificationInfos.getR().getValue() != null;
    }

    protected abstract void modifyCharacteristics(Branch<?> branch, BranchModificationInfos branchModificationInfos,
            ReportNode subReportNode);
}
