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
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;

import java.util.HashSet;
import java.util.Objects;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_ERROR;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class BranchStatusModification extends AbstractModification {

    private final BranchStatusModificationInfos modificationInfos;

    public BranchStatusModification(BranchStatusModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        String branchId = modificationInfos.getEquipmentId();
        Branch<?> branch = network.getBranch(branchId);
        if (branch == null) {
            throw new NetworkModificationException(BRANCH_NOT_FOUND, branchId);
        }

        String branchType = branch.getType() == IdentifiableType.LINE ? "Line" : "2WTransformer";
        switch (modificationInfos.getAction()) {
            case LOCKOUT:
                applyLockoutBranch(subReporter, branch, branchType);
                break;
            case TRIP:
                applyTripBranch(subReporter, branch, branchType, network);
                break;
            case SWITCH_ON:
                applySwitchOnBranch(subReporter, branch, branchType);
                break;
            case ENERGISE_END_ONE:
                applyEnergiseBranchEnd(subReporter, branch, branchType, Branch.Side.ONE);
                break;
            case ENERGISE_END_TWO:
                applyEnergiseBranchEnd(subReporter, branch, branchType, Branch.Side.TWO);
                break;
            default:
                throw NetworkModificationException.createBranchActionTypeUnsupported(modificationInfos.getAction());
        }
    }

    private void applyLockoutBranch(Reporter subReporter, Branch<?> branch, String branchType) {
        if (disconnectAllTerminals(branch)) {
            branch.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect all branch ends");
        }
        subReporter.report(Report.builder()
            .withKey("lockout" + branchType + "Applied")
            .withDefaultMessage(branchType + " ${id} (id) : lockout applied")
            .withValue("id", branch.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    private void applyTripBranch(Reporter subReporter, Branch<?> branch, String branchType, Network network) {
        var trip = new BranchTripping(branch.getId());
        var switchesToDisconnect = new HashSet<Switch>();
        var terminalsToDisconnect = new HashSet<Terminal>();
        var traversedTerminals = new HashSet<Terminal>();
        trip.traverse(network, switchesToDisconnect, terminalsToDisconnect, traversedTerminals);

        switchesToDisconnect.forEach(sw -> sw.setOpen(true));
        terminalsToDisconnect.forEach(Terminal::disconnect);

        subReporter.report(Report.builder()
                .withKey("trip" + branchType + "Applied")
                .withDefaultMessage(branchType + " ${id} (id) : trip applied")
                .withValue("id", branch.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        traversedTerminals.stream().map(t -> network.getBranch(t.getConnectable().getId())).filter(Objects::nonNull)
                .forEach(b -> ((Branch<?>) b).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add());
    }

    private void applySwitchOnBranch(Reporter subReporter, Branch<?> branch, String branchType) {
        if (connectAllTerminals(branch)) {
            branch.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to connect all branch ends");
        }

        subReporter.report(Report.builder()
                .withKey("switchOn" + branchType + "Applied")
                .withDefaultMessage(branchType + " ${id} (id) : switch on applied")
                .withValue("id", branch.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyEnergiseBranchEnd(Reporter subReporter, Branch<?> branch, String branchType, Branch.Side side) {
        Branch.Side oppositeSide = side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE;
        if (connectOneTerminal(branch.getTerminal(side)) && disconnectOneTerminal(branch.getTerminal(oppositeSide))) {
            branch.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise branch end");
        }

        subReporter.report(Report.builder()
                .withKey("energise" + branchType + "EndApplied")
                .withDefaultMessage(branchType + " ${id} (id) : energise the side ${side} applied")
                .withValue("id", branch.getId())
                .withValue("side", side.name())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private boolean disconnectAllTerminals(Branch<?> branch) {
        return branch.getTerminals().stream().allMatch(this::disconnectOneTerminal);
    }

    private boolean disconnectOneTerminal(Terminal terminal) {
        return !terminal.isConnected() || terminal.disconnect();
    }

    private boolean connectAllTerminals(Branch<?> branch) {
        return branch.getTerminals().stream().allMatch(this::connectOneTerminal);
    }

    private boolean connectOneTerminal(Terminal terminal) {
        return terminal.isConnected() || terminal.connect();
    }
}
