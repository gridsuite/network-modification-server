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
import com.powsybl.iidm.modification.tripping.AbstractTripping;
import com.powsybl.iidm.modification.tripping.BranchTripping;
import com.powsybl.iidm.modification.tripping.ThreeWindingsTransformerTripping;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;
import static org.gridsuite.modification.server.modifications.ModificationUtils.getEquipmentByIdentifiableType;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class BranchStatusModification extends AbstractModification {

    private final BranchStatusModificationInfos modificationInfos;
    private static final Logger LOGGER = LoggerFactory.getLogger(BranchStatusModification.class);

    public BranchStatusModification(BranchStatusModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        String branchId = modificationInfos.getEquipmentId();
        Identifiable<?> branch = network.getIdentifiable(branchId);

        if (branch == null) {
            throw new NetworkModificationException(BRANCH_NOT_FOUND, branchId);
        }
        String branchTypeName = branch.getType().toString();
        Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, branchTypeName, branchId);
        switch (modificationInfos.getAction()) {
            case LOCKOUT:
                applyLockoutBranch(subReporter, identifiable, branchTypeName);
                break;
            case TRIP:
                applyTripBranch(subReporter, identifiable, branchTypeName, network);
                break;
            case SWITCH_ON:
                applySwitchOnBranch(subReporter, identifiable, branchTypeName);
                break;
            case ENERGISE_END_ONE:
                applyEnergiseBranchEnd(subReporter, identifiable, branchTypeName, Branch.Side.ONE);
                break;
            case ENERGISE_END_TWO:
                applyEnergiseBranchEnd(subReporter, identifiable, branchTypeName, Branch.Side.TWO);
                break;
            default:
                throw NetworkModificationException.createBranchActionTypeUnsupported(modificationInfos.getAction());
        }
    }

    private void applyLockoutBranch(Reporter subReporter, Identifiable<?> identifiable, String branchTypeName) {
        if (disconnectAllTerminals(identifiable)) {
            identifiable.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect all branch ends");
        }
        subReporter.report(Report.builder()
                .withKey("lockout" + branchTypeName + "Applied")
                .withDefaultMessage(branchTypeName + " ${id} (id) : lockout applied")
                .withValue("id", identifiable.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyTripBranch(Reporter subReporter, Identifiable<?> branch, String branchTypeName, Network network) {
        AbstractTripping trip = null;
        if (branch instanceof Branch<?>) {
            trip = new BranchTripping(branch.getId());
        } else if (branch instanceof ThreeWindingsTransformer) {
            trip = new ThreeWindingsTransformerTripping(branch.getId());
        }

        var switchesToDisconnect = new HashSet<Switch>();
        var terminalsToDisconnect = new HashSet<Terminal>();
        var traversedTerminals = new HashSet<Terminal>();
        Objects.requireNonNull(trip).traverse(network, switchesToDisconnect, terminalsToDisconnect, traversedTerminals);

        LOGGER.info("Apply Trip on {} {}, switchesToDisconnect: {} terminalsToDisconnect: {} traversedTerminals: {}",
                branchTypeName, branch.getId(),
                switchesToDisconnect.stream().map(Identifiable::getId).collect(Collectors.toList()),
                terminalsToDisconnect.stream().map(Terminal::getConnectable).map(Identifiable::getId).collect(Collectors.toList()),
                traversedTerminals.stream().map(Terminal::getConnectable).map(Identifiable::getId).collect(Collectors.toList()));

        switchesToDisconnect.forEach(sw -> sw.setOpen(true));
        terminalsToDisconnect.forEach(Terminal::disconnect);

        subReporter.report(Report.builder()
                .withKey("trip" + branchTypeName + "Applied")
                .withDefaultMessage(branchTypeName + " ${id} (id) : trip applied")
                .withValue("id", branch.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        traversedTerminals.stream().map(t -> network.getIdentifiable(t.getConnectable().getId()))
                .filter(Objects::nonNull)
                .filter(distinctByKey(b -> b.getId()))  // dont process the same branch more than once
                .forEach(b -> b.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add());
    }

    private void applySwitchOnBranch(Reporter subReporter, Identifiable<?> branch, String branchTypeName) {
        if (connectAllTerminals(branch)) {
            branch.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to connect all branch ends");
        }

        subReporter.report(Report.builder()
                .withKey("switchOn" + branchTypeName + "Applied")
                .withDefaultMessage(branchTypeName + " ${id} (id) : switch on applied")
                .withValue("id", branch.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyEnergiseBranchEnd(Reporter subReporter, Identifiable<?> identifiable, String branchTypeName, Branch.Side side) {
        if (identifiable instanceof Branch<?>) {
            Branch<?> branch = (Branch<?>) identifiable;
            Branch.Side oppositeSide = side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE;
            if (connectOneTerminal(branch.getTerminal(side)) && disconnectOneTerminal(branch.getTerminal(oppositeSide))) {
                branch.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
            } else {
                throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise branch end");
            }

            subReporter.report(Report.builder()
                    .withKey("energise" + branchTypeName + "EndApplied")
                    .withDefaultMessage(branchTypeName + " ${id} (id) : energise the side ${side} applied")
                    .withValue("id", branch.getId())
                    .withValue("side", side.name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    boolean disconnectOneTerminal(Terminal terminal) {
        return !terminal.isConnected() || terminal.disconnect();
    }

    private boolean connectOneTerminal(Terminal terminal) {
        return terminal.isConnected() || terminal.connect();
    }

    private boolean connectAllTerminals(Identifiable<?> identifiable) {
        boolean res = false;
        if (identifiable instanceof Branch<?>) {
            Branch<?> branch = (Branch<?>) identifiable;
            res = Stream.of(branch.getTerminal1(), branch.getTerminal2()).allMatch(this::connectOneTerminal);
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ThreeWindingsTransformer transfo3W = (ThreeWindingsTransformer) identifiable;
            res = Stream.of(transfo3W.getLeg1().getTerminal(), transfo3W.getLeg2().getTerminal(), transfo3W.getLeg3().getTerminal()).allMatch(this::connectOneTerminal);
        }
        return res;
    }

    private boolean disconnectAllTerminals(Identifiable<?> identifiable) {
        boolean res = false;
        if (identifiable instanceof Branch<?>) {
            Branch<?> branch = (Branch<?>) identifiable;
            res = Stream.of(branch.getTerminal1(), branch.getTerminal2()).allMatch(this::disconnectOneTerminal);
        } else if (identifiable instanceof ThreeWindingsTransformer) {
            ThreeWindingsTransformer transfo3W = (ThreeWindingsTransformer) identifiable;
            res = Stream.of(transfo3W.getLeg1().getTerminal(), transfo3W.getLeg2().getTerminal(), transfo3W.getLeg3().getTerminal()).allMatch(this::disconnectOneTerminal);
        }
        return res;
    }
}
