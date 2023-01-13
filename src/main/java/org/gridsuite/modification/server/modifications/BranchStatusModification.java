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
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;

import java.util.HashSet;
import java.util.Objects;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BRANCH_ACTION_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;

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
        String lineId = modificationInfos.getEquipmentId();
        if (network.getLine(lineId) == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
        }
        switch (modificationInfos.getAction()) {
            case LOCKOUT:
                applyLockoutLine(network, subReporter, lineId);
                break;
            case TRIP:
                applyTripLine(network, subReporter, lineId);
                break;
            case SWITCH_ON:
                applySwitchOnLine(network, subReporter, lineId);
                break;
            case ENERGISE_END_ONE:
                applyEnergiseLineEnd(network, subReporter, lineId, Branch.Side.ONE);
                break;
            case ENERGISE_END_TWO:
                applyEnergiseLineEnd(network, subReporter, lineId, Branch.Side.TWO);
                break;
            default:
                throw NetworkModificationException.createBranchActionTypeUnsupported(modificationInfos.getAction());
        }
    }

    private void applyLockoutLine(Network network, Reporter subReporter, String lineId) {
        if (disconnectLineBothSides(network, lineId)) {
            network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to disconnect both line ends");
        }
        subReporter.report(Report.builder()
            .withKey("lockoutLineApplied")
            .withDefaultMessage("Line ${id} (id) : lockout applied")
            .withValue("id", lineId)
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    private void applyTripLine(Network network, Reporter subReporter, String lineId) {
        var trip = new BranchTripping(lineId);
        var switchToDisconnect = new HashSet<Switch>();
        var terminalsToDisconnect = new HashSet<Terminal>();
        var traversedTerminals = new HashSet<Terminal>();
        trip.traverse(network, switchToDisconnect, terminalsToDisconnect, traversedTerminals);

        switchToDisconnect.forEach(sw -> sw.setOpen(true));
        terminalsToDisconnect.forEach(Terminal::disconnect);

        subReporter.report(Report.builder()
                .withKey("tripLineApplied")
                .withDefaultMessage("Line ${id} (id) : trip applied")
                .withValue("id", lineId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        traversedTerminals.stream().map(t -> network.getLine(t.getConnectable().getId())).filter(Objects::nonNull)
                .forEach(b -> b.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add());
    }

    private void applySwitchOnLine(Network network, Reporter subReporter, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Connected = terminal1.isConnected() || terminal1.connect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Connected = terminal2.isConnected() || terminal2.connect();
        if (terminal1Connected && terminal2Connected) {
            network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to connect both line ends");
        }

        subReporter.report(Report.builder()
                .withKey("switchOnLineApplied")
                .withDefaultMessage("Line ${id} (id) : switch on applied")
                .withValue("id", lineId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyEnergiseLineEnd(Network network, Reporter subReporter, String lineId, Branch.Side side) {
        Terminal terminalToConnect = network.getLine(lineId).getTerminal(side);
        boolean isTerminalToConnectConnected = terminalToConnect.isConnected() || terminalToConnect.connect();

        Branch.Side oppositeSide = side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE;
        Terminal terminalToDisconnect = network.getLine(lineId).getTerminal(oppositeSide);
        boolean isTerminalToDisconnectDisconnected = !terminalToDisconnect.isConnected() || terminalToDisconnect.disconnect();

        if (isTerminalToConnectConnected && isTerminalToDisconnectDisconnected) {
            network.getLine(lineId).newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException(BRANCH_ACTION_ERROR, "Unable to energise line end");
        }

        subReporter.report(Report.builder()
                .withKey("energiseLineEndApplied")
                .withDefaultMessage("Line ${id} (id) : energise the side ${side} applied")
                .withValue("id", lineId)
                .withValue("side", side.name())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private boolean disconnectLineBothSides(Network network, String lineId) {
        Terminal terminal1 = network.getLine(lineId).getTerminal1();
        boolean terminal1Disconnected = !terminal1.isConnected() || terminal1.disconnect();
        Terminal terminal2 = network.getLine(lineId).getTerminal2();
        boolean terminal2Disconnected = !terminal2.isConnected() || terminal2.disconnect();

        return terminal1Disconnected && terminal2Disconnected;
    }
}
