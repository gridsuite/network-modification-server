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
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.OperationalStatusModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class OperationalStatusModification extends AbstractModification {

    private final OperationalStatusModificationInfos modificationInfos;
    private static final Logger LOGGER = LoggerFactory.getLogger(OperationalStatusModification.class);

    public OperationalStatusModification(OperationalStatusModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        String equipmentId = modificationInfos.getEquipmentId();
        Identifiable<?> equipment = network.getIdentifiable(equipmentId);
        if (equipment == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, equipmentId);
        }

        String equipmentTypeName = String.valueOf(equipment.getType());
        switch (modificationInfos.getAction()) {
            case LOCKOUT:
                applyLockoutEquipment(subReporter, equipment, equipmentTypeName);
                break;
            case TRIP:
                applyTripEquipment(subReporter, equipment, equipmentTypeName, network);
                break;
            case SWITCH_ON:
                applySwitchOnEquipment(subReporter, equipment, equipmentTypeName);
                break;
            case ENERGISE_END_ONE:
                applyEnergiseEquipmentEnd(subReporter, equipment, equipmentTypeName, Branch.Side.ONE);
                break;
            case ENERGISE_END_TWO:
                applyEnergiseEquipmentEnd(subReporter, equipment, equipmentTypeName, Branch.Side.TWO);
                break;
            default:
                throw NetworkModificationException.createOperationalStatusActionTypeUnsupported(modificationInfos.getAction());
        }
    }

    private void applyLockoutEquipment(Reporter subReporter, Identifiable<?> equipment, String equipmentTypeName) {
        if (disconnectAllTerminals(equipment)) {
            equipment.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.PLANNED_OUTAGE).add();
        } else {
            throw new NetworkModificationException(OPERATIONAL_STATUS_ERROR, "Unable to disconnect all equipment ends");
        }
        subReporter.report(Report.builder()
            .withKey("lockout" + equipmentTypeName + "Applied")
            .withDefaultMessage(equipmentTypeName + " ${id} (id) : lockout applied")
            .withValue("id", equipment.getId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }

    private void applyTripEquipment(Reporter subReporter, Identifiable<?> equipment, String equipmentTypeName, Network network) {
        var switchesToDisconnect = new HashSet<Switch>();
        var terminalsToDisconnect = new HashSet<Terminal>();
        var traversedTerminals = new HashSet<Terminal>();
        ModificationUtils.getInstance().getTrippingFromIdentifiable(equipment).traverse(network, switchesToDisconnect, terminalsToDisconnect, traversedTerminals);

        LOGGER.info("Apply Trip on {} {}, switchesToDisconnect: {} terminalsToDisconnect: {} traversedTerminals: {}",
                equipmentTypeName, equipment.getId(),
                switchesToDisconnect.stream().map(Identifiable::getId).collect(Collectors.toList()),
                terminalsToDisconnect.stream().map(Terminal::getConnectable).map(Identifiable::getId).collect(Collectors.toList()),
                traversedTerminals.stream().map(Terminal::getConnectable).map(Identifiable::getId).collect(Collectors.toList()));

        switchesToDisconnect.forEach(sw -> sw.setOpen(true));
        terminalsToDisconnect.forEach(Terminal::disconnect);

        subReporter.report(Report.builder()
                .withKey("trip" + equipmentTypeName + "Applied")
                .withDefaultMessage(equipmentTypeName + " ${id} (id) : trip applied")
                .withValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        traversedTerminals.stream().map(t -> network.getIdentifiable(t.getConnectable().getId()))
                .filter(Objects::nonNull)
                .filter(distinctByKey(Identifiable::getId))  // dont process the same branch more than once
                .forEach(b -> b.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.FORCED_OUTAGE).add());
    }

    private void applySwitchOnEquipment(Reporter subReporter, Identifiable<?> equipment, String equipmentTypeName) {
        if (connectAllTerminals(equipment)) {
            equipment.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
        } else {
            throw new NetworkModificationException(OPERATIONAL_STATUS_ERROR, "Unable to connect all branch ends");
        }

        subReporter.report(Report.builder()
                .withKey("switchOn" + equipmentTypeName + "Applied")
                .withDefaultMessage(equipmentTypeName + " ${id} (id) : switch on applied")
                .withValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyEnergiseEquipmentEnd(Reporter subReporter, Identifiable<?> equipment, String equipmentTypeName, Branch.Side side) {
        if (equipment instanceof Branch<?>) {
            Branch<?> branch = (Branch<?>) equipment;
            Branch.Side oppositeSide = side == Branch.Side.ONE ? Branch.Side.TWO : Branch.Side.ONE;
            if (connectOneTerminal(branch.getTerminal(side)) && disconnectOneTerminal(branch.getTerminal(oppositeSide))) {
                branch.newExtension(BranchStatusAdder.class).withStatus(BranchStatus.Status.IN_OPERATION).add();
            } else {
                throw new NetworkModificationException(OPERATIONAL_STATUS_ERROR, "Unable to energise branch end");
            }

            subReporter.report(Report.builder()
                    .withKey("energise" + equipmentTypeName + "EndApplied")
                    .withDefaultMessage(equipmentTypeName + " ${id} (id) : energise the side ${side} applied")
                    .withValue("id", equipment.getId())
                    .withValue("side", side.name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    private boolean disconnectAllTerminals(Identifiable<?> equipment) {
        return ModificationUtils.getInstance().getTerminalsFromIdentifiable(equipment).stream().allMatch(this::disconnectOneTerminal);
    }

    private boolean disconnectOneTerminal(Terminal terminal) {
        return !terminal.isConnected() || terminal.disconnect();
    }

    private boolean connectAllTerminals(Identifiable<?> equipment) {
        return ModificationUtils.getInstance().getTerminalsFromIdentifiable(equipment).stream().allMatch(this::connectOneTerminal);
    }

    private boolean connectOneTerminal(Terminal terminal) {
        return terminal.isConnected() || terminal.connect();
    }
}
