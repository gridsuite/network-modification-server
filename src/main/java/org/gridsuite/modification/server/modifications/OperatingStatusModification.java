/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.tripping.*;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.iidm.network.extensions.OperatingStatusAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.OperatingStatusModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Objects;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

/**
 * @author Ghazwa REHILI <ghazwa.rehili at rte-france.com>
 */
public class OperatingStatusModification extends AbstractModification {

    private final OperatingStatusModificationInfos modificationInfos;
    private static final Logger LOGGER = LoggerFactory.getLogger(OperatingStatusModification.class);

    private static final String APPLIED = "Applied";

    public OperatingStatusModification(OperatingStatusModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        String equipmentId = modificationInfos.getEquipmentId();
        Identifiable<?> equipment = network.getIdentifiable(equipmentId);
        if (equipment == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, equipmentId);
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        String equipmentId = modificationInfos.getEquipmentId();
        Identifiable<?> equipment = network.getIdentifiable(equipmentId);
        if (equipment == null) {
            throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, equipmentId);
        }

        String equipmentType = String.valueOf(equipment.getType());
        switch (modificationInfos.getAction()) {
            case LOCKOUT -> applyLockoutEquipment(subReporter, equipment, equipmentType);
            case TRIP -> applyTripEquipment(subReporter, equipment, equipmentType, network);
            case SWITCH_ON -> applySwitchOnEquipment(subReporter, equipment, equipmentType);
            case ENERGISE_END_ONE ->
                    applyEnergiseEquipmentEnd(subReporter, equipment, equipmentType, TwoSides.ONE);
            case ENERGISE_END_TWO ->
                    applyEnergiseEquipmentEnd(subReporter, equipment, equipmentType, TwoSides.TWO);
            default ->
                    throw NetworkModificationException.createOperatingActionTypeUnsupported(modificationInfos.getAction());
        }
    }

    private void applyLockoutEquipment(Reporter subReporter, Identifiable<?> equipment, String equipmentType) {
        if (disconnectAllTerminals(equipment)) {
            equipment.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.PLANNED_OUTAGE).add();
        } else {
            throw new NetworkModificationException(OPERATING_STATUS_MODIFICATION_ERROR, "Unable to disconnect all equipment ends");
        }
        subReporter.report(Report.builder()
                .withKey("lockout" + equipmentType + APPLIED)
                .withDefaultMessage(equipmentType + " ${id} (id) : lockout applied")
                .withValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void applyTripEquipment(Reporter subReporter, Identifiable<?> equipment, String equipmentType, Network network) {
        var switchesToDisconnect = new HashSet<Switch>();
        var terminalsToDisconnect = new HashSet<Terminal>();
        var traversedTerminals = new HashSet<Terminal>();
        getTrippingFromIdentifiable(equipment).traverse(network, switchesToDisconnect, terminalsToDisconnect, traversedTerminals);

        LOGGER.info("Apply Trip on {} {}, switchesToDisconnect: {} terminalsToDisconnect: {} traversedTerminals: {}",
                equipmentType, equipment.getId(),
                switchesToDisconnect.stream().map(Identifiable::getId).collect(Collectors.toList()),
                terminalsToDisconnect.stream().map(Terminal::getConnectable).map(Identifiable::getId).collect(Collectors.toList()),
                traversedTerminals.stream().map(Terminal::getConnectable).map(Identifiable::getId).collect(Collectors.toList()));

        switchesToDisconnect.forEach(sw -> sw.setOpen(true));
        terminalsToDisconnect.forEach(Terminal::disconnect);

        subReporter.report(Report.builder()
                .withKey("trip" + equipmentType + APPLIED)
                .withDefaultMessage(equipmentType + " ${id} (id) : trip applied")
                .withValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        traversedTerminals.stream().map(t -> network.getIdentifiable(t.getConnectable().getId()))
                .filter(Objects::nonNull)
                .filter(distinctByKey(Identifiable::getId))  // dont process the same equipment more than once
                .forEach(b -> b.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.FORCED_OUTAGE).add());
    }

    private void applySwitchOnEquipment(Reporter subReporter, Identifiable<?> equipment, String equipmentType) {
        if (equipment instanceof Branch<?>) {
            if (connectAllTerminals(equipment)) {
                equipment.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.IN_OPERATION).add();
            } else {
                throw new NetworkModificationException(OPERATING_STATUS_MODIFICATION_ERROR, "Unable to connect all equipment ends");
            }

            subReporter.report(Report.builder()
                    .withKey("switchOn" + equipmentType + APPLIED)
                    .withDefaultMessage(equipmentType + " ${id} (id) : switch on applied")
                    .withValue("id", equipment.getId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        throw NetworkModificationException.createEquipmentTypeNotSupported(equipment.getClass().getSimpleName());
    }

    private void applyEnergiseEquipmentEnd(Reporter subReporter, Identifiable<?> equipment, String equipmentType, TwoSides side) {
        if (equipment instanceof Branch<?> branch) {
            TwoSides oppositeSide = side == TwoSides.ONE ? TwoSides.TWO : TwoSides.ONE;
            if (connectOneTerminal(branch.getTerminal(side)) && disconnectOneTerminal(branch.getTerminal(oppositeSide))) {
                branch.newExtension(OperatingStatusAdder.class).withStatus(OperatingStatus.Status.IN_OPERATION).add();
            } else {
                throw new NetworkModificationException(OPERATING_STATUS_MODIFICATION_ERROR, "Unable to energise equipment end");
            }

            subReporter.report(Report.builder()
                    .withKey("energise" + equipmentType + "EndApplied")
                    .withDefaultMessage(equipmentType + " ${id} (id) : energise the side ${side} applied")
                    .withValue("id", equipment.getId())
                    .withValue("side", side.name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        throw NetworkModificationException.createEquipmentTypeNotSupported(equipment.getClass().getSimpleName());
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

    public Tripping getTrippingFromIdentifiable(Identifiable<?> identifiable) {
        if (identifiable instanceof Branch<?> branch) {
            return new BranchTripping(branch.getId());
        }
        throw NetworkModificationException.createEquipmentTypeNotSupported(identifiable.getClass().getSimpleName());
    }
}
