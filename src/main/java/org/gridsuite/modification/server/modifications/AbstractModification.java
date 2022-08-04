/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.NetworkModification;
import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.NetworkStoreListener;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public abstract class AbstractModification implements NetworkModification {

    protected final ModificationInfos modificationInfos;

    AbstractModification(ModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, ComputationManager computationManager) {
        throw new UnsupportedOperationException("Not implemented");
    }

    protected abstract NetworkModificationException.Type getErrorType();

    protected abstract ModificationType getType();

    protected abstract Reporter createSubReporter(ReporterModel reporter);

    // TODO create a network damage object for each modification
    protected ModificationInfos getNetworkDamage(NetworkStoreListener listener) {
        modificationInfos.setType(getType());
        modificationInfos.setSubstationIds(listener.getSubstationsIds());
        modificationInfos.setDate(ZonedDateTime.now(ZoneOffset.UTC));
        return modificationInfos;
    }

    public List<ModificationInfos> apply(ReporterModel reporter, NetworkStoreListener listener) {
        Reporter subReporter = createSubReporter(reporter);
        try {
            apply(listener.getNetwork(), subReporter);
            return List.of(getNetworkDamage(listener));
        } catch (PowsyblException e) {
            NetworkModificationException exc = e instanceof NetworkModificationException ? (NetworkModificationException) e : new NetworkModificationException(getErrorType(), e);
            subReporter.report(Report.builder()
                .withKey(getErrorType().name())
                .withDefaultMessage(exc.getMessage())
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
            if (!listener.isBuild()) {
                throw exc;
            }
        } catch (Exception e) {
            if (!listener.isBuild()) {
                throw new NetworkModificationException(getErrorType(), e);
            } else {
                throw e;
            }
        }
        return List.of();
    }

    protected VoltageLevel getVoltageLevel(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId);
        }
        return voltageLevel;
    }

    protected Bus getBusBreakerBus(VoltageLevel voltageLevel, String busId) {
        VoltageLevel.BusBreakerView busBreakerView = voltageLevel.getBusBreakerView();
        Bus bus = busBreakerView.getBus(busId);
        if (bus == null) {
            throw new NetworkModificationException(BUS_NOT_FOUND, busId);
        }
        return bus;
    }

    protected int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
                                                String equipmentName) {
        return createNodeBreakerCellSwitches(voltageLevel, busBarSectionId, equipmentId, equipmentName, "");
    }

    protected int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
                                                String equipmentName, String sideSuffix) {
        VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
        BusbarSection busbarSection = nodeBreakerView.getBusbarSection(busBarSectionId);
        if (busbarSection == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busBarSectionId);
        }

        // creating the disconnector
        int newNode = nodeBreakerView.getMaximumNodeIndex();
        String disconnectorId = "disconnector_" + equipmentId + sideSuffix;
        String disconnectorName = equipmentName != null ? "disconnector_" + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(disconnectorId)
            .setName(disconnectorName)
            .setKind(SwitchKind.DISCONNECTOR)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(busbarSection.getTerminal().getNodeBreakerView().getNode())
            .setNode2(newNode + 1)
            .add();

        // creating the breaker
        String breakerId = "breaker_" + equipmentId + sideSuffix;
        String breakerName = equipmentName != null ? "breaker_" + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(breakerId)
            .setName(breakerName)
            .setKind(SwitchKind.BREAKER)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(newNode + 1)
            .setNode2(newNode + 2)
            .add();

        return newNode + 2;
    }
}
