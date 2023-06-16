/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package com.powsybl.iidm.modification.topology;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.computation.ComputationManager;
import com.powsybl.iidm.modification.AbstractNetworkModification;
import com.powsybl.iidm.network.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * TODO RemoveHvdcLine class has been locally duplicated from Powsybl-core - should be removed when
 * https://github.com/powsybl/powsybl-core/pull/2620 is merged and delivered to Gridsuite
 */
public class RemoveHvdcLine extends AbstractNetworkModification {

    private static final Logger LOGGER = LoggerFactory.getLogger(RemoveHvdcLine.class);

    private final String hvdcLineId;
    private final List<String> shuntCompensatorIds;

    RemoveHvdcLine(String hvdcLineId, List<String> shuntCompensatorIds) {
        this.hvdcLineId = Objects.requireNonNull(hvdcLineId);
        this.shuntCompensatorIds = Objects.requireNonNull(shuntCompensatorIds);
    }

    private static void ignoredVscShunts(Reporter reporter, String shuntsIds, String converterStationId1, String converterStationId2) {
        reporter.report(Report.builder()
                .withKey("ignoredVscShunts")
                .withDefaultMessage("Shunts ${shuntsIds} are ignored since converter stations ${converterStationId1} and ${converterStationId2} are VSC")
                .withValue("shuntsIds", shuntsIds)
                .withValue("converterStationId1", converterStationId1)
                .withValue("converterStationId2", converterStationId2)
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
    }

    private static void removedHvdcLineReport(Reporter reporter, String hvdcLineId) {
        reporter.report(Report.builder()
                .withKey("removeHvdcLine")
                .withDefaultMessage("Hvdc line ${hvdcLineId} has been removed")
                .withValue("hvdcLineId", hvdcLineId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private static void notFoundHvdcLineReport(Reporter reporter, String hvdcLineId) {
        reporter.report(Report.builder()
                .withKey("HvdcNotFound")
                .withDefaultMessage("Hvdc line ${hvdcLineId} is not found")
                .withValue("hvdcLineId", hvdcLineId)
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
    }

    private static void removedVscConverterStationReport(Reporter reporter, String vscConverterStationId) {
        reporter.report(Report.builder()
                .withKey("removeVscConverterStation")
                .withDefaultMessage("Vsc converter station ${vscConverterStationId} has been removed")
                .withValue("vscConverterStationId", vscConverterStationId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private static void removedLccConverterStationReport(Reporter reporter, String lccConverterStationId) {
        reporter.report(Report.builder()
                .withKey("removeLccConverterStation")
                .withDefaultMessage("Lcc converter station ${lccConverterStationId} has been removed")
                .withValue("lccConverterStationId", lccConverterStationId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private static void removedShuntCompensatorReport(Reporter reporter, String shuntCompensatorId) {
        reporter.report(Report.builder()
                .withKey("removeShuntCompensator")
                .withDefaultMessage("Shunt compensator ${shuntCompensatorId} has been removed")
                .withValue("shuntCompensatorId", shuntCompensatorId)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private static void ignoredShuntInAnotherVoltageLevel(Reporter reporter, String shuntId, String voltageLevelId1, String voltageLevelId2) {
        reporter.report(Report.builder()
                .withKey("ignoredShuntInAnotherVoltageLevel")
                .withDefaultMessage("Shunt compensator ${shuntId} has been ignored because it is not in the same voltage levels as the Lcc (${voltageLevelId1} or ${voltageLevelId2})")
                .withValue("shuntId", shuntId)
                .withValue("voltageLevelId1", voltageLevelId1)
                .withValue("voltageLevelId2", voltageLevelId2)
                .withSeverity(TypedValue.WARN_SEVERITY)
                .build());
    }

    private static void notFoundShuntReport(Reporter reporter, String shuntId) {
        reporter.report(Report.builder()
                .withKey("notFoundShunt")
                .withDefaultMessage("Shunt ${shuntId} not found")
                .withValue("shuntId", shuntId)
                .withSeverity(TypedValue.ERROR_SEVERITY)
                .build());
    }

    @Override
    public void apply(Network network, boolean throwException, ComputationManager computationManager, Reporter reporter) {
        HvdcLine hvdcLine = network.getHvdcLine(hvdcLineId);
        if (hvdcLine != null) {
            HvdcConverterStation<?> hvdcConverterStation1 = hvdcLine.getConverterStation1();
            HvdcConverterStation<?> hvdcConverterStation2 = hvdcLine.getConverterStation2();
            Set<ShuntCompensator> shunts = null;
            if (hvdcConverterStation1.getHvdcType() == HvdcConverterStation.HvdcType.LCC) { // in real-life cases, both converter stations are of the same type
                shunts = shuntCompensatorIds.stream()
                        .map(id -> getShuntCompensator(id, network, throwException, reporter))
                        .filter(Objects::nonNull).collect(Collectors.toCollection(LinkedHashSet::new));
            } else if (!shuntCompensatorIds.isEmpty()) { // VSC converter stations and defined shunts
                String shuntIds = String.join(",", shuntCompensatorIds);
                LOGGER.warn("Shunts {} are ignored since converter stations {} and {} are VSC", shuntIds, hvdcConverterStation1.getId(), hvdcConverterStation2.getId());
                ignoredVscShunts(reporter, shuntIds, hvdcConverterStation1.getId(), hvdcConverterStation2.getId());
            }
            hvdcLine.remove();
            removedHvdcLineReport(reporter, hvdcLineId);
            LOGGER.info("Hvdc line {} has been removed", hvdcLineId);
            // Remove the Shunt compensators that represent the filters of the LCC
            removeShuntCompensators(network, hvdcConverterStation1, hvdcConverterStation2, shunts, throwException, computationManager, reporter);
            removeConverterStations(network, hvdcConverterStation1, hvdcConverterStation2, throwException, computationManager, reporter);
        } else {
            LOGGER.error("Hvdc Line {} not found", hvdcLineId);
            notFoundHvdcLineReport(reporter, hvdcLineId);
            if (throwException) {
                throw new PowsyblException("Hvdc Line " + hvdcLineId + " not found");
            }
        }
    }

    private static ShuntCompensator getShuntCompensator(String id, Network network, boolean throwException, Reporter reporter) {
        ShuntCompensator sc = network.getShuntCompensator(id);
        if (sc == null) {
            notFoundShuntReport(reporter, id);
            LOGGER.error("Shunt {} not found", id);
            if (throwException) {
                throw new PowsyblException("Shunt " + id + " not found");
            }
        }
        return sc;
    }

    private static void removeShuntCompensators(Network network, HvdcConverterStation<?> hvdcConverterStation1, HvdcConverterStation<?> hvdcConverterStation2, Set<ShuntCompensator> shunts, boolean throwException, ComputationManager computationManager, Reporter reporter) {
        if (shunts == null) {
            return;
        }

        // Get the voltage levels of both lcc converter stations
        VoltageLevel vl1 = hvdcConverterStation1.getTerminal().getVoltageLevel();
        VoltageLevel vl2 = hvdcConverterStation2.getTerminal().getVoltageLevel();

        // removing shunt compensators
        for (ShuntCompensator shuntCompensator : shunts) {
            VoltageLevel shuntVl = shuntCompensator.getTerminal().getVoltageLevel();
            // check whether the shunt compensator is connected to the same voltage level as the lcc
            String shuntId = shuntCompensator.getId();
            if (vl1 == shuntVl || vl2 == shuntVl) {
                new RemoveFeederBay(shuntId).apply(network, throwException, computationManager, reporter);
                removedShuntCompensatorReport(reporter, shuntId);
                LOGGER.info("Shunt compensator {} has been removed", shuntId);
            } else {
                LOGGER.warn("Shunt compensator {} has been ignored because it is not in the same voltage levels as the Lcc ({} or {})", shuntId, vl1.getId(), vl2.getId());
                ignoredShuntInAnotherVoltageLevel(reporter, shuntId, vl1.getId(), vl2.getId());
            }
        }
    }

    private static void removeConverterStations(Network network, HvdcConverterStation<?> hvdcConverterStation1, HvdcConverterStation<?> hvdcConverterStation2, boolean throwException, ComputationManager computationManager, Reporter reporter) {
        String station1Id = hvdcConverterStation1.getId();
        String station2Id = hvdcConverterStation2.getId();
        HvdcConverterStation.HvdcType station1Type = hvdcConverterStation1.getHvdcType();
        HvdcConverterStation.HvdcType station2Type = hvdcConverterStation2.getHvdcType();
        new RemoveFeederBay(station1Id).apply(network, throwException, computationManager, reporter);
        new RemoveFeederBay(station2Id).apply(network, throwException, computationManager, reporter);
        reportConverterStationRemoved(reporter, station1Id, station1Type);
        reportConverterStationRemoved(reporter, station2Id, station2Type);
    }

    private static void reportConverterStationRemoved(Reporter reporter, String stationId, HvdcConverterStation.HvdcType converterStationType) {
        if (converterStationType == HvdcConverterStation.HvdcType.LCC) {
            removedLccConverterStationReport(reporter, stationId);
            LOGGER.info("Lcc converter station {} has been removed", stationId);
        } else if (converterStationType == HvdcConverterStation.HvdcType.VSC) {
            removedVscConverterStationReport(reporter, stationId);
            LOGGER.info("Vsc converter station {} has been removed", stationId);
        }
    }

}
