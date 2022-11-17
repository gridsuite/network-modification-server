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
import com.powsybl.iidm.modification.topology.TopologyModificationUtils;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import com.powsybl.iidm.network.extensions.BusbarSectionPositionAdder;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.BusbarSectionCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
// TODO transfer to powsybl-core (com.powsybl.iidm.modification)
// TODO remove public qualifier for all methods
public final class ModificationUtils {

    private ModificationUtils() {
    }

    public static ModificationUtils getInstance() {
        return new ModificationUtils();
    }

    VoltageLevel getVoltageLevel(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId);
        }
        return voltageLevel;
    }

    public int getPosition(String busOrBusbarSectionId, Network network, VoltageLevel voltageLevel) {
        var position = 0;
        var bbs = network.getBusbarSection(busOrBusbarSectionId);

        if (bbs != null) {
            var extensionExist = bbs.getExtension(BusbarSectionPosition.class) != null;
            if (!extensionExist) {
                return position;
            }

            if (voltageLevel.getConnectableStream().anyMatch(c -> !(c instanceof BusbarSection))) {
                var rightRange = TopologyModificationUtils.getUnusedOrderPositionsAfter(bbs);
                if (rightRange.isPresent()) {
                    position = rightRange.get().getMinimum();
                } else {
                    var leftRange = TopologyModificationUtils.getUnusedOrderPositionsBefore(bbs);
                    if (leftRange.isPresent()) {
                        position = leftRange.get().getMaximum();
                    } else {
                        throw new NetworkModificationException(POSITION_ORDER_ERROR, "no available position");
                    }
                }
            }
        } else {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "Bus bar section " + busOrBusbarSectionId + " not found");
        }
        return position;
    }

    public Bus getBusBreakerBus(VoltageLevel voltageLevel, String busId) {
        VoltageLevel.BusBreakerView busBreakerView = voltageLevel.getBusBreakerView();
        Bus bus = busBreakerView.getBus(busId);
        if (bus == null) {
            throw new NetworkModificationException(BUS_NOT_FOUND, busId);
        }
        return bus;
    }

    public int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
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

    private Pair<Integer, Integer> addBusbarConnectionTo(VoltageLevelCreationInfos voltageLevelCreationInfos,
                                                         BusbarConnectionCreationInfos bbsci, Map<String, Integer> idToNodeRank, Pair<Integer, Integer> ranks,
                                                         VoltageLevel voltageLevel) {

        String fromBBSId = bbsci.getFromBBS();
        Integer rank1 = idToNodeRank.get(fromBBSId);
        if (rank1 == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "From side '" + fromBBSId + "' unknown");
        }

        String toBBSId = bbsci.getToBBS();
        Integer rank2 = idToNodeRank.get(toBBSId);
        if (rank2 == null) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "To side '" + toBBSId + "' unknown");
        }

        SwitchKind switchKind = bbsci.getSwitchKind();
        if (switchKind == SwitchKind.DISCONNECTOR && fromBBSId.equals(toBBSId)) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR,
                "Disconnector between same bus bar section '" + toBBSId + "'");
        }

        int nodeRank = ranks.getLeft();
        int cnxRank = ranks.getRight();
        String infix = voltageLevelCreationInfos.getEquipmentId() + "_" + fromBBSId + "_" + toBBSId + "_";
        if (switchKind == SwitchKind.DISCONNECTOR) {
            voltageLevel.getNodeBreakerView().newDisconnector()
                .setKind(switchKind)
                .setId("disconnector_" + infix + cnxRank++)
                .setNode1(rank1)
                .setNode2(rank2)
                .setFictitious(false)
                .setRetained(false)
                .setOpen(false)
                .add();
        } else if (switchKind == SwitchKind.BREAKER) {
            int preBreakerRank = nodeRank++;
            int postBreakerRank = nodeRank++;
            voltageLevel.getNodeBreakerView().newDisconnector()
                .setKind(SwitchKind.DISCONNECTOR)
                .setId("disconnector_" + infix + cnxRank++)
                .setNode1(rank1)
                .setNode2(preBreakerRank)
                .setFictitious(false)
                .setRetained(false)
                .setOpen(false)
                .add();

            voltageLevel.getNodeBreakerView().newBreaker()
                .setKind(switchKind)
                .setId("breaker_" + infix + cnxRank++)
                .setNode1(preBreakerRank)
                .setNode2(postBreakerRank)
                .setFictitious(false)
                .setRetained(false)
                .setOpen(false)
                .add();

            voltageLevel.getNodeBreakerView().newDisconnector()
                .setKind(SwitchKind.DISCONNECTOR)
                .setId("disconnector_" + infix + cnxRank++)
                .setNode1(postBreakerRank)
                .setNode2(rank2)
                .setFictitious(false)
                .setRetained(false)
                .setOpen(false)
                .add();
        } else {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Swich kind '" + switchKind + "' unknown");
        }

        return Pair.of(nodeRank, cnxRank);
    }

    public void createVoltageLevelAction(VoltageLevelCreationInfos voltageLevelCreationInfos,
                                         Reporter subReporter, Network network) {
        String substationId = voltageLevelCreationInfos.getSubstationId();
        Substation substation = network.getSubstation(substationId);
        if (substation == null) {
            throw new NetworkModificationException(SUBSTATION_NOT_FOUND, substationId);
        }

        VoltageLevel voltageLevel = substation.newVoltageLevel()
            .setId(voltageLevelCreationInfos.getEquipmentId())
            .setName(voltageLevelCreationInfos.getEquipmentName())
            .setTopologyKind(TopologyKind.NODE_BREAKER)
            .setNominalV(voltageLevelCreationInfos.getNominalVoltage())
            .add();

        int nodeRank = voltageLevel.getNodeBreakerView().getMaximumNodeIndex() + 1;
        Map<String, Integer> idToNodeRank = new TreeMap<>();
        for (BusbarSectionCreationInfos bbs : voltageLevelCreationInfos.getBusbarSections()) {
            BusbarSection sjb = voltageLevel.getNodeBreakerView().newBusbarSection()
                .setId(bbs.getId())
                .setName(bbs.getName())
                .setNode(nodeRank)
                .add();
            sjb.newExtension(BusbarSectionPositionAdder.class)
                .withBusbarIndex(bbs.getVertPos())
                .withSectionIndex(bbs.getHorizPos())
                .add();
            idToNodeRank.put(bbs.getId(), nodeRank);
            nodeRank += 1;
        }

        int cnxRank = 1;
        Pair<Integer, Integer> currRanks = Pair.of(nodeRank, cnxRank);
        List<BusbarConnectionCreationInfos> busbarConnections = voltageLevelCreationInfos.getBusbarConnections();
        // js empty [] seems to be decoded null on java side some times -> temporary (?) protection
        if (busbarConnections != null) {
            for (BusbarConnectionCreationInfos bbsci : busbarConnections) {
                currRanks = addBusbarConnectionTo(voltageLevelCreationInfos, bbsci, idToNodeRank, currRanks, voltageLevel);
            }
        }

        subReporter.report(Report.builder()
            .withKey("voltageLevelCreated")
            .withDefaultMessage("New voltage level with id=${id} created")
            .withValue("id", voltageLevelCreationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
    }
}

