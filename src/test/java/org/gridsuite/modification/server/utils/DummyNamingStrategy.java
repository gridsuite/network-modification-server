package org.gridsuite.modification.server.utils;

import com.google.auto.service.AutoService;
import com.powsybl.iidm.modification.topology.NamingStrategy;
import com.powsybl.iidm.network.BusbarSection;
import com.powsybl.iidm.network.Connectable;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VoltageLevel;

import java.util.List;

/**
 * @author Etienne Homer {@literal <etienne.homer at rte-france.com>}
 */
@AutoService(NamingStrategy.class)
public class DummyNamingStrategy implements NamingStrategy {

    @Override
    public final String getName() {
        return "Dummy naming strategy";
    }

    @Override
    public String getSectioningPrefix(String baseId, BusbarSection bbs, int busBarNum, int section1Num, int section2Num) {
        return "SECTION_" + busBarNum + "_" + section1Num + "_" + section2Num;
    }

    @Override
    public String getChunkPrefix(String baseId, List<SwitchKind> switchKindList, int busBarNum, int section1Num, int section2Num) {
        return "PREFIX_" + busBarNum + "_" + section1Num + "_" + section2Num;
    }

    @Override
    public final String getDisconnectorId(String baseId, int id1Num, int id2Num) {
        return "DISCONNECTOR_" + id1Num + "_" + id2Num;
    }

    @Override
    public String getDisconnectorId(BusbarSection bbs, String baseId, int id1Num, int id2Num, int side) {
        return "DISCONNECTOR_" + id1Num + "_" + id2Num + "_" + side;
    }

    @Override
    public String getDisconnectorBetweenChunksId(BusbarSection bbs, String baseId, int id1Num, int id2Num) {
        return "DISCONNECTOR_" + id1Num + "_" + id2Num;
    }

    @Override
    public final String getBreakerId(String baseId) {
        return baseId;
    }

    @Override
    public final String getBreakerId(String baseId, int id1Num, int id2Num) {
        return "BREAKER_" + id1Num + "_" + id1Num;
    }

    @Override
    public final String getSwitchId(String baseId) {
        return baseId;
    }

    @Override
    public final String getSwitchId(String baseId, int idNum) {
        return "SWITCH_" + idNum;
    }

    @Override
    public final String getSwitchId(String baseId, int id1Num, int id2Num) {
        return "SWITCH_" + id1Num + "_" + id2Num;
    }

    @Override
    public final String getBusbarId(String baseId, int id1Num, int id2Num) {
        return "BUSBAR_" + id1Num + "_" + id2Num;
    }

    @Override
    public final String getBusbarId(String baseId, List<SwitchKind> switchKindList, int id1Num, int id2Num) {
        return "BUSBAR_" + id1Num + "_" + id2Num;
    }

    @Override
    public final String getBusId(String baseId) {
        return baseId;
    }

    @Override
    public final String getSwitchBaseId(Connectable<?> connectable, int side) {
        return "SWITCH_" + connectable.getId() + "_" + side;
    }

    @Override
    public String getSwitchBaseId(VoltageLevel voltageLevel, BusbarSection bbs1, BusbarSection bbs2) {
        return "SWITCH_" + bbs1.getId() + "_" + bbs2.getId();
    }
}
