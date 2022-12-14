/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;

import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at rte-france.com>
 */
public final class NetworkWithTeePoint {

    private NetworkWithTeePoint() {
    }

    public static Network create(UUID uuid) {
        return create(uuid, new NetworkFactoryImpl());
    }

    /**
     *     VL1            VL2            VL3
     *
     *     ld1            g2              ld3
     *      |              |               |
     *     br1            br2             br3
     *      |             |                |
     *     d1             d2               d3
     *      |              |               |
     *     bbs1 ----------bbs2------------bbs3
     *            l1       |       l2
     *                     | l3
     *                     |
     *                    bbs4         VL4
     *                    |
     *                    d4
     *                    |
     *                    br4
     *                    |
     *                    ld4
     */

    public static Network create(UUID uuid, NetworkFactory networkFactory) {
        Network network = networkFactory.createNetwork(uuid.toString(), "NetworkWithTeePoint");

        // VL1
        Substation s1 = createSubstation(network, "s1", null, Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v1, "bbs1", null, 0);
        createLoad(v1, "ld1", null, 2, 0., 0., "ld1", 0, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v1, "d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "br1", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL2
        Substation s2 = createSubstation(network, "s2", null, Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v2, "bbs2", null, 0);

        createGenerator(v2, "g2", 2, 42.1, 1.0, "g2", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v2, "br2", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL3
        Substation s3 = createSubstation(network, "s3", null, Country.FR);
        VoltageLevel v3 = createVoltageLevel(s3, "v3", null, TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v3, "bbs3", null, 0);

        createLoad(v3, "ld3", null, 2, 0., 0., "ld3", 3, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v3, "d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "br3", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL4
        Substation s4 = createSubstation(network, "s4", null, Country.FR);
        VoltageLevel v4 = createVoltageLevel(s4, "v4", null, TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v4, "bbs4", null, 0);

        createLoad(v4, "ld4", null, 2, 0., 0., "ld4", 3, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v4, "d4", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v4, "br4", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // create lines
        createLine(network, "l1", null, "v1", "v2", 4, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "l1", 1, ConnectablePosition.Direction.TOP, "l1", 1, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l1d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l1br1", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v2, "l1d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l1br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l2", null, "v2", "v3", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "l2", 2, ConnectablePosition.Direction.TOP, "l2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "l2d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l2br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v3, "l2d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "l2br3", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l3", null, "v2", "v4", 5, 4, 12.0, 7.0, 5.5, 7.5, 6.5, 8.5, "l3", 3, ConnectablePosition.Direction.TOP, "l3", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "l3d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 6);
        createSwitch(v2, "l3br2", null, SwitchKind.BREAKER, true, false, true, 6, 5);
        createSwitch(v4, "l3d4", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v4, "l3br4", null, SwitchKind.BREAKER, true, false, true, 5, 4);

        /********************************/

//        VoltageLevel vl1 = createVoltageLevel(s1, "vl1", "vl1", TopologyKind.NODE_BREAKER, 225.0);
//        createBusBarSection(vl1, "vl11A", "vl1A", 0);
//        createBusBarSection(vl1, "vl11B", "vl1B", 1);
//        VoltageLevel vl2 = createVoltageLevel(s1, "vl2", "vl2", TopologyKind.NODE_BREAKER, 225.0);
//        createBusBarSection(vl2, "vl21A", "vl21A", 0);
//        createBusBarSection(vl2, "vl21B", "vl21B", 1);
//        VoltageLevel vl3 = createVoltageLevel(s1, "vl3", "vl3", TopologyKind.NODE_BREAKER, 225.0);
//        createBusBarSection(vl3, "vl31A", "vl31A", 0);
//        createBusBarSection(vl3, "vl31B", "vl31B", 1);
//        VoltageLevel vl4 = createVoltageLevel(s1, "vl4", "vl4", TopologyKind.NODE_BREAKER, 225.0);
//        createBusBarSection(vl4, "vl41A", "vl41A", 0);
//        createBusBarSection(vl4, "vl41B", "vl41B", 1);
//
//        createLine(network, "l1", "l1", "vl2", "vl4", 8, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "cn1line1", 1, ConnectablePosition.Direction.TOP, "cn2line1", 1, ConnectablePosition.Direction.TOP);
//        createSwitch(vl1, "vl3dl1", "vl3dl1", SwitchKind.DISCONNECTOR, true, false, false, 0, 7);
//        createSwitch(vl1, "vl3bl1", "vl3bl1", SwitchKind.BREAKER, true, false, false, 7, 8);
//        createSwitch(vl4, "vl4dl1", "vl4dl1", SwitchKind.DISCONNECTOR, true, false, false, 0, 3);
//        createSwitch(vl4, "vl4bl1", "vl4bl1", SwitchKind.BREAKER, true, false, false, 3, 4);
//
//        createLine(network, "l2", "l2", "vl1", "vl4", 31, 31, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "cn1line2", 2, ConnectablePosition.Direction.TOP, "cn2line2", 2, ConnectablePosition.Direction.TOP);
//        createSwitch(vl1, "vl1dl2", "vl1dl2", SwitchKind.DISCONNECTOR, true, false, false, 0, 30);
//        createSwitch(vl1, "vl1bl2", "vl1bl2", SwitchKind.BREAKER, true, false, false, 30, 31);
//        createSwitch(vl4, "vl3dl2", "vl3dl2", SwitchKind.DISCONNECTOR, true, false, false, 0, 30);
//        createSwitch(vl4, "vl3bl2", "vl3bl2", SwitchKind.BREAKER, true, false, false, 30, 31);
//
//        createLine(network, "l3", "l3", "vl1", "vl3", 10, 12, 12.0, 7.0, 5.5, 7.5, 6.5, 8.5, "cn1line3", 3, ConnectablePosition.Direction.TOP, "cn2line3", 3, ConnectablePosition.Direction.TOP);
//        createSwitch(vl1, "vl1dl3", "vl1dl3", SwitchKind.DISCONNECTOR, true, false, false, 0, 9);
//        createSwitch(vl1, "vl1bl3", "vl1bl3", SwitchKind.BREAKER, true, false, true, 9, 10);
//        createSwitch(vl3, "vl3dl3", "vl3dl3", SwitchKind.DISCONNECTOR, true, false, false, 0, 11);
//        createSwitch(vl3, "vl3bl3", "vl3bl3", SwitchKind.BREAKER, true, false, true, 11, 12);
//
//        createLine(network, "attachedLineId", "attachedLine", "vl3", "vl2", 10, 12, 12.0, 7.0, 5.5, 7.5, 6.5, 8.5, "cn1line3", 3, ConnectablePosition.Direction.TOP, "cn2line3", 3, ConnectablePosition.Direction.TOP);
//        createSwitch(vl3, "v1dal", "v1dal", SwitchKind.DISCONNECTOR, true, false, false, 0, 20);
//        createSwitch(vl3, "v1bal", "v1bal", SwitchKind.BREAKER, true, false, true, 20, 21);
//        createSwitch(vl2, "v3dal", "v3dal", SwitchKind.DISCONNECTOR, true, false, false, 0, 20);
//        createSwitch(vl2, "v3bal", "v3bal", SwitchKind.BREAKER, true, false, true, 20, 21);
        /************************************************/
        return network;
    }
}
