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
     * Create a network as following:
     * <pre>
     *     VL1            VL2             VL3
     *
     *     ld1            g2              ld3
     *      |              |               |
     *     br1            br2             br3
     *      |              |               |
     *     d1             d2              d3
     *      |              |               |
     *     bbs1 ----------bbs2------------bbs3
     *              l1     |       l2
     *                     | l3
     *                     |
     *                    bbs4         VL4
     *                     |
     *                    d4
     *                     |
     *                    br4
     *                     |
     *                    ld4
     * </pre>
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

        //createLine(network, "l2", null, "v2", "v3", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "l2", 2, ConnectablePosition.Direction.TOP, "l2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "l2d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l2br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v3, "l2d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "l2br3", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l3", null, "v2", "v4", 5, 4, 12.0, 7.0, 5.5, 7.5, 6.5, 8.5, "l3", 3, ConnectablePosition.Direction.TOP, "l3", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "l3d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 6);
        createSwitch(v2, "l3br2", null, SwitchKind.BREAKER, true, false, true, 6, 5);
        createSwitch(v4, "l3d4", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v4, "l3br4", null, SwitchKind.BREAKER, true, false, true, 5, 4);

        return network;
    }
}
