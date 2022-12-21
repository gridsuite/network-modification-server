/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import okhttp3.mockwebserver.MockWebServer;
import org.springframework.cloud.stream.binder.test.OutputDestination;

import java.io.IOException;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertDeleteCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertInsertCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertUpdateCount;
import static org.gridsuite.modification.server.utils.NetworkUtil.createBusBarSection;
import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLine;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSubstation;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSwitch;
import static org.gridsuite.modification.server.utils.NetworkUtil.createVoltageLevel;
import static org.junit.Assert.assertNull;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class TestUtils {

    private static final long TIMEOUT = 100;

    private TestUtils() {
    }

    public static Set<String> getRequestsDone(int n, MockWebServer server) throws UncheckedInterruptedException {
        return IntStream.range(0, n).mapToObj(i -> {
            try {
                return Objects.requireNonNull(server.takeRequest(TIMEOUT, TimeUnit.MILLISECONDS)).getPath();
            } catch (InterruptedException e) {
                throw new UncheckedInterruptedException(e);
            }
        }).collect(Collectors.toSet());
    }

    public static void purgeRequests(MockWebServer server) throws UncheckedInterruptedException {
        IntStream.range(0, server.getRequestCount()).forEach(i -> {
            try {
                Objects.requireNonNull(server.takeRequest(TIMEOUT, TimeUnit.MILLISECONDS));
            } catch (InterruptedException e) {
                throw new UncheckedInterruptedException(e);
            }
        });
    }

    public static void assertQueuesEmptyThenClear(List<String> destinations, OutputDestination output) {
        try {
            destinations.forEach(destination -> assertNull("Should not be any messages in queue " + destination + " : ", output.receive(TIMEOUT, destination)));
        } catch (NullPointerException e) {
            // Ignoring
        } finally {
            output.clear(); // purge in order to not fail the other tests
        }
    }

    public static void assertServerRequestsEmptyThenShutdown(MockWebServer server) throws UncheckedInterruptedException, IOException {
        Set<String> httpRequest = null;

        try {
            httpRequest = getRequestsDone(1, server);
        } catch (NullPointerException e) {
            // ignoring
        } finally {
            server.shutdown();
        }

        assertNull("Should not be any http requests : ", httpRequest);
    }

    public static void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }

    public static Network createNetworkForDeleteVoltageLevelOnLine(UUID networkUuid) {
        Network network = new NetworkFactoryImpl().createNetwork(networkUuid.toString(), "NetworkWithTeePoint");

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

        // create lines
        createLine(network, "l1", null, "v1", "v2", 4, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "l1", 1, ConnectablePosition.Direction.TOP, "l1", 1, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l1d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l1br1", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v2, "l1d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l1br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l2", null, "v1", "v3", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "l2", 2, ConnectablePosition.Direction.TOP, "l2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l2d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l2br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v3, "l2d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "l2br3", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        return network;
    }
}
