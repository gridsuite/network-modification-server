/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.iidm.network.SwitchKind;
import okhttp3.mockwebserver.MockWebServer;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.BusbarSectionCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.springframework.cloud.stream.binder.test.OutputDestination;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertDeleteCount;
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

    public static  VoltageLevelCreationInfos makeAVoltageLevelInfos(int nbBBSs, int nbCnxs) {
        List<BusbarSectionCreationInfos> bbses;
        if (nbBBSs < 0) {
            bbses = null;
        } else {
            bbses = new ArrayList<>();
            Stream.iterate(1, n -> n + 1).limit(nbBBSs + 1).forEach(i -> bbses.add(new BusbarSectionCreationInfos("bbs" + i, "NW", 1 + i, 1)));
        }

        List<BusbarConnectionCreationInfos> cnxes;
        if (nbCnxs < 0) {
            cnxes = null;
        } else {
            cnxes = new ArrayList<>();
            Stream.iterate(0, n -> n + 1).limit(nbBBSs).forEach(i -> {
                cnxes.add(new BusbarConnectionCreationInfos("bbs.nw", "bbs.ne", SwitchKind.BREAKER));
                cnxes.add(new BusbarConnectionCreationInfos("bbs.nw", "bbs.ne", SwitchKind.DISCONNECTOR));
            });
        }

        VoltageLevelCreationInfos createVoltLvlEntity1 = VoltageLevelCreationInfos.builder()
                .type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .substationId("s1").nominalVoltage(379.0).equipmentId("idVL1").equipmentName("VLName")
                .busbarSections(bbses).busbarConnections(cnxes)
                .build();

        return createVoltLvlEntity1;
    }
}
