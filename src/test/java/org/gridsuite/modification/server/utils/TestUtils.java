/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.google.common.io.ByteStreams;
import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.platform.commons.util.StringUtils;
import org.springframework.cloud.stream.binder.test.OutputDestination;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;
import static org.junit.Assert.*;

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

    public static void assertBranchStatus(Network network, String branchName, BranchStatus.Status status) {
        assertNotNull(network);
        Branch<?> branch = network.getBranch(branchName);
        assertNotNull(branch);
        BranchStatus branchStatus = branch.getExtensionByName("branchStatus");
        assertNotNull(branchStatus);
        assertEquals(status, branchStatus.getStatus());
    }

    public static void setBranchStatus(Network network, String branchName, BranchStatus.Status status) {
        Branch<?> branch = network.getBranch(branchName);
        assertNotNull(branch);
        branch.newExtension(BranchStatusAdder .class).withStatus(status).add();
    }

    public static String resourceToString(String resource) throws IOException {
        String content = new String(ByteStreams.toByteArray(TestUtils.class.getResourceAsStream(resource)), StandardCharsets.UTF_8);
        return StringUtils.replaceWhitespaceCharacters(content, "");
    }
}
