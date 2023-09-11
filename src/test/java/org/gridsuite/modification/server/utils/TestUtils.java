/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.common.io.ByteStreams;
import com.powsybl.commons.exceptions.UncheckedInterruptedException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Branch;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.BranchStatus;
import com.powsybl.iidm.network.extensions.BranchStatusAdder;
import okhttp3.mockwebserver.MockWebServer;
import org.apache.commons.text.StringSubstitutor;
import org.gridsuite.modification.server.service.ReportService;
import org.junit.platform.commons.util.StringUtils;
import org.mockito.ArgumentCaptor;
import org.springframework.cloud.stream.binder.test.OutputDestination;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.assertDeleteCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertInsertCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertSelectCount;
import static com.vladmihalcea.sql.SQLStatementCountValidator.assertUpdateCount;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;

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

    @SuppressWarnings("unchecked")
    public static void assertBranchStatus(Network network, String branchName, BranchStatus.Status status) {
        assertNotNull(network);
        Branch<?> branch = network.getBranch(branchName);
        assertNotNull(branch);
        BranchStatus branchStatus = branch.getExtensionByName("branchStatus");
        assertNotNull(branchStatus);
        assertEquals(status, branchStatus.getStatus());
    }

    @SuppressWarnings("unchecked")
    public static void setBranchStatus(Network network, String branchName, BranchStatus.Status status) {
        Branch<?> branch = network.getBranch(branchName);
        assertNotNull(branch);
        branch.newExtension(BranchStatusAdder .class).withStatus(status).add();
    }

    public static String resourceToString(String resource) throws IOException {
        InputStream inputStream = Objects.requireNonNull(TestUtils.class.getResourceAsStream(resource));
        String content = new String(ByteStreams.toByteArray(inputStream), StandardCharsets.UTF_8);
        return StringUtils.replaceWhitespaceCharacters(content, "");
    }

    public static void assertLogMessage(String expectedMessage, String reportKey, ReportService reportService) {
        ArgumentCaptor<ReporterModel> reporterCaptor = ArgumentCaptor.forClass(ReporterModel.class);
        verify(reportService, atLeast(1)).sendReport(any(UUID.class), reporterCaptor.capture());
        assertNotNull(reporterCaptor.getValue());
        Optional<String> message = getMessageFromReporter(reportKey, reporterCaptor.getValue());
        assertTrue(message.isPresent());
        assertEquals(expectedMessage, message.get().trim());
    }

    private static Optional<String> getMessageFromReporter(String reportKey, ReporterModel reporterModel) {
        Optional<String> message = Optional.empty();

        Iterator<Report> reportsIterator = reporterModel.getReports().iterator();
        while (message.isEmpty() && reportsIterator.hasNext()) {
            Report report = reportsIterator.next();
            if (report.getReportKey().equals(reportKey)) {
                message = Optional.of(formatReportMessage(report, reporterModel));
            }
        }

        Iterator<ReporterModel> reportersIterator = reporterModel.getSubReporters().iterator();
        while (message.isEmpty() && reportersIterator.hasNext()) {
            message = getMessageFromReporter(reportKey, reportersIterator.next());
        }

        return message;
    }

    private static String formatReportMessage(Report report, ReporterModel reporterModel) {
        return new StringSubstitutor(reporterModel.getTaskValues()).replace(new StringSubstitutor(report.getValues()).replace(report.getDefaultMessage()));
    }

    public static void assertWiremockServerRequestsEmptyThenShutdown(WireMockServer wireMockServer) throws UncheckedInterruptedException, IOException {
        try {
            wireMockServer.checkForUnmatchedRequests(); // requests no matched ? (it returns an exception if a request was not matched by wireMock, but does not complain if it was not verified by 'verify')
            assertEquals(0, wireMockServer.findAll(WireMock.anyRequestedFor(WireMock.anyUrl())).size()); // requests no verified ?
        } finally {
            wireMockServer.shutdown();
        }
    }

}
