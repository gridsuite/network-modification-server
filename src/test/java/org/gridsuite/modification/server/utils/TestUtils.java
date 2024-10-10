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
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.iidm.network.extensions.OperatingStatusAdder;
import mockwebserver3.MockWebServer;
import org.apache.commons.text.StringSubstitutor;
import org.gridsuite.modification.server.service.ReportService;
import org.mockito.ArgumentCaptor;
import org.springframework.cloud.stream.binder.test.OutputDestination;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static com.vladmihalcea.sql.SQLStatementCountValidator.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public final class TestUtils {

    private static final long TIMEOUT = 100;

    private TestUtils() {
        throw new IllegalCallerException("Utility class");
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
            for (String destination : destinations) {
                try {
                    assertNull(output.receive(TIMEOUT, destination), "Should not be any messages in queue " + destination + " :");
                } catch (NullPointerException e) {
                    // Ignoring
                }
            }
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
        assertNull(httpRequest, "Should not be any http requests :");
    }

    public static void assertRequestsCount(long select, long insert, long update, long delete) {
        assertSelectCount(select);
        assertInsertCount(insert);
        assertUpdateCount(update);
        assertDeleteCount(delete);
    }

    @SuppressWarnings("unchecked")
    public static void assertOperatingStatus(Network network, String identifiableName, OperatingStatus.Status status) {
        assertNotNull(network);
        Identifiable<?> identifiable = network.getIdentifiable(identifiableName);
        assertNotNull(identifiable);
        OperatingStatus operatingStatus = identifiable.getExtensionByName("operatingStatus");
        assertNotNull(operatingStatus);
        assertEquals(status, operatingStatus.getStatus());
    }

    @SuppressWarnings("unchecked")
    public static void setOperatingStatus(Network network, String identifiableName, OperatingStatus.Status status) {
        Identifiable<?> identifiable = network.getIdentifiable(identifiableName);
        assertNotNull(identifiable);
        identifiable.newExtension(OperatingStatusAdder.class).withStatus(status).add();
    }

    public static String resourceToString(String resource) throws IOException {
        InputStream inputStream = Objects.requireNonNull(TestUtils.class.getResourceAsStream(resource));
        String content = new String(ByteStreams.toByteArray(inputStream), StandardCharsets.UTF_8);
        return content.replaceAll("\\s", "");
    }

    public static void assertLogNthMessage(String expectedMessage, String reportKey, ReportService reportService, int rank) {
        ArgumentCaptor<ReportNode> reporterCaptor = ArgumentCaptor.forClass(ReportNode.class);
        verify(reportService, atLeast(1)).sendReport(any(UUID.class), reporterCaptor.capture());
        assertNotNull(reporterCaptor.getValue());
        Optional<String> message = getMessageFromReporter(reportKey, reporterCaptor.getValue(), rank);
        assertTrue(message.isPresent());
        assertEquals(expectedMessage, message.get().trim());
    }

    public static void assertLogMessage(String expectedMessage, String reportKey, ReportService reportService) {
        assertLogNthMessage(expectedMessage, reportKey, reportService, 1);
    }

    public static void assertLogMessageWithoutRank(String expectedMessage, String reportKey, ReportService reportService) {
        ArgumentCaptor<ReportNode> reporterCaptor = ArgumentCaptor.forClass(ReportNode.class);
        verify(reportService, atLeast(1)).sendReport(any(UUID.class), reporterCaptor.capture());
        assertNotNull(reporterCaptor.getValue());
        assertTrue(assertMessageFoundFromReporter(expectedMessage, reportKey, reporterCaptor.getValue()));
    }

    private static boolean assertMessageFoundFromReporter(String expectedMessage, String reportKey, ReportNode reporterModel) {
        for (ReportNode report : reporterModel.getChildren()) {
            if (report.getMessageKey().equals(reportKey)) {
                String message = formatReportMessage(report, reporterModel);
                if (message.trim().equals(expectedMessage)) {
                    return true;
                }
            }
        }

        boolean foundInSubReporters = false;
        Iterator<ReportNode> reportersIterator = reporterModel.getChildren().iterator();
        while (!foundInSubReporters && reportersIterator.hasNext()) {
            foundInSubReporters = assertMessageFoundFromReporter(expectedMessage, reportKey, reportersIterator.next());
        }
        return foundInSubReporters;
    }

    private static Optional<String> getMessageFromReporter(String reportKey, ReportNode reporterModel, int rank) {
        Optional<String> message = Optional.empty();

        Iterator<ReportNode> reportsIterator = reporterModel.getChildren().iterator();
        int nbTimes = 0;
        while (message.isEmpty() && reportsIterator.hasNext()) {
            ReportNode report = reportsIterator.next();
            if (report.getMessageKey().equals(reportKey)) {
                nbTimes++;
                if (nbTimes == rank) {
                    message = Optional.of(formatReportMessage(report, reporterModel));
                }
            }
        }

        Iterator<ReportNode> reportersIterator = reporterModel.getChildren().iterator();
        while (message.isEmpty() && reportersIterator.hasNext()) {
            message = getMessageFromReporter(reportKey, reportersIterator.next(), rank);
        }

        return message;
    }

    private static String formatReportMessage(ReportNode report, ReportNode reporterModel) {
        return new StringSubstitutor(reporterModel.getValues()).replace(new StringSubstitutor(report.getValues()).replace(report.getMessageTemplate()));
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
