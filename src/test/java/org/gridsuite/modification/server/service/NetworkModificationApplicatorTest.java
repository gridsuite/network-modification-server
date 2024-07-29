/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.ReportNodeAdder;
import com.powsybl.commons.report.TypedValue;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

@Tag("UnitTest")
class NetworkModificationApplicatorTest {
    @ParameterizedTest
    @MethodSource("provideArgumentsForComputeHigherSeverity")
    void computeHigherSeverity(List<ReportNode> reports, ApplicationStatus expectedSeverity) {

        assertEquals(6, reports.size(), "We need exactly 6 reports to run the test");

        ReportNode rootReportNode = ReportNode.newRootReportNode()
                .withMessageTemplate("rep1", "")
                .build();

        ReportNode subReportNode1 = rootReportNode.newReportNode().withMessageTemplate("subrep1", "").add();
        ReportNode subReportNode2 = rootReportNode.newReportNode().withMessageTemplate("subrep2", "").add();
        ReportNode subReportNode3 = rootReportNode.newReportNode().withMessageTemplate("rep2", "").add();

        ReportNode subSubReportNode1 = subReportNode3.newReportNode().withMessageTemplate("subsubrep1", "").add();
        ReportNode subSubReportNode2 = subReportNode3.newReportNode().withMessageTemplate("subsubrep2", "").add();

        addSubReport(rootReportNode, reports.get(0));
        addSubReport(subReportNode1, reports.get(1));
        addSubReport(subReportNode2, reports.get(2));
        addSubReport(subReportNode3, reports.get(3));
        addSubReport(subSubReportNode1, reports.get(4));
        addSubReport(subSubReportNode2, reports.get(5));

        ApplicationStatus actualSeverity = NetworkModificationApplicator.getApplicationStatus(rootReportNode);
        assertEquals(expectedSeverity, actualSeverity);
    }

    private static void addSubReport(ReportNode parent, ReportNode child) {
        ReportNodeAdder adder = parent.newReportNode().withMessageTemplate(child.getMessageKey(), child.getMessageTemplate());
        TypedValue severity = child.getValue(ReportConstants.SEVERITY_KEY).orElse(null);
        if (severity != null) {
            adder.withSeverity(severity);
        }
        adder.add();
    }

    @Test
    void shouldThrowExceptionOnBadSeverity() {
        ReportNode rootReportNode = ReportNode.newRootReportNode()
                .withMessageTemplate("rep1", "")
                .build();
        rootReportNode.newReportNode()
                .withMessageTemplate("badSeverity", "Bad severity message")
                .withUntypedValue("reportSeverity", "bad severity")
                .add();

        assertThrows(IllegalArgumentException.class, () -> NetworkModificationApplicator.getApplicationStatus(rootReportNode));
    }

    private static Stream<Arguments> provideArgumentsForComputeHigherSeverity() {
        return Stream.of(
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                warningReport,
                                errorReport),
                        ApplicationStatus.WITH_ERRORS),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                warningReport,
                                infoReport),
                        ApplicationStatus.WITH_WARNINGS),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport),
                        ApplicationStatus.ALL_OK),
                Arguments.of(List.of(
                                errorReport,
                                warningReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport),
                        ApplicationStatus.WITH_ERRORS),
                Arguments.of(List.of(
                                infoReport,
                                errorReport,
                                warningReport,
                                infoReport,
                                infoReport,
                                infoReport),
                        ApplicationStatus.WITH_ERRORS),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                errorReport,
                                warningReport,
                                infoReport,
                                infoReport),
                        ApplicationStatus.WITH_ERRORS),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                errorReport,
                                warningReport,
                                infoReport),
                        ApplicationStatus.WITH_ERRORS),
                Arguments.of(List.of(
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                warningReport,
                                notSeverityReport,
                                notSeverityReport),
                        ApplicationStatus.WITH_WARNINGS),
                Arguments.of(List.of(
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport),
                        ApplicationStatus.ALL_OK)
        );
    }

    private static ReportNode infoReport = ReportNode.newRootReportNode()
            .withMessageTemplate("info", "Info severity message")
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build();

    private static ReportNode warningReport = ReportNode.newRootReportNode()
            .withMessageTemplate("warning", "Warning severity message")
            .withSeverity(TypedValue.WARN_SEVERITY)
            .build();

    private static ReportNode errorReport = ReportNode.newRootReportNode()
            .withMessageTemplate("error", "Error severity message")
            .withSeverity(TypedValue.ERROR_SEVERITY)
            .build();

    private static ReportNode notSeverityReport = ReportNode.newRootReportNode()
            .withMessageTemplate("notSeverity", "Not a severity message")
            .withUntypedValue("rand", "random value")
            .build();
}
