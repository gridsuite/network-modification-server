/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.commons.reporter.TypedValue;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class NetworkModificationApplicatorTest {

    @ParameterizedTest
    @MethodSource("provideArgumentsForComputeHigherSeverity")
    void computeHigherSeverity(List<Report> reports, TypedValue expectedSeverity) {

        assertEquals(6, reports.size(), "We need exactly 6 reports to run the test");

        ReporterModel reporterModel = new ReporterModel("rep1", "");
        Reporter subReporter1 = reporterModel.createSubReporter("subrep1", "");
        Reporter subReporter2 = reporterModel.createSubReporter("subrep2", "");
        ReporterModel subReporterModel = reporterModel.createSubReporter("rep2", "", Collections.emptyMap());
        Reporter subSubReporter1 = subReporterModel.createSubReporter("subsubrep1", "");
        Reporter subSubReporter2 = subReporterModel.createSubReporter("subsubrep2", "");

        reporterModel.report(reports.get(0));
        subReporter1.report(reports.get(1));
        subReporter2.report(reports.get(2));
        subReporterModel.report(reports.get(3));
        subSubReporter1.report(reports.get(4));
        subSubReporter2.report(reports.get(5));

        TypedValue actualSeverity = NetworkModificationApplicator.computeHighestSeverityFromReporterModel(reporterModel);
        assertEquals(expectedSeverity, actualSeverity);
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
                        TypedValue.ERROR_SEVERITY),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                warningReport,
                                infoReport),
                        TypedValue.WARN_SEVERITY),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport),
                        TypedValue.INFO_SEVERITY),
                Arguments.of(List.of(
                                errorReport,
                                warningReport,
                                infoReport,
                                infoReport,
                                infoReport,
                                infoReport),
                        TypedValue.ERROR_SEVERITY),
                Arguments.of(List.of(
                                infoReport,
                                errorReport,
                                warningReport,
                                infoReport,
                                infoReport,
                                infoReport),
                        TypedValue.ERROR_SEVERITY),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                errorReport,
                                warningReport,
                                infoReport,
                                infoReport),
                        TypedValue.ERROR_SEVERITY),
                Arguments.of(List.of(
                                infoReport,
                                infoReport,
                                infoReport,
                                errorReport,
                                warningReport,
                                infoReport),
                        TypedValue.ERROR_SEVERITY),
                Arguments.of(List.of(
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                warningReport,
                                notSeverityReport,
                                notSeverityReport),
                        TypedValue.WARN_SEVERITY),
                Arguments.of(List.of(
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport,
                                notSeverityReport),
                        TypedValue.TRACE_SEVERITY)
        );
    }

    private static Report infoReport = Report.builder()
            .withKey("info")
            .withDefaultMessage("Info severity message")
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build();

    private static Report warningReport = Report.builder()
            .withKey("warning")
            .withDefaultMessage("Warning severity message")
            .withSeverity(TypedValue.WARN_SEVERITY)
            .build();

    private static Report errorReport = Report.builder()
            .withKey("error")
            .withDefaultMessage("Error severity message")
            .withSeverity(TypedValue.ERROR_SEVERITY)
            .build();
    private static Report notSeverityReport = Report.builder()
            .withKey("notSeverity")
            .withDefaultMessage("Not a severity message")
            .withValue("rand", "random value")
            .build();

}
