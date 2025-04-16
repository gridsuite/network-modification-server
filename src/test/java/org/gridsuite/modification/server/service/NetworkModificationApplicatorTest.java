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
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;

import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@Tag("UnitTest")
class NetworkModificationApplicatorTest {

    @Mock
    private NetworkStoreService networkStoreService;

    @Mock
    private EquipmentInfosService equipmentInfosService;

    @Mock
    private ReportService reportService;

    @Mock
    private FilterService filterService;

    @Mock
    private NetworkModificationObserver networkModificationObserver;

    @Mock
    private LargeNetworkModificationExecutionService largeNetworkModificationExecutionService;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }

    @Test
    void testApplyModificationsWithAllCollectionsNeededForBusView() {
        List<ModificationInfos> modificationInfosList = List.of(mock(ModificationInfos.class));
        NetworkInfos networkInfos = mock(NetworkInfos.class);
        ReportInfos reportInfos = mock(ReportInfos.class);

        NetworkModificationApplicator applicator = new NetworkModificationApplicator(
                networkStoreService, equipmentInfosService, reportService, filterService, networkModificationObserver, largeNetworkModificationExecutionService);

        ModificationType mockModificationType = mock(ModificationType.class);
        when(modificationInfosList.get(0).getType()).thenReturn(mockModificationType);
        when(mockModificationType.getStrategy()).thenReturn(PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW);
        when(largeNetworkModificationExecutionService.supplyAsync(any())).thenReturn(CompletableFuture.completedFuture(NetworkModificationResult.builder().build()));

        NetworkModificationResult result = applicator.applyModifications(modificationInfosList, networkInfos, reportInfos);

        assertNotNull(result);
        verify(largeNetworkModificationExecutionService).supplyAsync(any());
    }

    @Test
    void testApplyModificationsWithGroupsAndAllCollectionsNeededForBusView() {
        List<Pair<ReportInfos, List<ModificationInfos>>> modificationInfosGroups = List.of(Pair.of(mock(ReportInfos.class), List.of(mock(ModificationInfos.class))));
        NetworkInfos networkInfos = mock(NetworkInfos.class);

        NetworkModificationApplicator applicator = new NetworkModificationApplicator(
                networkStoreService, equipmentInfosService, reportService, filterService, networkModificationObserver, largeNetworkModificationExecutionService);

        ModificationType mockModificationType = mock(ModificationType.class);
        when(modificationInfosGroups.get(0).getRight().get(0).getType()).thenReturn(mockModificationType);
        when(mockModificationType.getStrategy()).thenReturn(PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW);
        when(largeNetworkModificationExecutionService.supplyAsync(any())).thenReturn(CompletableFuture.completedFuture(NetworkModificationResult.builder().build()));

        NetworkModificationResult result = applicator.applyModifications(modificationInfosGroups, networkInfos);

        assertNotNull(result);
        verify(largeNetworkModificationExecutionService).supplyAsync(any());
    }

    @ParameterizedTest
    @MethodSource("provideArgumentsForComputeHigherSeverity")
    void computeHigherSeverity(List<ReportNode> reports, ApplicationStatus expectedSeverity) {

        assertEquals(6, reports.size(), "We need exactly 6 reports to run the test");

        ReportNode rootReportNode = ReportNode.newRootReportNode()
                .withResourceBundles("i18n.reports")
                .withMessageTemplate("rep1")
                .build();

        ReportNode subReportNode1 = rootReportNode.newReportNode().withMessageTemplate("subrep1").add();
        ReportNode subReportNode2 = rootReportNode.newReportNode().withMessageTemplate("subrep2").add();
        ReportNode subReportNode3 = rootReportNode.newReportNode().withMessageTemplate("rep2").add();

        ReportNode subSubReportNode1 = subReportNode3.newReportNode().withMessageTemplate("subsubrep1").add();
        ReportNode subSubReportNode2 = subReportNode3.newReportNode().withMessageTemplate("subsubrep2").add();

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
        ReportNodeAdder adder = parent.newReportNode()
                .withMessageTemplate("message")
                .withUntypedValue("message", child.getMessageTemplate());
        TypedValue severity = child.getValue(ReportConstants.SEVERITY_KEY).orElse(null);
        if (severity != null) {
            adder.withSeverity(severity);
        }
        adder.add();
    }

    @Test
    void shouldThrowExceptionOnBadSeverity() {
        ReportNode rootReportNode = ReportNode.newRootReportNode()
                .withResourceBundles("i18n.reports")
                .withMessageTemplate("rep1")
                .build();
        rootReportNode.newReportNode()
                .withMessageTemplate("badSeverity")
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
            .withResourceBundles("i18n.reports")
            .withMessageTemplate("info")
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build();

    private static ReportNode warningReport = ReportNode.newRootReportNode()
            .withResourceBundles("i18n.reports")
            .withMessageTemplate("warning")
            .withSeverity(TypedValue.WARN_SEVERITY)
            .build();

    private static ReportNode errorReport = ReportNode.newRootReportNode()
            .withResourceBundles("i18n.reports")
            .withMessageTemplate("error")
            .withSeverity(TypedValue.ERROR_SEVERITY)
            .build();

    private static ReportNode notSeverityReport = ReportNode.newRootReportNode()
            .withResourceBundles("i18n.reports")
            .withMessageTemplate("notSeverity")
            .withUntypedValue("rand", "random value")
            .build();
}
