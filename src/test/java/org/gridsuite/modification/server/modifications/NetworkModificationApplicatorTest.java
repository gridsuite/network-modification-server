/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.ReportNodeAdder;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.VoltageInitModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.dto.ModificationApplicationGroup;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.VoltageInitModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.VoltageLevelModificationEntity;
import org.gridsuite.modification.server.network.ModificationNetwork;
import org.gridsuite.modification.server.network.ModificationNetworkService;
import org.gridsuite.modification.server.network.NetworkUtils;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.NetworkModificationObserver;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.bean.override.mockito.MockitoSpyBean;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@DisableElasticsearch
@Tag("UnitTest")
class NetworkModificationApplicatorTest {

    @MockitoBean
    private ModificationNetworkService modificationNetworkService;

    private MockedStatic<NetworkUtils> networkUtils;

    @MockitoBean
    private ReportService reportService;

    @MockitoBean
    private FilterService filterService;

    @MockitoBean
    private NetworkModificationObserver networkModificationObserver;

    @MockitoSpyBean
    private LargeNetworkModificationExecutionService largeNetworkModificationExecutionService;

    @Autowired
    private NetworkModificationApplicator networkModificationApplicator;

    @Mock
    private ReportInfos reportInfos;

    private final UUID networkUuid = UUID.randomUUID();

    private final String variantId = UUID.randomUUID().toString();

    @BeforeEach
    void setUp() {
        Network network = mock(Network.class);
        ModificationNetwork modificationNetwork = mock(ModificationNetwork.class);
        when(modificationNetwork.network()).thenReturn(network);
        networkUtils = mockStatic(NetworkUtils.class);
        networkUtils.when(() -> NetworkUtils.switchOnExistingVariant(eq(network), eq(variantId))).thenReturn(true);
        when(modificationNetworkService.getNetwork(eq(networkUuid), any())).thenReturn(modificationNetwork);
    }

    @AfterEach
    void tearDown() {
        networkUtils.close();
    }

    @Test
    void testApplyModificationsWithAllCollectionsNeededForBusView() {
        var modificationInfosPreloadingAllCollections = VoltageInitModificationInfos.builder().build();
        var modificationInfosPreloadingNone = VoltageLevelModificationInfos.builder()
            .equipmentId(UUID.randomUUID().toString())
            .build();
        List<ModificationEntity> modificationInfosList = List.of(
            new VoltageInitModificationEntity(modificationInfosPreloadingAllCollections),
            new VoltageLevelModificationEntity(modificationInfosPreloadingNone)
        );

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(
            networkModificationApplicator,
            new ModificationApplicationGroup(UUID.randomUUID(), modificationInfosList, reportInfos),
            networkUuid,
            variantId);

        assertNotNull(result);
        verify(largeNetworkModificationExecutionService).supplyAsync(any());
    }

    @Test
    void testApplyModificationsWithAllCollectionsNeededForBusViewStashedAndNotActivated() {
        var modificationInfosPreloadingNone = VoltageLevelModificationInfos.builder()
            .equipmentId(UUID.randomUUID().toString())
            .build();
        List<ModificationEntity> modificationInfosList = List.of(
            new VoltageLevelModificationEntity(modificationInfosPreloadingNone)
        );

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(
            networkModificationApplicator,
            new ModificationApplicationGroup(UUID.randomUUID(), modificationInfosList, reportInfos),
            networkUuid,
            variantId);

        assertNotNull(result);
        verifyNoInteractions(largeNetworkModificationExecutionService);
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
