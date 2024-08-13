/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.databind.InjectableValues;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.ReportNodeDeserializer;
import com.powsybl.commons.report.ReportNodeJsonModule;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ThreeSides;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.NetworkStoreServicePublic;
import com.powsybl.network.store.client.PreloadingStrategy;
import com.powsybl.network.store.client.RestClient;
import com.powsybl.network.store.iidm.impl.CachedNetworkStoreClient;
import com.powsybl.network.store.iidm.impl.OfflineNetworkStoreClient;
import io.micrometer.observation.ObservationRegistry;
import lombok.extern.slf4j.Slf4j;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.service.NetworkModificationObserver;
import org.gridsuite.modification.server.service.ReportService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyscreamer.jsonassert.JSONAssert;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;

@Slf4j
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@ExtendWith({MockitoExtension.class})
class VoltageInitReportTest {
    private final ObjectMapper objectMapper = new ObjectMapper().findAndRegisterModules()
            .registerModule(new ReportNodeJsonModule())
            .setInjectableValues(new InjectableValues.Std().addValue(ReportNodeDeserializer.DICTIONARY_VALUE_ID, null));

    @SuppressWarnings("DataFlowIssue") //for .toURI() nullable warning
    @MethodSource("voltageInitModifications")
    @ParameterizedTest(name = "result status = {0}")
    @DisplayName("Verifying logs after applying Voltage-Init server modifications on a network")
    void testVoltageInitDuplicationLogs(final ApplicationStatus resultStatus, final String logsJsonFile, final VoltageInitModificationInfos modificationInfos) throws Exception {
        final ReportService reportService = Mockito.mock(ReportService.class);
        final RestClient restClient = Mockito.mock(RestClient.class);
        final NetworkStoreService networkStoreService = new NetworkStoreServicePublic(restClient, PreloadingStrategy.NONE,
            (restClient_, preloadingStrategy, executorService) -> new CachedNetworkStoreClient(new OfflineNetworkStoreClient()));
        final EquipmentInfosService equipmentInfosService = Mockito.mock(EquipmentInfosService.class);
        final NetworkModificationObserver networkModificationObserver = new NetworkModificationObserver(ObservationRegistry.NOOP);
        final NetworkModificationApplicator networkModificationApplicator = new NetworkModificationApplicator(networkStoreService, equipmentInfosService, reportService, null, 2, networkModificationObserver);
        networkModificationApplicator.setCollectionThreshold(5);

        final Network network = Network.read(Paths.get(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm").toURI()));

        //for internal call to reportService.sendReport(reportInfos.getReportUuid(), reporter);
        final ArgumentCaptor<ReportNode> reporterCaptor = ArgumentCaptor.forClass(ReportNode.class);

        final UUID networkUuuid = UUID.fromString("11111111-1111-1111-1111-111111111111");
        final UUID reportUuid = UUID.fromString("88888888-8888-8888-8888-888888888888");
        //simulate PUT /v1/groups/abc/duplications?networkUuid=0000&reportUuid=0000&reporterId=0000&variantId=0000&duplicateFrom=0000
        assertThat(networkModificationApplicator.applyModifications(
                List.of(modificationInfos),
                new NetworkInfos(network, networkUuuid, true),
                new ReportInfos(reportUuid, "99999999-9999-9999-9999-999999999999")))
                .as("network modifications results")
            .isNotNull()
            .extracting(NetworkModificationResult::getApplicationStatus)
            .isEqualTo(resultStatus);

        Mockito.verify(reportService, Mockito.times(1)).sendReport(Mockito.eq(reportUuid), reporterCaptor.capture());
        final ReportNode result = reporterCaptor.getValue();
        log.info("Result = {}", objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(result));
        JSONAssert.assertEquals("voltage-init plan logs aggregated",
                Files.readString(Paths.get(this.getClass().getClassLoader().getResource(logsJsonFile).toURI())),
                objectMapper.writeValueAsString(result), false);
        Mockito.verifyNoMoreInteractions(reportService);
        Mockito.reset(reportService); //because parametized tests haven't same lifecycle than tests
    }

    private static List<Arguments> voltageInitModifications() {
        return List.of(
            Arguments.of(ApplicationStatus.ALL_OK, "reports_voltage_init_modification_ok.json", VoltageInitModificationInfos.builder()
                .uuid(UUID.fromString("44444444-4444-4444-4444-444444444444"))
                .date(Instant.EPOCH)
                .stashed(false)
                .generators(List.of(
                    VoltageInitGeneratorModificationInfos.builder().generatorId("GTH2").targetV(0.1).build())) //added for test case
                .transformers(List.of(
                    VoltageInitTransformerModificationInfos.builder().transformerId("TWT2").ratioTapChangerPosition(2).build()))
                .staticVarCompensators(List.of(
                    VoltageInitStaticVarCompensatorModificationInfos.builder().staticVarCompensatorId("SVC").reactivePowerSetpoint(1346.7).build()))
                .vscConverterStations(List.of(
                    VoltageInitVscConverterStationModificationInfos.builder().vscConverterStationId("VSC2").reactivePowerSetpoint(326.6).build()))
                .shuntCompensators(List.of(
                    VoltageInitShuntCompensatorModificationInfos.builder().shuntCompensatorId("SHUNT2").sectionCount(1).connect(true).build(),
                    VoltageInitShuntCompensatorModificationInfos.builder().shuntCompensatorId("SHUNT3").sectionCount(0).connect(false).build())) //altered for test case
                .buses(List.of())
                .build()),
            Arguments.of(ApplicationStatus.WITH_WARNINGS, "reports_voltage_init_modification_warnings.json", VoltageInitModificationInfos.builder()
                .uuid(UUID.fromString("44444444-4444-4444-4444-444444444444"))
                .date(Instant.EPOCH)
                .stashed(false)
                .generators(List.of(
                    VoltageInitGeneratorModificationInfos.builder().generatorId("G1").targetQ(10.).build(),
                    VoltageInitGeneratorModificationInfos.builder().generatorId("G2").targetV(226.).build()))
                .transformers(List.of(
                    VoltageInitTransformerModificationInfos.builder().transformerId("2WT1").ratioTapChangerPosition(3).build(),
                    VoltageInitTransformerModificationInfos.builder().transformerId("3WT1").ratioTapChangerPosition(1).legSide(ThreeSides.TWO).build()))
                .staticVarCompensators(List.of(
                    VoltageInitStaticVarCompensatorModificationInfos.builder().staticVarCompensatorId("SVC1").reactivePowerSetpoint(50.).build(),
                    VoltageInitStaticVarCompensatorModificationInfos.builder().staticVarCompensatorId("SVC2").voltageSetpoint(374.).build()))
                .vscConverterStations(List.of(
                    VoltageInitVscConverterStationModificationInfos.builder().vscConverterStationId("VSC1").reactivePowerSetpoint(40.).build(),
                    VoltageInitVscConverterStationModificationInfos.builder().vscConverterStationId("VSC2").voltageSetpoint(224.).build()))
                .shuntCompensators(List.of(
                    VoltageInitShuntCompensatorModificationInfos.builder().shuntCompensatorId("v2shunt").sectionCount(1).connect(true).build(),
                    VoltageInitShuntCompensatorModificationInfos.builder().shuntCompensatorId("v5shunt").sectionCount(0).connect(false).build(),
                    VoltageInitShuntCompensatorModificationInfos.builder().shuntCompensatorId("v6shunt").sectionCount(1).connect(false).build()))
                .buses(List.of())
                .build())
        );
    }
}
