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
import lombok.extern.slf4j.Slf4j;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult.ApplicationStatus;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.assertj.core.api.Assertions.assertThat;

@Slf4j
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@ExtendWith({MockitoExtension.class})
@SpringBootTest
@DisableElasticsearch
class VoltageInitReportTest {

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Autowired
    private NetworkModificationApplicator networkModificationApplicator;

    @MockitoBean
    protected ReportService reportService;

    private static final UUID NETWORK_ID = UUID.fromString("11111111-1111-1111-1111-111111111111");
    private static final UUID NODE_ID = UUID.fromString("99999999-9999-9999-9999-999999999999");
    private static final UUID REPORT_ID = UUID.fromString("88888888-8888-8888-8888-888888888888");
    private static final UUID GROUP_ID = UUID.randomUUID();

    private final ObjectMapper objectMapper = new ObjectMapper().findAndRegisterModules()
            .registerModule(new ReportNodeJsonModule())
            .setInjectableValues(new InjectableValues.Std().addValue(ReportNodeDeserializer.DICTIONARY_VALUE_ID, null));

    @SuppressWarnings("DataFlowIssue") //for .toURI() nullable warning
    @MethodSource("voltageInitModifications")
    @ParameterizedTest(name = "result status = {0}")
    @DisplayName("Verifying logs after applying Voltage-Init server modifications on a network")
    void testVoltageInitDuplicationLogs(final ApplicationStatus resultStatus, final String logsJsonFile, final VoltageInitModificationInfos modificationInfos) throws Exception {
        final Network network = Network.read(Paths.get(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm").toURI()));

        // apply a VoltageInit modification and check status
        assertThat(applyModification(network, modificationInfos))
                .as("voltage init result status")
                .isEqualTo(resultStatus);

        // check produced report json data
        ArgumentCaptor<ReportNode> reporterCaptor = ArgumentCaptor.forClass(ReportNode.class);
        verify(reportService, atLeast(1)).sendReport(any(UUID.class), reporterCaptor.capture(), eq(ReportMode.APPEND));
        final ReportNode result = reporterCaptor.getValue();
        assertNotNull(result);
        JSONAssert.assertEquals("voltage-init plan logs aggregated",
                Files.readString(Paths.get(this.getClass().getClassLoader().getResource(logsJsonFile).toURI())),
                objectMapper.writeValueAsString(result), false);
        Mockito.verifyNoMoreInteractions(reportService);
        Mockito.reset(reportService); //because parameterized tests haven't same lifecycle than tests
    }

    private ApplicationStatus applyModification(Network network, VoltageInitModificationInfos infos) {
        List<ModificationInfos> modifications = modificationRepository.saveModifications(GROUP_ID, List.of(ModificationEntity.fromDTO(infos)));
        List<ModificationApplicationGroup> modificationInfosGroups = List.of(
                new ModificationApplicationGroup(GROUP_ID, modifications, new ReportInfos(REPORT_ID, NODE_ID))
        );
        NetworkModificationResult result = networkModificationApplicator.applyModifications(modificationInfosGroups, new NetworkInfos(network, NETWORK_ID, true));
        return result.getApplicationStatus();
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
