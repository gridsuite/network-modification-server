/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.vladmihalcea.sql.SQLStatementCountValidator;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.junit.Before;
import org.mockito.ArgumentMatchers;
import org.mockito.stubbing.Answer;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.MatcherDeleteVoltageLevelOnLineInfos.createMatcherDeleteVoltageLevelOnLineInfos;
import static org.gridsuite.modification.server.utils.NetworkUtil.createBusBarSection;
import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLine;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSubstation;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSwitch;
import static org.gridsuite.modification.server.utils.NetworkUtil.createVoltageLevel;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;
/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteVoltageLevelOnLineTest extends AbstractNetworkModificationTest {

    private static final UUID TEST_REPORT_ID = UUID.randomUUID();

    private Network network;

    @Before
    public void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        when(networkStoreService.getNetwork(TEST_NETWORK_ID)).then((Answer<Network>) invocation -> {
            network = createNetwork();
            return network;
        });

        networkModificationService.setReportServerRest(reportServerRest);
        given(reportServerRest.exchange(eq("/v1/reports/" + TEST_REPORT_ID), eq(HttpMethod.PUT), ArgumentMatchers.any(HttpEntity.class), eq(ReporterModel.class)))
                .willReturn(new ResponseEntity<>(HttpStatus.OK));

        // clean DB
        modificationRepository.deleteAll();
        SQLStatementCountValidator.reset();
    }

    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteVoltageLevelOnLineInfos.builder()
               .type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
               .lineToAttachTo1Id("l1")
               .lineToAttachTo2Id("l2")
               .replacingLine1Id("replacementLineId")
               .replacingLine1Name("replacementLine")
               .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return DeleteVoltageLevelOnLineInfos.builder()
                .type(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE)
                .lineToAttachTo1Id("line00")
                .lineToAttachTo2Id("line11")
                .replacingLine1Id("replacingLineId2")
                .replacingLine1Name("replacingLine2")
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherDeleteVoltageLevelOnLineInfos((DeleteVoltageLevelOnLineInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("replacingLineId"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("replacingLineIdEdited"));
    }

    private List<DeleteVoltageLevelOnLineInfos> getModifications() {
        return modificationRepository.getModifications(TEST_GROUP_ID, false, true)
                .stream().map(DeleteVoltageLevelOnLineInfos.class::cast).collect(Collectors.toList());
    }

    private Network createNetwork() {
        Network network = new NetworkFactoryImpl().createNetwork(DeleteVoltageLevelOnLineTest.TEST_NETWORK_ID.toString(), "NetworkWithTeePoint");

        // VL1
        Substation s1 = createSubstation(network, "s1", null, Country.FR);
        VoltageLevel v1 = createVoltageLevel(s1, "v1", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v1, "bbs1", null, 0);
        createLoad(v1, "ld1", null, 2, 0., 0., "ld1", 0, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v1, "d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v1, "br1", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL2
        Substation s2 = createSubstation(network, "s2", null, Country.FR);
        VoltageLevel v2 = createVoltageLevel(s2, "v2", null, TopologyKind.NODE_BREAKER, 380);
        createBusBarSection(v2, "bbs2", null, 0);

        createGenerator(v2, "g2", 2, 42.1, 1.0, "g2", 3, ConnectablePosition.Direction.TOP);
        createSwitch(v2, "d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v2, "br2", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // VL3
        Substation s3 = createSubstation(network, "s3", null, Country.FR);
        VoltageLevel v3 = createVoltageLevel(s3, "v3", null, TopologyKind.NODE_BREAKER, 380.0);
        createBusBarSection(v3, "bbs3", null, 0);

        createLoad(v3, "ld3", null, 2, 0., 0., "ld3", 3, ConnectablePosition.Direction.BOTTOM);
        createSwitch(v3, "d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 1);
        createSwitch(v3, "br3", null, SwitchKind.BREAKER, true, false, false, 1, 2);

        // create lines
        createLine(network, "l1", null, "v1", "v2", 4, 4, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, "l1", 1, ConnectablePosition.Direction.TOP, "l1", 1, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l1d1", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l1br1", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v2, "l1d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v2, "l1br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        createLine(network, "l2", null, "v1", "v3", 4, 4, 10.0, 5.0, 3.5, 5.5, 4.5, 6.5, "l2", 2, ConnectablePosition.Direction.TOP, "l2", 2, ConnectablePosition.Direction.TOP);
        createSwitch(v1, "l2d2", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v1, "l2br2", null, SwitchKind.BREAKER, true, false, false, 5, 4);
        createSwitch(v3, "l2d3", null, SwitchKind.DISCONNECTOR, true, false, false, 0, 5);
        createSwitch(v3, "l2br3", null, SwitchKind.BREAKER, true, false, false, 5, 4);

        return network;
    }
}
