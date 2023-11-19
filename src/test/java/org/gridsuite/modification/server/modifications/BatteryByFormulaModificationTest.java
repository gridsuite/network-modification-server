/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.junit.Test;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.createBattery;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class BatteryByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final String BATTERY_ID_1 = "v3Battery";
    private static final String BATTERY_ID_2 = "battery2";
    private static final String BATTERY_ID_3 = "battery3";
    private static final String BATTERY_ID_4 = "battery4";
    private static final String BATTERY_ID_5 = "battery5";
    private static final String BATTERY_ID_6 = "battery6";

    @Test
    public void testCreateWithWarning() throws Exception {
        UUID filterId = UUID.randomUUID();
        String equipmentId = "v3Battery";
        IdentifiableAttributes identifiableAttributes1 = getIdentifiableAttributes(equipmentId, 1.0);
        FilterEquipments filter = getFilterEquipments(filterId, "filterWithWrongId", List.of(identifiableAttributes1), List.of("wrongId"));
        var filterInfo = FilterInfos.builder()
                .id(filterId)
                .name("filterWithWrongId")
                .build();

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/export\\?networkUuid=" + getNetworkUuid() + "&variantId=variant_1&ids=" + filterId))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader("Content-Type", "application/json"))).getId();

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterInfo))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(20.).build())
                .build();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(List.of(formulaInfos))
                .identifiableType(IdentifiableType.BATTERY)
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);
        assertEquals(75, getNetwork().getBattery(equipmentId).getTargetP(), 0);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), List.of(filterId)), false);
    }

    @Test
    public void testCreateWithError() throws Exception {
        UUID filterId = UUID.randomUUID();
        FilterEquipments filter = getFilterEquipments(filterId, "filterWithWrongId", List.of(), List.of("wrongId1", "wrongId2"));
        var filterInfo = FilterInfos.builder()
                .id(filterId)
                .name("filterWithWrongId")
                .build();

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/export\\?networkUuid=" + getNetworkUuid() + "&variantId=variant_1&ids=" + filterId))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader("Content-Type", "application/json"))).getId();

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterInfo))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(20.).build())
                .build();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(List.of(formulaInfos))
                .identifiableType(IdentifiableType.BATTERY)
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), List.of(filterId)), false);
    }

    @Override
    void createEquipments() {
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getBattery(BATTERY_ID_1).setTargetP(100).setMaxP(500).setMinP(0).setTargetQ(80);
        getNetwork().getBattery(BATTERY_ID_1).newExtension(ActivePowerControlAdder.class).withDroop(1).add();

        createBattery(getNetwork().getVoltageLevel("v2"), BATTERY_ID_2, "v2Battery2", 20, 50, 2000, 200, 50);
        createBattery(getNetwork().getVoltageLevel("v3"), BATTERY_ID_3, "v3Battery3", 30, 70, 400, 300, 50);

        createBattery(getNetwork().getVoltageLevel("v4"), BATTERY_ID_4, "v4Battery4", 40, 25, 350, 70, 50);

        createBattery(getNetwork().getVoltageLevel("v5"), BATTERY_ID_5, "v5Battery5", 50, 50, 600, 55, 140);
        getNetwork().getBattery(BATTERY_ID_5).newExtension(ActivePowerControlAdder.class).withDroop(4).add();

        createBattery(getNetwork().getVoltageLevel("v6"), BATTERY_ID_6, "v6Battery6", 60, 200, 700, 250, 210);
    }

    @Override
    List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes battery1 = getIdentifiableAttributes(BATTERY_ID_1, 1.0);
        IdentifiableAttributes battery2 = getIdentifiableAttributes(BATTERY_ID_2, 2.0);
        IdentifiableAttributes battery3 = getIdentifiableAttributes(BATTERY_ID_3, 2.0);
        IdentifiableAttributes battery4 = getIdentifiableAttributes(BATTERY_ID_4, 5.0);
        IdentifiableAttributes battery5 = getIdentifiableAttributes(BATTERY_ID_5, 6.0);
        IdentifiableAttributes battery6 = getIdentifiableAttributes(BATTERY_ID_6, 7.0);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(battery1, battery2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(battery3, battery4), List.of());
        FilterEquipments filter3 = getFilterEquipments(FILTER_ID_3, "filter3", List.of(battery5, battery6), List.of());
        FilterEquipments filter4 = getFilterEquipments(FILTER_ID_4, "filter4", List.of(battery1, battery5), List.of());
        FilterEquipments filter5 = getFilterEquipments(FILTER_ID_5, "filter5", List.of(battery2, battery3), List.of());

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    List<FormulaInfos> getFormulaInfos() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter3")
                .build();

        var filter4 = FilterInfos.builder()
                .id(FILTER_ID_4)
                .name("filter4")
                .build();

        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter5")
                .build();

        ReferenceFieldOrValue maxActivePowerRef = ReferenceFieldOrValue.builder().equipmentField(BatteryField.MAXIMUM_ACTIVE_POWER.name()).build();
        ReferenceFieldOrValue minActivePowerRef = ReferenceFieldOrValue.builder().equipmentField(BatteryField.MINIMUM_ACTIVE_POWER.name()).build();

        FormulaInfos formulaInfos1 = getFormulaInfo(BatteryField.MAXIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                maxActivePowerRef,
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(BatteryField.MINIMUM_ACTIVE_POWER.name(),
                List.of(filter3),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(30.).build(),
                minActivePowerRef);

        FormulaInfos formulaInfos3 = getFormulaInfo(BatteryField.ACTIVE_POWER_SET_POINT.name(),
                List.of(filter5),
                Operator.SUBTRACTION,
                maxActivePowerRef,
                minActivePowerRef);

        FormulaInfos formulaInfos4 = getFormulaInfo(BatteryField.REACTIVE_POWER_SET_POINT.name(),
                List.of(filter4),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(BatteryField.REACTIVE_POWER_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos5 = getFormulaInfo(BatteryField.DROOP.name(),
                List.of(filter4),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(BatteryField.DROOP.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4, formulaInfos5);
    }

    @Override
    List<FormulaInfos> getUpdatedFormulaInfos() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter3")
                .build();

        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter5")
                .build();

        FormulaInfos formulaInfos1 = FormulaInfos.builder()
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(200.).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(BatteryField.MAXIMUM_ACTIVE_POWER.name()).build())
                .operator(Operator.ADDITION)
                .filters(List.of(filter1, filter2))
                .build();

        FormulaInfos formulaInfos2 = FormulaInfos.builder()
                .editedField(BatteryField.MINIMUM_ACTIVE_POWER.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(35.).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(BatteryField.MINIMUM_ACTIVE_POWER.name()).build())
                .operator(Operator.PERCENTAGE)
                .filters(List.of(filter3))
                .build();

        FormulaInfos formulaInfos3 = FormulaInfos.builder()
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().equipmentField(BatteryField.ACTIVE_POWER_SET_POINT.name()).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(10.).build())
                .operator(Operator.ADDITION)
                .filters(List.of(filter5))
                .build();

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(550, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(40, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(2, activePowerControl1.getDroop(), 0);

        assertEquals(2050, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(2000, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(450, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(380, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(400, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(15, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(70, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(8, activePowerControl5.getDroop(), 0);

        assertEquals(60, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(500, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(1, activePowerControl1.getDroop(), 0);

        assertEquals(2000, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(200, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(400, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(300, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(350, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(50, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(140, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(4, activePowerControl5.getDroop(), 0);

        assertEquals(200, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);
    }
}
