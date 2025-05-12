/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurve;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class BatteryModificationTest extends AbstractInjectionModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final String ERROR_MESSAGE_KEY = "network.modification.server.errorMessage";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return BatteryModificationInfos.builder()
                .stashed(false)
                .equipmentId("v3Battery")
                .equipmentName(new AttributeModification<>("newV1Battery", OperationType.SET))
                .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                .targetP(new AttributeModification<>(80.0, OperationType.SET))
                .targetQ(new AttributeModification<>(40.0, OperationType.SET))
                .minP(new AttributeModification<>(0., OperationType.SET))
                .maxP(new AttributeModification<>(100., OperationType.SET))
                .minQ(new AttributeModification<>(-100., OperationType.SET))
                .maxP(new AttributeModification<>(100., OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of(
                        new ReactiveCapabilityCurvePointsInfos(100., 100., 0.1),
                        new ReactiveCapabilityCurvePointsInfos(100., 100., 150.)))
                .droop(new AttributeModification<>(0.1f, OperationType.SET))
                .participate(new AttributeModification<>(true, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BatteryModificationInfos.builder()
                .stashed(false)
                .equipmentId("idBatteryEdited")
                .equipmentName(new AttributeModification<>("newV1BatteryEdited", OperationType.SET))
                .targetP(new AttributeModification<>(81.0, OperationType.SET))
                .targetQ(new AttributeModification<>(41.0, OperationType.SET))
                .minP(new AttributeModification<>(1., OperationType.SET))
                .maxP(new AttributeModification<>(102., OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Battery modifiedBattery = getNetwork().getBattery("v3Battery");
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        assertEquals("newV1Battery", modifiedBattery.getNameOrId());
        assertEquals(80.0, modifiedBattery.getTargetP());
        assertEquals(40.0, modifiedBattery.getTargetQ());
        assertEquals(0., modifiedBattery.getMinP());
        assertEquals(100., modifiedBattery.getMaxP());
        assertEquals(0.1f, modifiedBattery.getExtension(ActivePowerControl.class).getDroop());
        assertTrue(modifiedBattery.getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedBattery.getReactiveLimits().getKind());
        Collection<ReactiveCapabilityCurve.Point> points = modifiedBattery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = batteryModificationInfos.getReactiveCapabilityCurvePoints();
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, batteryPoints.size())
                    .forEach(i -> {
                        var point = batteryPoints.get(i);
                        var modificationPoint = modificationPoints.get(i);
                        assertEquals(modificationPoint.getMaxQ(), point.getMaxQ());
                        assertEquals(modificationPoint.getMinQ(), point.getMinQ());
                        assertEquals(modificationPoint.getP(), point.getP());
                    });
        }
        assertEquals(PROPERTY_VALUE, getNetwork().getBattery("v3Battery").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        Battery battery = getNetwork().getBattery("v3Battery");
        assertEquals("v3Battery", battery.getNameOrId());
        assertEquals(1.0, battery.getTargetP());
        assertEquals(1.0, battery.getTargetQ());
        assertEquals(0.0, battery.getMinP());
        assertEquals(10.0, battery.getMaxP());
        assertEquals(ReactiveLimitsKind.MIN_MAX, battery.getReactiveLimits().getKind());
    }

    @Test
    void testMinMaxReactiveLimitsAttributesModification() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();

        //setting ReactiveCapabilityCurve to false with null min and max reactive limits
        batteryModificationInfos.setReactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET));
        batteryModificationInfos.setMaxQ(null);
        batteryModificationInfos.setMinQ(null);
        //setting ReactiveCapabilityCurvePoints for the battery we are modifying
        Battery battery = getNetwork().getBattery("v3Battery");
        battery.newReactiveCapabilityCurve()
                .beginPoint()
                .setP(0.)
                .setMaxQ(100.)
                .setMinQ(0.)
                .endPoint()
                .beginPoint()
                .setP(200.)
                .setMaxQ(150.)
                .setMinQ(0.)
                .endPoint()
                .add();
        String modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        BatteryModificationInfos createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // Modifying only min reactive limit
        batteryModificationInfos.setMinQ(new AttributeModification<>(-200., OperationType.SET));
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);

        // Modifying only max reactive limit
        batteryModificationInfos.setMinQ(null);
        batteryModificationInfos.setMaxQ(new AttributeModification<>(200., OperationType.SET));
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 3);

        // Modifying both min and max reactive limits
        batteryModificationInfos.setMinQ(new AttributeModification<>(-1.1, OperationType.SET));
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 4);

        // nothing before reactive limits modification
        batteryModificationInfos = (BatteryModificationInfos) buildModification();
        batteryModificationInfos.setEquipmentName(null);
        batteryModificationInfos.setMinP(null);
        batteryModificationInfos.setMaxP(null);
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);
        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 5);
    }

    @Test
    void testDroopUnchanged() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();

        batteryModificationInfos.getDroop().setValue(18f);
        String modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        BatteryModificationInfos createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);

        // setting droop to null, modifying only participate
        batteryModificationInfos.setDroop(null);
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertEquals(18f, createdModification.getDroop().getValue());
    }

    @Test
    void testImpactsAfterActivePowerControlModifications() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        String modificationToCreateJson = getJsonBody(batteryModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        Battery battery = getNetwork().getBattery("v3Battery");
        assertEquals(0.1f, battery.getExtension(ActivePowerControl.class).getDroop());
        assertTrue(battery.getExtension(ActivePowerControl.class).isParticipate());
        //modify only droop
        batteryModificationInfos.setDroop(new AttributeModification<>(0.5f, OperationType.SET));
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        //check impacts
        String resultAsString = mvcResult.getResponse().getContentAsString();
        NetworkModificationsResult networkModificationsResult = mapper.readValue(resultAsString, NetworkModificationsResult.class);
        assertEquals(1, getNetworkImpacts(networkModificationsResult).size());
        assertEquals(1, getImpactedSubstationsIds(networkModificationsResult).size());
        assertEquals("[s2]", getImpactedSubstationsIds(networkModificationsResult).toString());
        //modify only participate
        batteryModificationInfos.setParticipate(new AttributeModification<>(false, OperationType.SET));
        modificationToCreateJson = getJsonBody(batteryModificationInfos, null);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        //check impacts
        resultAsString = mvcResult.getResponse().getContentAsString();
        networkModificationsResult = mapper.readValue(resultAsString, NetworkModificationsResult.class);
        assertEquals(1, getNetworkImpacts(networkModificationsResult).size());
        assertEquals(1, getImpactedSubstationsIds(networkModificationsResult).size());
        assertEquals("[s2]", getImpactedSubstationsIds(networkModificationsResult).toString());

    }

    @Test
    void testActivePowerZeroOrBetweenMinAndMaxActivePower() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        Battery battery = getNetwork().getBattery("v3Battery");
        battery.setTargetP(80.)
                .setMinP(0.)
                .setMaxP(100.);
        batteryModificationInfos.setTargetP(new AttributeModification<>(155.0, OperationType.SET));

        Double minActivePower = batteryModificationInfos.getMinP() != null ? batteryModificationInfos.getMinP().getValue() : battery.getMinP();
        Double maxActivePower = batteryModificationInfos.getMaxP() != null ? batteryModificationInfos.getMaxP().getValue() : battery.getMaxP();
        Double activePower = batteryModificationInfos.getTargetP() != null ? batteryModificationInfos.getTargetP().getValue() : battery.getTargetP();

        String modificationToCreateJson = getJsonBody(batteryModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : Active power " + activePower + " is expected to be equal to 0 or within the range of minimum active power and maximum active power: [" + minActivePower + ", " + maxActivePower + "]",
                ERROR_MESSAGE_KEY, reportService);

    }

    @Test
    void testMinQGreaterThanMaxQ() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();
        Battery battery = getNetwork().getBattery("v3Battery");
        battery.newReactiveCapabilityCurve()
                .beginPoint()
                .setP(0.)
                .setMaxQ(100.)
                .setMinQ(0.)
                .endPoint()
                .beginPoint()
                .setP(200.)
                .setMaxQ(150.)
                .setMinQ(0.)
                .endPoint()
                .add();
        Collection<ReactiveCapabilityCurve.Point> points = battery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = batteryModificationInfos.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurve.Point oldPoint = batteryPoints.get(i);
                        ReactiveCapabilityCurvePointsInfos newPoint = modificationPoints.get(i);
                        Double oldMaxQ = Double.NaN;
                        Double oldMinQ = Double.NaN;
                        if (oldPoint != null) {
                            oldMaxQ = oldPoint.getMaxQ();
                            oldMinQ = oldPoint.getMinQ();
                        }
                        newPoint.setMinQ(300.0);
                        maxQ.set(newPoint.getMaxQ() != null ? newPoint.getMaxQ() : oldMaxQ);
                        minQ.set(newPoint.getMinQ() != null ? newPoint.getMinQ() : oldMinQ);
                    });
        }
        String modificationToCreateJson = getJsonBody(batteryModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : maximum reactive power " + maxQ.get() + " is expected to be greater than or equal to minimum reactive power " + minQ.get(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v3Battery", updatedValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BATTERY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBatteryEdited", updatedValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getBattery("v3Battery"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getBattery("v3Battery"), true);
    }
}
