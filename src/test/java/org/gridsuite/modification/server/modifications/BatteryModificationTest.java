/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class BatteryModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return BatteryModificationInfos.builder()
                .equipmentId("v3Battery")
                .equipmentName(new AttributeModification<>("newV1Battery", OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
                .minActivePower(new AttributeModification<>(0., OperationType.SET))
                .maxActivePower(new AttributeModification<>(100., OperationType.SET))
                .minimumReactivePower(new AttributeModification<>(-100., OperationType.SET))
                .maximumReactivePower(new AttributeModification<>(100., OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of(
                        new ReactiveCapabilityCurveModificationInfos(0., 0., 100., 100., 0., 0.1),
                        new ReactiveCapabilityCurveModificationInfos(0., 0., 100., 100., 200., 150.)))
                .droop(new AttributeModification<>(0.1f, OperationType.SET))
                .participate(new AttributeModification<>(true, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return BatteryModificationInfos.builder()
                .equipmentId("idBatteryEdited")
                .equipmentName(new AttributeModification<>("newV1BatteryEdited", OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(81.0, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(41.0, OperationType.SET))
                .minActivePower(new AttributeModification<>(1., OperationType.SET))
                .maxActivePower(new AttributeModification<>(102., OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of())
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
        assertEquals(true, modifiedBattery.getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedBattery.getReactiveLimits().getKind());
        Collection<ReactiveCapabilityCurve.Point> points = modifiedBattery.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> batteryPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = batteryModificationInfos.getReactiveCapabilityCurvePoints();
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, batteryPoints.size())
                    .forEach(i -> {
                        var point = batteryPoints.get(i);
                        var modificationPoint = modificationPoints.get(i);
                        assertEquals(modificationPoint.getQmaxP(), point.getMaxQ());
                        assertEquals(modificationPoint.getQminP(), point.getMinQ());
                        assertEquals(modificationPoint.getP(), point.getP());
                    });
        }
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
    public void testMinMaxReactiveLimitsAttributesModification() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();

        //setting ReactiveCapabilityCurve to false with null min and max reactive limits
        batteryModificationInfos.setReactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET));
        batteryModificationInfos.setMaximumReactivePower(null);
        batteryModificationInfos.setMinimumReactivePower(null);
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
        String modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        BatteryModificationInfos createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // Modifying only min reactive limit
        batteryModificationInfos.setMinimumReactivePower(new AttributeModification<>(-200., OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);

        // Modifying only max reactive limit
        batteryModificationInfos.setMinimumReactivePower(null);
        batteryModificationInfos.setMaximumReactivePower(new AttributeModification<>(200., OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 3);

        // Modifying both min and max reactive limits
        batteryModificationInfos.setMinimumReactivePower(new AttributeModification<>(-1.1, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 4);

        // nothing before reactive limits modification
        batteryModificationInfos = (BatteryModificationInfos) buildModification();
        batteryModificationInfos.setEquipmentName(null);
        batteryModificationInfos.setMinActivePower(null);
        batteryModificationInfos.setMaxActivePower(null);
        modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);
        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);
        testNetworkModificationsCount(getGroupId(), 5);
    }

    @Test
    public void testDroopUnchanged() throws Exception {
        BatteryModificationInfos batteryModificationInfos = (BatteryModificationInfos) buildModification();

        batteryModificationInfos.getDroop().setValue(18f);
        String modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        BatteryModificationInfos createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(batteryModificationInfos);

        // setting droop to null, modifying only participate
        batteryModificationInfos.setDroop(null);
        modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (BatteryModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertEquals(18f, createdModification.getDroop().getValue());
    }

    @Test
    public void testMinQGreaterThanMaxQ() throws Exception {
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
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = batteryModificationInfos.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurve.Point oldPoint = batteryPoints.get(i);
                        ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                        Double oldMaxQ = Double.NaN;
                        Double oldMinQ = Double.NaN;
                        if (oldPoint != null) {
                            oldMaxQ = oldPoint.getMaxQ();
                            oldMinQ = oldPoint.getMinQ();
                        }
                        newPoint.setQminP(300.0);
                        newPoint.setOldQmaxP(250.0);
                        maxQ.set(newPoint.getQmaxP() != null ? newPoint.getQmaxP() : oldMaxQ);
                        minQ.set(newPoint.getQminP() != null ? newPoint.getQminP() : oldMinQ);
                    });
        }
        String modificationToCreateJson = mapper.writeValueAsString(batteryModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("MODIFY_BATTERY_ERROR : Battery '" + "v3Battery" + "' : maximum reactive power " + maxQ.get() + " is expected to be greater than or equal to minimum reactive power " + minQ.get(),
                batteryModificationInfos.getErrorType().name(), reportService);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BATTERY_MODIFICATION");
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v3Battery", updatedValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals(modificationInfos.getMessageType(), "BATTERY_MODIFICATION");
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idBattery2Edited", updatedValues.get("equipmentId"));
    }
}
