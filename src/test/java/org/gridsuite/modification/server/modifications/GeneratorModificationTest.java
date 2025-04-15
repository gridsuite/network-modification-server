/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuit;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.utils.NetworkUtil.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class GeneratorModificationTest extends AbstractInjectionModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GeneratorModificationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator")
                .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
                .voltageLevelId(new AttributeModification<>("v2", OperationType.SET))
                .busOrBusbarSectionId(new AttributeModification<>("1B", OperationType.SET))
                .connectionName(new AttributeModification<>("idGenerator", OperationType.SET))
                .connectionPosition(new AttributeModification<>(1, OperationType.SET))
                .connectionDirection(new AttributeModification<>(ConnectablePosition.Direction.TOP, OperationType.SET))
                .targetP(new AttributeModification<>(80.0, OperationType.SET))
                .targetQ(new AttributeModification<>(40.0, OperationType.SET))
                .targetV(new AttributeModification<>(48.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(false, OperationType.SET))
                .minP(new AttributeModification<>(0., OperationType.SET))
                .maxP(new AttributeModification<>(100., OperationType.SET))
                .ratedS(new AttributeModification<>(220., OperationType.SET))
                .voltageRegulationType(
                        new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .plannedActivePowerSetPoint(new AttributeModification<>(10., OperationType.SET))
                .marginalCost(new AttributeModification<>(0.1, OperationType.SET))
                .plannedOutageRate(new AttributeModification<>(.30, OperationType.SET))
                .forcedOutageRate(new AttributeModification<>(.40, OperationType.SET))
                .minQ(new AttributeModification<>(-100., OperationType.SET))
                .maxQ(new AttributeModification<>(100., OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of(
                        new ReactiveCapabilityCurvePointsInfos(100., 100., 0.1),
                        new ReactiveCapabilityCurvePointsInfos(100., 100., 150.)))
                .droop(new AttributeModification<>(0.1f, OperationType.SET))
                .participate(new AttributeModification<>(true, OperationType.SET))
                .directTransX(new AttributeModification<>(0.1, OperationType.SET))
                .stepUpTransformerX(new AttributeModification<>(0.1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v2load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                .qPercent(new AttributeModification<>(0.1, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GeneratorModificationInfos.builder()
                .equipmentId("idGeneratorEdited")
                .stashed(false)
                .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1GeneratorEdited", OperationType.SET))
                .targetP(new AttributeModification<>(81.0, OperationType.SET))
                .targetQ(new AttributeModification<>(41.0, OperationType.SET))
                .targetV(new AttributeModification<>(49.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
                .minP(new AttributeModification<>(1., OperationType.SET))
                .maxP(new AttributeModification<>(102., OperationType.SET))
                .ratedS(new AttributeModification<>(221., OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET))
                .voltageRegulationType(
                                new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET))
                .plannedActivePowerSetPoint(new AttributeModification<>(111., OperationType.SET))
                .marginalCost(new AttributeModification<>(0.40, OperationType.SET))
                .plannedOutageRate(new AttributeModification<>(.45, OperationType.SET))
                .forcedOutageRate(new AttributeModification<>(.66, OperationType.SET))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Generator modifiedGenerator = getNetwork().getGenerator("idGenerator");
        assertEquals("newV1Generator", modifiedGenerator.getNameOrId());
        assertEquals(EnergySource.SOLAR, modifiedGenerator.getEnergySource());
        assertEquals(80.0, modifiedGenerator.getTargetP());
        assertEquals(40.0, modifiedGenerator.getTargetQ());
        assertEquals(48.0, modifiedGenerator.getTargetV());
        assertFalse(modifiedGenerator.isVoltageRegulatorOn());
        assertEquals(0., modifiedGenerator.getMinP());
        assertEquals(100., modifiedGenerator.getMaxP());
        assertEquals(220., modifiedGenerator.getRatedS());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorStartup.class).getMarginalCost());
        assertEquals(10., modifiedGenerator.getExtension(GeneratorStartup.class).getPlannedActivePowerSetpoint());
        assertEquals(0.30, modifiedGenerator.getExtension(GeneratorStartup.class).getPlannedOutageRate());
        assertEquals(0.40, modifiedGenerator.getExtension(GeneratorStartup.class).getForcedOutageRate());
        assertEquals(0.1f, modifiedGenerator.getExtension(ActivePowerControl.class).getDroop());
        assertTrue(modifiedGenerator.getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorShortCircuit.class).getDirectTransX());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorShortCircuit.class).getStepUpTransformerX());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedGenerator.getReactiveLimits().getKind());
        assertEquals(PROPERTY_VALUE, modifiedGenerator.getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        Generator generator = getNetwork().getGenerator("idGenerator");
        assertEquals("idGenerator", generator.getNameOrId());
        assertEquals(EnergySource.OTHER, generator.getEnergySource());
        assertEquals(42.1, generator.getTargetP());
        assertEquals(1.0, generator.getTargetQ());
        assertEquals(Double.NaN, generator.getTargetV());
        assertFalse(generator.isVoltageRegulatorOn());
        assertEquals(-1.1, generator.getMinP());
        assertEquals(1000.0, generator.getMaxP());
        assertEquals(Double.NaN, generator.getRatedS());
        assertEquals(ReactiveLimitsKind.MIN_MAX, generator.getReactiveLimits().getKind());
        assertNull(generator.getProperty(PROPERTY_NAME));
    }

    @Test
    void testMinMaxReactiveLimitsAttributesModification() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        //setting ReactiveCapabilityCurve to false with null min and max reactive limits
        generatorModificationInfos.setReactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET));
        generatorModificationInfos.setMaxQ(null);
        generatorModificationInfos.setMinQ(null);
        //setting ReactiveCapabilityCurvePoints for the generator we are modifying
        Generator generator = getNetwork().getGenerator("idGenerator");
        generator.newReactiveCapabilityCurve()
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
        String modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // Modifying only min reactive limit
        generatorModificationInfos.setMinQ(new AttributeModification<>(-200., OperationType.SET));
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);

        // Modifying only max reactive limit
        generatorModificationInfos.setMinQ(null);
        generatorModificationInfos.setMaxQ(new AttributeModification<>(200., OperationType.SET));
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 3);

        // Modifying both min and max reactive limits
        generatorModificationInfos.setMinQ(new AttributeModification<>(-1.1, OperationType.SET));
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 4);

        // nothing before reactive limits modification
        generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos.setEnergySource(null);
        generatorModificationInfos.setEquipmentName(null);
        generatorModificationInfos.setMinP(null);
        generatorModificationInfos.setMaxP(null);
        generatorModificationInfos.setRatedS(null);
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);
        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 5);
    }

    @Test
    void testGeneratorShortCircuitAttributesModification() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // setting transient reactance to null, modifying only step up transformer reactance
        generatorModificationInfos.setDirectTransX(null);
        String modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // setting step up transformer reactance to null, modifying only transient reactance
        generatorModificationInfos.setDirectTransX(new AttributeModification<>(1.1, OperationType.SET));
        generatorModificationInfos.setStepUpTransformerX(null);

        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);
    }

    @Test
    void testGeneratorVoltageRegulatorAttributesModification() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // setting voltageRegulatorOn to true, applying qPercent and regulatingTerminal
        generatorModificationInfos.setVoltageRegulationOn(new AttributeModification<>(true, OperationType.SET));
        String modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // setting voltageRegulatorOn to null, no modification on voltageRegulationOn
        generatorModificationInfos.setVoltageRegulationOn(null);
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);

        // setting voltageRegulationType to local, setting reginingTerminal to null
        generatorModificationInfos.setVoltageRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET));
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 3);

        // no modification in setpoints
        generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos.setTargetP(null);
        generatorModificationInfos.setTargetQ(null);
        generatorModificationInfos.setVoltageRegulationOn(null);
        generatorModificationInfos.setParticipate(null);

        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);
        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 4);
    }

    @Test
    void testCreateWithErrors() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        // Unset an attribute that should not be null
        generatorModificationInfos.setEnergySource(new AttributeModification<>(null, OperationType.UNSET));

        String generatorModificationInfosJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertEquals(EnergySource.OTHER, getNetwork().getGenerator("idGenerator").getEnergySource());
        assertLogMessage("Generator '" + "idGenerator" + "': energy source is not set",
                generatorModificationInfos.getErrorType().name(), reportService);
    }

    @Test
    void testDroopUnchanged() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        generatorModificationInfos.getDroop().setValue(18f);
        String modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);

        // setting droop to null, modifying only participate
        generatorModificationInfos.setDroop(null);
        modificationToCreateJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertEquals(18f, createdModification.getDroop().getValue());
    }

    @Test
    void testMinQGreaterThanMaxQ() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        Generator generator = getNetwork().getGenerator("idGenerator");
        generator.newReactiveCapabilityCurve()
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
        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
        List<ReactiveCapabilityCurve.Point> generatorPoints = new ArrayList<>(points);
        List<ReactiveCapabilityCurvePointsInfos> modificationPoints = generatorModificationInfos.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurve.Point oldPoint = generatorPoints.get(i);
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
        String modificationToCreateJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("MODIFY_GENERATOR_ERROR : Generator '" + "idGenerator" + "' : maximum reactive power " + maxQ.get() + " is expected to be greater than or equal to minimum reactive power " + minQ.get(),
                generatorModificationInfos.getErrorType().name(), reportService);
    }

    @Test
    void testActivePowerZeroOrBetweenMinAndMaxActivePower() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        Generator generator = getNetwork().getGenerator("idGenerator");
        generator.setTargetP(80.)
                .setMinP(10.)
                .setMaxP(150.);

        generatorModificationInfos.setTargetP(new AttributeModification<>(110.0, OperationType.SET));

        Double minActivePower = generatorModificationInfos.getMinP() != null ? generatorModificationInfos.getMinP().getValue() : generator.getMinP();
        Double maxActivePower = generatorModificationInfos.getMaxP() != null ? generatorModificationInfos.getMaxP().getValue() : generator.getMaxP();
        Double activePower = generatorModificationInfos.getTargetP() != null ? generatorModificationInfos.getTargetP().getValue() : generator.getTargetP();

        String modificationToCreateJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("MODIFY_GENERATOR_ERROR : Generator '" + "idGenerator" + "' : Active power " + activePower + " is expected to be equal to 0 or within the range of minimum active power and maximum active power: [" + minActivePower + ", " + maxActivePower + "]",
                generatorModificationInfos.getErrorType().name(), reportService);

    }

    @Test
    void testUnsetAttributes() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // Unset TargetV
        generatorModificationInfos.setTargetV(new AttributeModification<>(null, OperationType.UNSET));

        String generatorModificationInfosJson = getJsonBody(generatorModificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertEquals(Double.NaN, getNetwork().getGenerator("idGenerator").getTargetV());

        //Unset TargetQ (voltage regulation needs to be turned on and voltage setpoint to have a value)
        generatorModificationInfos.setVoltageRegulationOn(new AttributeModification<>(true, OperationType.SET));
        generatorModificationInfos.setTargetV(new AttributeModification<>(44.0, OperationType.SET));
        generatorModificationInfos.setTargetQ(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfosJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertEquals(Double.NaN, getNetwork().getGenerator("idGenerator").getTargetQ());

    }

    @Test
    void changeGeneratorOnBusBreakerWithoutBusBarSection() throws Exception {
        VoltageLevel v1 = createVoltageLevel(getNetwork().getSubstation("s1"), "v11", "v32", TopologyKind.BUS_BREAKER, 380.0);
        createBusBarSection(getNetwork().getVoltageLevel("v1"), "1.7", "1.7", 0);
        createBus(v1, "bus111", "bus111");
        createGeneratorOnBus(v1, "idGenerator1", "bus111", 42.1, 1.0);
        GeneratorModificationInfos generatorModificationInfos = GeneratorModificationInfos.builder()
                .stashed(false)
                .equipmentId("idGenerator1")
                .connectionPosition(new AttributeModification<>(1, OperationType.SET))
                .build();
        String generatorModificationInfosJson = getJsonBody(generatorModificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        generatorModificationInfos = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);
        assertEquals(1, generatorModificationInfos.getConnectionPosition().getValue());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGenerator", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("GENERATOR_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idGeneratorEdited", createdValues.get("equipmentId"));
    }

    @Test
    void testDisconnection() throws Exception {
        assertChangeConnectionState(getNetwork().getGenerator("idGenerator"), false);
    }

    @Test
    void testConnection() throws Exception {
        assertChangeConnectionState(getNetwork().getGenerator("idGenerator"), true);
    }
}
