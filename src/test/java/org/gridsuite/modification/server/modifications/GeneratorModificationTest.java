/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuit;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.utils.assertions.Assertions.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class GeneratorModificationTest extends AbstractNetworkModificationTest {
    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return GeneratorModificationInfos.builder()
                .equipmentId("idGenerator")
                .energySource(new AttributeModification<>(EnergySource.SOLAR, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1Generator", OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(80.0, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(40.0, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(48.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(false, OperationType.SET))
                .minActivePower(new AttributeModification<>(0., OperationType.SET))
                .maxActivePower(new AttributeModification<>(100., OperationType.SET))
                .ratedNominalPower(new AttributeModification<>(220., OperationType.SET))
                .voltageRegulationType(
                        new AttributeModification<>(VoltageRegulationType.DISTANT, OperationType.SET))
                .plannedActivePowerSetPoint(new AttributeModification<>(10., OperationType.SET))
                .marginalCost(new AttributeModification<>(0.1, OperationType.SET))
                .plannedOutageRate(new AttributeModification<>(.30, OperationType.SET))
                .forcedOutageRate(new AttributeModification<>(.40, OperationType.SET))
                .minimumReactivePower(new AttributeModification<>(-100., OperationType.SET))
                .maximumReactivePower(new AttributeModification<>(100., OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of(
                        new ReactiveCapabilityCurveModificationInfos(0., 0., 100., 100., 0., 0.1),
                        new ReactiveCapabilityCurveModificationInfos(0., 0., 100., 100., 200., 150.)))
                .droop(new AttributeModification<>(0.1f, OperationType.SET))
                .participate(new AttributeModification<>(true, OperationType.SET))
                .transientReactance(new AttributeModification<>(0.1, OperationType.SET))
                .stepUpTransformerReactance(new AttributeModification<>(0.1, OperationType.SET))
                .regulatingTerminalId(new AttributeModification<>("v2load", OperationType.SET))
                .regulatingTerminalType(new AttributeModification<>("LOAD", OperationType.SET))
                .regulatingTerminalVlId(new AttributeModification<>("v1", OperationType.SET))
                .qPercent(new AttributeModification<>(0.1, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GeneratorModificationInfos.builder()
                .equipmentId("idGenerator")
                .energySource(new AttributeModification<>(EnergySource.HYDRO, OperationType.SET))
                .equipmentName(new AttributeModification<>("newV1GeneratorEdited", OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(81.0, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(41.0, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(49.0, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
                .minActivePower(new AttributeModification<>(1., OperationType.SET))
                .maxActivePower(new AttributeModification<>(102., OperationType.SET))
                .ratedNominalPower(new AttributeModification<>(221., OperationType.SET))
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
    protected void assertNetworkAfterCreation() {
        Generator modifiedGenerator = getNetwork().getGenerator("idGenerator");
        assertEquals("newV1Generator", modifiedGenerator.getNameOrId());
        assertEquals(EnergySource.SOLAR, modifiedGenerator.getEnergySource());
        assertEquals(80.0, modifiedGenerator.getTargetP());
        assertEquals(40.0, modifiedGenerator.getTargetQ());
        assertEquals(48.0, modifiedGenerator.getTargetV());
        assertEquals(false, modifiedGenerator.isVoltageRegulatorOn());
        assertEquals(0., modifiedGenerator.getMinP());
        assertEquals(100., modifiedGenerator.getMaxP());
        assertEquals(220., modifiedGenerator.getRatedS());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorStartup.class).getMarginalCost());
        assertEquals(10., modifiedGenerator.getExtension(GeneratorStartup.class).getPlannedActivePowerSetpoint());
        assertEquals(0.30, modifiedGenerator.getExtension(GeneratorStartup.class).getPlannedOutageRate());
        assertEquals(0.40, modifiedGenerator.getExtension(GeneratorStartup.class).getForcedOutageRate());
        assertEquals(0.1f, modifiedGenerator.getExtension(ActivePowerControl.class).getDroop());
        assertEquals(true, modifiedGenerator.getExtension(ActivePowerControl.class).isParticipate());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorShortCircuit.class).getDirectTransX());
        assertEquals(0.1, modifiedGenerator.getExtension(GeneratorShortCircuit.class).getStepUpTransformerX());
        assertEquals(ReactiveLimitsKind.CURVE, modifiedGenerator.getReactiveLimits().getKind());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        Generator generator = getNetwork().getGenerator("idGenerator");
        assertEquals("idGenerator", generator.getNameOrId());
        assertEquals(EnergySource.OTHER, generator.getEnergySource());
        assertEquals(42.1, generator.getTargetP());
        assertEquals(1.0, generator.getTargetQ());
        assertEquals(Double.NaN, generator.getTargetV());
        assertEquals(false, generator.isVoltageRegulatorOn());
        assertEquals(-1.1, generator.getMinP());
        assertEquals(1000.0, generator.getMaxP());
        assertEquals(Double.NaN, generator.getRatedS());
        assertEquals(ReactiveLimitsKind.MIN_MAX, generator.getReactiveLimits().getKind());
    }

    @Test
    public void testMinMaxReactiveLimitsAttributesModification() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        //setting ReactiveCapabilityCurve to false with null min and max reactive limits
        generatorModificationInfos.setReactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET));
        generatorModificationInfos.setMaximumReactivePower(null);
        generatorModificationInfos.setMinimumReactivePower(null);
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
        String modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // Modifying only min reactive limit
        generatorModificationInfos.setMinimumReactivePower(new AttributeModification<>(-200., OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);

        // Modifying only max reactive limit
        generatorModificationInfos.setMinimumReactivePower(null);
        generatorModificationInfos.setMaximumReactivePower(new AttributeModification<>(200., OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 3);

        // Modifying both min and max reactive limits
        generatorModificationInfos.setMinimumReactivePower(new AttributeModification<>(-1.1, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 4);

        // nothing before reactive limits modification
        generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos.setEnergySource(null);
        generatorModificationInfos.setEquipmentName(null);
        generatorModificationInfos.setMinActivePower(null);
        generatorModificationInfos.setMaxActivePower(null);
        generatorModificationInfos.setRatedNominalPower(null);
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(4);
        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 5);
    }

    @Test
    public void testGeneratorShortCircuitAttributesModification() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // setting transient reactance to null, modifying only step up transformer reactance
        generatorModificationInfos.setTransientReactance(null);
        String modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // setting step up transformer reactance to null, modifying only transient reactance
        generatorModificationInfos.setTransientReactance(new AttributeModification<>(1.1, OperationType.SET));
        generatorModificationInfos.setStepUpTransformerReactance(null);
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);
    }

    @Test
    public void testGeneratorVoltageRegulatorAttributesModification() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // setting voltageRegulatorOn to true, applying qPercent and regulatingTerminal
        generatorModificationInfos.setVoltageRegulationOn(new AttributeModification<>(true, OperationType.SET));
        String modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 1);

        // setting voltageRegulatorOn to null, no modification on voltageRegulationOn
        generatorModificationInfos.setVoltageRegulationOn(null);
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(1);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 2);

        // setting voltageRegulationType to local, setting reginingTerminal to null
        generatorModificationInfos.setVoltageRegulationType(new AttributeModification<>(VoltageRegulationType.LOCAL, OperationType.SET));
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(2);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 3);

        // no modification in setpoints
        generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        generatorModificationInfos.setActivePowerSetpoint(null);
        generatorModificationInfos.setReactivePowerSetpoint(null);
        generatorModificationInfos.setVoltageRegulationOn(null);
        generatorModificationInfos.setParticipate(null);

        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(3);
        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);
        testNetworkModificationsCount(getGroupId(), 4);
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();
        // Unset an attribute that should not be null
        generatorModificationInfos.setEnergySource(new AttributeModification<>(null, OperationType.UNSET));

        String generatorModificationInfosJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertEquals(EnergySource.OTHER, getNetwork().getGenerator("idGenerator").getEnergySource());
        assertLogMessage("Generator '" + "idGenerator" + "': energy source is not set",
                generatorModificationInfos.getErrorType().name(), reportService);
    }

    @Test
    public void testDroopUnchanged() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        generatorModificationInfos.getDroop().setValue(18f);
        String modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        GeneratorModificationInfos createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertThat(createdModification).recursivelyEquals(generatorModificationInfos);

        // setting droop to null, modifying only participate
        generatorModificationInfos.setDroop(null);
        modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        createdModification = (GeneratorModificationInfos) modificationRepository.getModifications(getGroupId(), false, true).get(0);

        assertEquals(18f, createdModification.getDroop().getValue());
    }

    @Test
    public void testMinQGreaterThanMaxQ() throws Exception {
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
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = generatorModificationInfos.getReactiveCapabilityCurvePoints();
        AtomicReference<Double> maxQ = new AtomicReference<>(Double.NaN);
        AtomicReference<Double> minQ = new AtomicReference<>(Double.NaN);
        if (!CollectionUtils.isEmpty(points)) {
            IntStream.range(0, modificationPoints.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurve.Point oldPoint = generatorPoints.get(i);
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
        String modificationToCreateJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        assertLogMessage("MODIFY_GENERATOR_ERROR : Generator '" + "idGenerator" + "' : maximum reactive power " + maxQ.get() + " is expected to be greater than or equal to minimum reactive power " + minQ.get(),
                generatorModificationInfos.getErrorType().name(), reportService);
    }

    @Test
    public void testUnsetAttributes() throws Exception {
        GeneratorModificationInfos generatorModificationInfos = (GeneratorModificationInfos) buildModification();

        // Unset TargetV
        generatorModificationInfos.setVoltageSetpoint(new AttributeModification<>(null, OperationType.UNSET));

        String generatorModificationInfosJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertEquals(Double.NaN, getNetwork().getGenerator("idGenerator").getTargetV());

        //Unset TargetQ (voltage regulation needs to be turned on and voltage setpoint to have a value)
        generatorModificationInfos.setVoltageRegulationOn(new AttributeModification<>(true, OperationType.SET));
        generatorModificationInfos.setVoltageSetpoint(new AttributeModification<>(44.0, OperationType.SET));
        generatorModificationInfos.setReactivePowerSetpoint(new AttributeModification<>(null, OperationType.UNSET));
        generatorModificationInfosJson = mapper.writeValueAsString(generatorModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(generatorModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertEquals(Double.NaN, getNetwork().getGenerator("idGenerator").getTargetQ());

    }
}
