/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.computation.ComputationManager;
import com.powsybl.computation.local.LocalComputationManager;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.VscModification;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.NetworkModificationException.Type.WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL;
import static org.gridsuite.modification.modifications.VscModification.ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG;
import static org.junit.jupiter.api.Assertions.*;
/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@Tag("IntegrationTest")
class VscModificationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";
    private static final Double CS_1_MEASUREMENT_P_VALUE = 400.0;
    private static final Boolean CS_1_MEASUREMENT_P_VALID = true;
    private static final Double CS_1_MEASUREMENT_Q_VALUE = 100.0;
    private static final Boolean CS_1_MEASUREMENT_Q_VALID = false;
    private static final Double CS_2_MEASUREMENT_P_VALUE = 225.0;
    private static final Boolean CS_2_MEASUREMENT_P_VALID = false;
    private static final Double CS_2_MEASUREMENT_Q_VALUE = 50.0;
    private static final Boolean CS_2_MEASUREMENT_Q_VALID = true;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createWithVSC(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return VscModificationInfos.builder()
                .stashed(false)
                .equipmentId("hvdcLine")
                .equipmentName(new AttributeModification<>("hvdcLine", OperationType.SET))
                .nominalV(new AttributeModification<>(39., OperationType.SET))
                .r(new AttributeModification<>(4., OperationType.SET))
                .maxP(new AttributeModification<>(56., OperationType.SET))
                .p0(new AttributeModification<>(5F, OperationType.SET))
                .operatorActivePowerLimitFromSide2ToSide1(new AttributeModification<>(5.6F, OperationType.SET))
                .convertersMode(new AttributeModification<>(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(5., OperationType.SET))
                .operatorActivePowerLimitFromSide1ToSide2(new AttributeModification<>(6.0F, OperationType.SET))
                .operatorActivePowerLimitFromSide2ToSide1(new AttributeModification<>(8F, OperationType.SET))
                .droop(new AttributeModification<>(1F, OperationType.SET))
                .angleDroopActivePowerControl(new AttributeModification<>(true, OperationType.SET))
                .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
                .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
                .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
                .build();
    }

    private static ConverterStationModificationInfos buildConverterStationWithReactiveCapabilityCurve() {
        return ConverterStationModificationInfos.builder()
                .equipmentId("v1vsc")
                .stashed(false)
                .equipmentName(new AttributeModification<>("v1vsc-name", OperationType.SET))
                .lossFactor(new AttributeModification<>(0.1F, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(0.2, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(0.3, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(true, OperationType.SET))
                .reactiveCapabilityCurvePoints(List.of(
                        new ReactiveCapabilityCurvePointsInfos(0.4, 11., 0.7),
                        new ReactiveCapabilityCurvePointsInfos(0.6, 12., 0.8)))
                .pMeasurementValue(new AttributeModification<>(CS_1_MEASUREMENT_P_VALUE, OperationType.SET))
                .pMeasurementValidity(new AttributeModification<>(CS_1_MEASUREMENT_P_VALID, OperationType.SET))
                .qMeasurementValue(new AttributeModification<>(CS_1_MEASUREMENT_Q_VALUE, OperationType.SET))
                .qMeasurementValidity(new AttributeModification<>(CS_1_MEASUREMENT_Q_VALID, OperationType.SET))
                .build();
    }

    private static ConverterStationModificationInfos buildEmptyConverterStation() {
        return ConverterStationModificationInfos.builder()
                .equipmentId("v1vsc")
                .stashed(false)
                .build();
    }

    private static ConverterStationModificationInfos buildConverterStationWithMinMaxReactiveLimits() {
        return ConverterStationModificationInfos.builder()
                .equipmentId("v2vsc")
                .stashed(false)
                .equipmentName(new AttributeModification<>("v2vsc-name", OperationType.SET))
                .lossFactor(new AttributeModification<>(0.1F, OperationType.SET))
                .reactivePowerSetpoint(new AttributeModification<>(0.2, OperationType.SET))
                .voltageRegulationOn(new AttributeModification<>(true, OperationType.SET))
                .voltageSetpoint(new AttributeModification<>(0.3, OperationType.SET))
                .minQ(new AttributeModification<>(0.4, OperationType.SET))
                .maxQ(new AttributeModification<>(0.5, OperationType.SET))
                .reactiveCapabilityCurve(new AttributeModification<>(false, OperationType.SET))
                .pMeasurementValue(new AttributeModification<>(CS_2_MEASUREMENT_P_VALUE, OperationType.SET))
                .pMeasurementValidity(new AttributeModification<>(CS_2_MEASUREMENT_P_VALID, OperationType.SET))
                .qMeasurementValue(new AttributeModification<>(CS_2_MEASUREMENT_Q_VALUE, OperationType.SET))
                .qMeasurementValidity(new AttributeModification<>(CS_2_MEASUREMENT_Q_VALID, OperationType.SET))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VscModificationInfos.builder()
                .stashed(false)
                .equipmentId("vsc1Edited")
                .equipmentName(new AttributeModification<>("newV1VscEdited", OperationType.SET))
                .nominalV(new AttributeModification<>(40., OperationType.SET))
                .r(new AttributeModification<>(5., OperationType.SET))
                .maxP(new AttributeModification<>(57., OperationType.SET))
                .p0(new AttributeModification<>(6F, OperationType.SET))
                .operatorActivePowerLimitFromSide2ToSide1(new AttributeModification<>(6.6F, OperationType.SET))
                .convertersMode(new AttributeModification<>(HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER, OperationType.SET))
                .activePowerSetpoint(new AttributeModification<>(6., OperationType.SET))
                .operatorActivePowerLimitFromSide1ToSide2(new AttributeModification<>(7.0F, OperationType.SET))
                .operatorActivePowerLimitFromSide2ToSide1(new AttributeModification<>(9F, OperationType.SET))
                .droop(new AttributeModification<>(2F, OperationType.SET))
                .angleDroopActivePowerControl(new AttributeModification<>(true, OperationType.SET))
                .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
                .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
                .build();

    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        HvdcLine hvdcLine = getNetwork().getHvdcLine("hvdcLine");
        assertNotNull(hvdcLine);
        assertEquals("hvdcLine", hvdcLine.getOptionalName().orElse(""));
        assertEquals(39., hvdcLine.getNominalV(), 0);
        assertEquals(4., hvdcLine.getR(), 0);
        assertEquals(5., hvdcLine.getActivePowerSetpoint(), 0);
        assertEquals(56., hvdcLine.getMaxP(), 0);
        assertEquals(PROPERTY_VALUE, hvdcLine.getProperty(PROPERTY_NAME));

        HvdcOperatorActivePowerRange hvdcOperatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
        assertEquals(6, hvdcOperatorActivePowerRange.getOprFromCS1toCS2(), 0);
        assertEquals(8, hvdcOperatorActivePowerRange.getOprFromCS2toCS1(), 0);

        HvdcAngleDroopActivePowerControl activePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertEquals(5, activePowerControl.getP0(), 0);
        assertEquals(1, activePowerControl.getDroop(), 0);
        assertTrue(activePowerControl.isEnabled());

        assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());

        assertEquals(1, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("v1vsc")).count());

        assertEquals(1, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("v2vsc")).count());

        VscModificationInfos vscModificationInfos = (VscModificationInfos) buildModification();

        {
            VscConverterStation vscConverterStation1 = (VscConverterStation) hvdcLine.getConverterStation1();
            assertNotNull(vscConverterStation1);
            assertEquals("v1vsc-name", vscConverterStation1.getOptionalName().orElse(""));
            assertEquals(0.2, vscConverterStation1.getReactivePowerSetpoint(), 0);
            assertEquals(0.1F, vscConverterStation1.getLossFactor(), 0);
            assertEquals(ReactiveLimitsKind.CURVE, vscConverterStation1.getReactiveLimits().getKind());
            ReactiveCapabilityCurve reactiveLimits1 = vscConverterStation1.getReactiveLimits(ReactiveCapabilityCurve.class);
            assertEquals(2, reactiveLimits1.getPointCount());
            Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation1.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
            List<ReactiveCapabilityCurve.Point> vscPoints = new ArrayList<>(points);
            List<ReactiveCapabilityCurvePointsInfos> modificationPoints = vscModificationInfos.getConverterStation1().getReactiveCapabilityCurvePoints();
            if (!CollectionUtils.isEmpty(points)) {
                IntStream.range(0, vscPoints.size())
                        .forEach(i -> {
                            var point = vscPoints.get(i);
                            var modificationPoint = modificationPoints.get(i);
                            assertEquals(modificationPoint.getMaxQ(), point.getMaxQ());
                            assertEquals(modificationPoint.getMinQ(), point.getMinQ());
                            assertEquals(modificationPoint.getP(), point.getP());
                        });
            }
            assertEquals(0.3, vscConverterStation1.getVoltageSetpoint(), 0);
            assertEquals("v1", vscConverterStation1.getTerminal().getVoltageLevel().getId());
            assertMeasurements(vscConverterStation1, Measurement.Type.ACTIVE_POWER, CS_1_MEASUREMENT_P_VALUE, CS_1_MEASUREMENT_P_VALID);
            assertMeasurements(vscConverterStation1, Measurement.Type.REACTIVE_POWER, CS_1_MEASUREMENT_Q_VALUE, CS_1_MEASUREMENT_Q_VALID);
        }
        {
            VscConverterStation vscConverterStation2 = (VscConverterStation) hvdcLine.getConverterStation2();
            assertNotNull(vscConverterStation2);
            assertEquals("v2vsc-name", vscConverterStation2.getOptionalName().orElse(""));
            assertEquals(0.2, vscConverterStation2.getReactivePowerSetpoint(), 0);
            assertEquals(0.1F, vscConverterStation2.getLossFactor(), 0);
            assertEquals(ReactiveLimitsKind.MIN_MAX, vscConverterStation2.getReactiveLimits().getKind());
            MinMaxReactiveLimits reactiveLimits2 = vscConverterStation2.getReactiveLimits(MinMaxReactiveLimits.class);
            assertEquals(0.5, reactiveLimits2.getMaxQ(), 0);
            assertEquals(0.4, reactiveLimits2.getMinQ(), 0);
            assertEquals(0.3, vscConverterStation2.getVoltageSetpoint(), 0);
            assertEquals("v2", vscConverterStation2.getTerminal().getVoltageLevel().getId());
            assertMeasurements(vscConverterStation2, Measurement.Type.ACTIVE_POWER, CS_2_MEASUREMENT_P_VALUE, CS_2_MEASUREMENT_P_VALID);
            assertMeasurements(vscConverterStation2, Measurement.Type.REACTIVE_POWER, CS_2_MEASUREMENT_Q_VALUE, CS_2_MEASUREMENT_Q_VALID);
        }
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        String type = modificationInfos.getMessageType();
        assertEquals("VSC_MODIFICATION", type);
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("hvdcLine", createdValues.get("equipmentId")); //TODO : implement equipment id change and change hvdcLine to vsc1 for example
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getHvdcLine("vsc1"));
        assertEquals(0, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId1")).count());

        assertEquals(0, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId2")).count());
    }

    @Test
    void testCreateAngleDroopActivePowerControlWithEnabling() throws Exception {
        var networkuuid = UUID.randomUUID();
        Network networkWithoutExt = NetworkCreation.createWithVSC(networkuuid, false);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setAngleDroopActivePowerControl(new AttributeModification<>(true, OperationType.SET));
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        assertDoesNotThrow(() -> vscModification.check(networkWithoutExt));
        vscModification.apply(networkWithoutExt, true, computationManager, subReporter);

        HvdcLine hvdcLine = networkWithoutExt.getHvdcLine("hvdcLine");
        assertThat(hvdcLine).isNotNull();

        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertThat(activePowerControlExt).isNotNull();
        assertEquals(5, activePowerControlExt.getP0(), 0);
        assertEquals(1, activePowerControlExt.getDroop(), 0);
        assertThat(activePowerControlExt.isEnabled()).isTrue();
    }

    @Test
    void testAngleDroopActivePowerControlWithAbsentInfos() {
        var networkuuid = UUID.randomUUID();
        Network networkWithoutExt = NetworkCreation.createWithVSC(networkuuid, false);

        boolean[][] droopInfosIsPresentData = {
                {true, false, false},
                {true, true, false},
                {true, false, true},
                {false, true, false},
                {false, true, true},
                {false, false, true},
        };

        for (boolean[] droopInfoIsPresent : droopInfosIsPresentData) {
            VscModificationInfos modificationInfos = buildModificationWithDroopAbsentInfos(droopInfoIsPresent[0], droopInfoIsPresent[1], droopInfoIsPresent[2]);
            checkDroopWithAbsentInfos(modificationInfos, networkWithoutExt);
        }
    }

    private VscModificationInfos buildModificationWithDroopAbsentInfos(boolean isPresentAngleDroopActivePowerControl, boolean isPresentDroop, boolean isPresentP0) {
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        // reset null depending to test arguments
        if (!isPresentAngleDroopActivePowerControl) {
            modificationInfos.setAngleDroopActivePowerControl(null);
        }
        if (!isPresentDroop) {
            modificationInfos.setDroop(null);
        }
        if (!isPresentP0) {
            modificationInfos.setP0(null);
        }
        return modificationInfos;
    }

    private static void checkDroopWithAbsentInfos(VscModificationInfos modificationInfos, Network networkWithoutExt) {
        VscModification vscModification = new VscModification(modificationInfos);
        String message = assertThrows(NetworkModificationException.class,
                () -> vscModification.check(networkWithoutExt))
            .getMessage();
        assertThat(message).isEqualTo(WRONG_HVDC_ANGLE_DROOP_ACTIVE_POWER_CONTROL.name() + " : "
              + ACTIVE_POWER_CONTROL_DROOP_P0_REQUIRED_ERROR_MSG);
    }

    @Test
    void testNotCreateAngleDroopActivePowerControl() throws Exception {
        var networkuuid = UUID.randomUUID();
        Network networkWithExt = NetworkCreation.createWithVSC(networkuuid, false);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setAngleDroopActivePowerControl(null);
        modificationInfos.setDroop(null);
        modificationInfos.setP0(null);
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        assertDoesNotThrow(() -> vscModification.check(networkWithExt));
        vscModification.apply(networkWithExt, true, computationManager, subReporter);
        HvdcLine hvdcLine = networkWithExt.getHvdcLine("hvdcLine");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertThat(activePowerControlExt).isNull();
    }

    @Test
    void testNotChangeAngleDroopActivePowerControl() throws Exception {
        var networkuuid = UUID.randomUUID();
        Network networkWithExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setAngleDroopActivePowerControl(null);
        modificationInfos.setDroop(null);
        modificationInfos.setP0(null);
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        assertDoesNotThrow(() -> vscModification.check(networkWithExt));
        vscModification.apply(networkWithExt, true, computationManager, subReporter);
        HvdcLine hvdcLine = networkWithExt.getHvdcLine("hvdcLine");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertEquals(10, activePowerControlExt.getDroop(), 0);
        assertEquals(0, activePowerControlExt.getP0(), 0);
        assertThat(activePowerControlExt.isEnabled()).isTrue();
    }

    @Test
    void testChangeAngleDroopActivePowerControl() throws Exception {
        var networkuuid = UUID.randomUUID();
        Network networkWithExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setAngleDroopActivePowerControl(new AttributeModification<>(false, OperationType.SET));
        modificationInfos.setDroop(new AttributeModification<>(2.F, OperationType.SET));
        modificationInfos.setP0(new AttributeModification<>(6F, OperationType.SET));
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        assertDoesNotThrow(() -> vscModification.check(networkWithExt));
        vscModification.apply(networkWithExt, true, computationManager, subReporter);
        HvdcLine hvdcLine = networkWithExt.getHvdcLine("hvdcLine");
        assertThat(hvdcLine).isNotNull();
        HvdcAngleDroopActivePowerControl activePowerControlExt = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        assertEquals(2, activePowerControlExt.getDroop(), 0);
        assertEquals(6, activePowerControlExt.getP0(), 0);
        assertThat(activePowerControlExt.isEnabled()).isFalse();
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VSC_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vsc1Edited", updatedValues.get("equipmentId"));
    }

    @Test
    void testDtoContainRequiredData() {
        VscModificationInfos modificationInfos = VscModificationInfos.builder()
                .stashed(false)
                .equipmentId("hvdcLine")
                .build();

        var networkuuid = UUID.randomUUID();
        Network networkWitoutExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModification vscModification = new VscModification(modificationInfos);
        assertThrows(NetworkModificationException.class, () -> vscModification.check(networkWitoutExt));
    }

    @Test
    void testModifyOperatorActiveRange() throws Exception {
        VscModificationInfos modificationInfos = VscModificationInfos.builder()
                .stashed(false)
                .equipmentId("hvdcLine")
                .converterStation1(buildConverterStationWithReactiveCapabilityCurve())
                .converterStation2(buildConverterStationWithMinMaxReactiveLimits())
                .operatorActivePowerLimitFromSide2ToSide1(new AttributeModification<>(99.f, OperationType.SET))
                .operatorActivePowerLimitFromSide1ToSide2(new AttributeModification<>(100.f, OperationType.SET))
                .build();

        var networkuuid = UUID.randomUUID();
        Network networkWithExt = NetworkCreation.createWithVSC(networkuuid, true);
        var hvdcLine = networkWithExt.getHvdcLine("hvdcLine");
        hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                .withOprFromCS1toCS2(10)
                .withOprFromCS2toCS1(12)
                .add();
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        VscModification vscModification = new VscModification(modificationInfos);
        assertDoesNotThrow(() -> vscModification.check(networkWithExt));
        vscModification.apply(networkWithExt, true, computationManager, subReporter);
        var hvdcOperatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
        assertEquals(100.f, hvdcOperatorActivePowerRange.getOprFromCS1toCS2(), 0.1);
        assertEquals(99.f, hvdcOperatorActivePowerRange.getOprFromCS2toCS1(), 0.1);
    }

    @Test
    void testNoChangeOnConverterStation() throws Exception {
        var networkuuid = UUID.randomUUID();
        ConverterStationModificationInfos emptyConverterStation = buildEmptyConverterStation();
        Network networkWithExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setConverterStation1(emptyConverterStation); // no change on converter station
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        assertDoesNotThrow(() -> vscModification.check(networkWithExt));
        vscModification.apply(networkWithExt, true, computationManager, subReporter);
        assertDoesNotThrow(() -> vscModification.apply(networkWithExt, true, computationManager, subReporter));
    }

    @Test
    void testMeasurementsUpdatedWithNewMeasurements() {
        Network network = getNetwork();
        Double newCs1PMeasurementValue = 115.0D;
        Boolean newCs1PMeasurementValidity = false;
        Double newCs1QMeasurementValue = 25.0D;
        Boolean newCs1QMeasurementValidity = true;
        Double newCs2PMeasurementValue = 75.0D;
        Boolean newCs2PMeasurementValidity = true;
        Double newCs2QMeasurementValue = 15.0D;
        Boolean newCs2QMeasurementValidity = false;
        buildModification().toModification().apply(network);
        HvdcLine hvdcLine = getNetwork().getHvdcLine("hvdcLine");
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation1(), Measurement.Type.ACTIVE_POWER, CS_1_MEASUREMENT_P_VALUE, CS_1_MEASUREMENT_P_VALID);
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation1(), Measurement.Type.REACTIVE_POWER, CS_1_MEASUREMENT_Q_VALUE, CS_1_MEASUREMENT_Q_VALID);
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation2(), Measurement.Type.ACTIVE_POWER, CS_2_MEASUREMENT_P_VALUE, CS_2_MEASUREMENT_P_VALID);
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation2(), Measurement.Type.REACTIVE_POWER, CS_2_MEASUREMENT_Q_VALUE, CS_2_MEASUREMENT_Q_VALID);

        ConverterStationModificationInfos vsc1 = ConverterStationModificationInfos.builder()
                .equipmentId("v1vsc")
                .pMeasurementValue(new AttributeModification<>(newCs1PMeasurementValue, OperationType.SET))
                .pMeasurementValidity(new AttributeModification<>(newCs1PMeasurementValidity, OperationType.SET))
                .qMeasurementValue(new AttributeModification<>(newCs1QMeasurementValue, OperationType.SET))
                .qMeasurementValidity(new AttributeModification<>(newCs1QMeasurementValidity, OperationType.SET))
                .build();
        ConverterStationModificationInfos vsc2 = ConverterStationModificationInfos.builder()
                .equipmentId("v2vsc")
                .pMeasurementValue(new AttributeModification<>(newCs2PMeasurementValue, OperationType.SET))
                .pMeasurementValidity(new AttributeModification<>(newCs2PMeasurementValidity, OperationType.SET))
                .qMeasurementValue(new AttributeModification<>(newCs2QMeasurementValue, OperationType.SET))
                .qMeasurementValidity(new AttributeModification<>(newCs2QMeasurementValidity, OperationType.SET))
                .build();

        VscModificationInfos vscModificationInfos = VscModificationInfos.builder()
                .equipmentId("hvdcLine")
                .converterStation1(vsc1)
                .converterStation2(vsc2)
                .build();
        ReportNode rootNode = ReportNode.newRootReportNode()
                .withMessageTemplate("test")
                .build();
        vscModificationInfos.toModification().apply(network, rootNode);

        hvdcLine = getNetwork().getHvdcLine("hvdcLine");
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation1(), Measurement.Type.ACTIVE_POWER, newCs1PMeasurementValue, newCs1PMeasurementValidity);
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation1(), Measurement.Type.REACTIVE_POWER, newCs1QMeasurementValue, newCs1QMeasurementValidity);
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation2(), Measurement.Type.ACTIVE_POWER, newCs2PMeasurementValue, newCs2PMeasurementValidity);
        assertMeasurements((VscConverterStation) hvdcLine.getConverterStation2(), Measurement.Type.REACTIVE_POWER, newCs2QMeasurementValue, newCs2QMeasurementValidity);
        assertMeasurementsReportNodes(rootNode, vsc1.getEquipmentId(), CS_1_MEASUREMENT_P_VALUE, newCs1PMeasurementValue, CS_1_MEASUREMENT_P_VALID, newCs1PMeasurementValidity,
                CS_1_MEASUREMENT_Q_VALUE, newCs1QMeasurementValue, CS_1_MEASUREMENT_Q_VALID, newCs1QMeasurementValidity);
        assertMeasurementsReportNodes(rootNode, vsc2.getEquipmentId(), CS_2_MEASUREMENT_P_VALUE, newCs2PMeasurementValue, CS_2_MEASUREMENT_P_VALID, newCs2PMeasurementValidity,
                CS_2_MEASUREMENT_Q_VALUE, newCs2QMeasurementValue, CS_2_MEASUREMENT_Q_VALID, newCs2QMeasurementValidity);
    }

    private void assertMeasurements(VscConverterStation vcs, Measurement.Type type, double expectedValue, boolean expectedValid) {
        assertNotNull(vcs);
        Measurements<?> measurements = (Measurements<?>) vcs.getExtension(Measurements.class);
        assertNotNull(measurements);
        Collection<Measurement> voltageMeasurements = measurements.getMeasurements(type).stream().toList();
        assertThat(voltageMeasurements).isNotEmpty().allSatisfy(m -> {
            assertThat(m.getValue()).isEqualTo(expectedValue);
            assertThat(m.isValid()).isEqualTo(expectedValid);
        });
    }

    private void assertMeasurementsReportNodes(ReportNode rootNode, String vscId, Double measurementPValue, Double newPMeasurementValue, Boolean measurementPValid, Boolean newPMeasurementValidity,
                                               Double measurementQValue, Double newQMeasurementValue, Boolean measurementQValid, Boolean newQMeasurementValidity) {
        Optional<ReportNode> vscConverterStationNode = rootNode.getChildren().stream()
                .filter(node -> node.getMessageKey().equals("network.modification.vscConverterStationModified"))
                .filter(node -> vscId.equals(node.getValues().get("id").toString()))
                .findFirst();
        assertThat(vscConverterStationNode).isPresent();
        Optional<ReportNode> stateEstimationDataNode = vscConverterStationNode.get().getChildren().stream()
                .filter(node -> node.getMessageKey().equals("network.modification.stateEstimationData"))
                .findFirst();
        assertThat(stateEstimationDataNode).isPresent();
        Optional<ReportNode> expectedMeasurementsNodeOpt = stateEstimationDataNode.get().getChildren().stream()
                .filter(node -> node.getMessageKey().equals("network.modification.measurements"))
                .findFirst();
        assertThat(expectedMeasurementsNodeOpt).isPresent();
        ReportNode expectedMeasurementsNode = expectedMeasurementsNodeOpt.get();
        assertThat(expectedMeasurementsNode.getChildren()).hasSize(4);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementPValue, newPMeasurementValue);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementQValue, newQMeasurementValue);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementPValid, newPMeasurementValidity);
        assertMeasurementReportNode(expectedMeasurementsNode, measurementQValid, newQMeasurementValidity);
    }

    private void assertMeasurementReportNode(ReportNode rootNode, Object expectedOldValue, Object expectedNewValue) {
        assertThat(rootNode.getChildren()).anySatisfy(node -> {
            assertThat(node.getMessageKey()).isEqualTo("network.modification.fieldModification");
            assertThat(node.getValues().get("oldValue")).hasToString(expectedOldValue.toString());
            assertThat(node.getValues().get("newValue")).hasToString(expectedNewValue.toString());
        });
    }
}
