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
import com.powsybl.iidm.network.extensions.HvdcAngleDroopActivePowerControl;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRange;
import com.powsybl.iidm.network.extensions.HvdcOperatorActivePowerRangeAdder;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Assert;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.springframework.util.CollectionUtils;

import java.io.IOException;
import java.util.*;
import java.util.stream.IntStream;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
@Tag("IntegrationTest")
public class VscModificationTest extends AbstractNetworkModificationTest {

    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

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

    private ConverterStationModificationInfos buildConverterStationWithReactiveCapabilityCurve() {
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
                        new ReactiveCapabilityCurveModificationInfos(0.4, 3., 11., 13., 0.7, 0.9),
                        new ReactiveCapabilityCurveModificationInfos(0.6, 2., 12., 14., 0.8, 0.11)))
                .build();
    }

    private ConverterStationModificationInfos buildEmptyConverterStation() {
        return ConverterStationModificationInfos.builder()
                .equipmentId("v1vsc")
                .stashed(false)
                .build();
    }

    private ConverterStationModificationInfos buildConverterStationWithMinMaxReactiveLimits() {
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
        Assert.assertEquals(6, hvdcOperatorActivePowerRange.getOprFromCS1toCS2(), 0);
        Assert.assertEquals(8, hvdcOperatorActivePowerRange.getOprFromCS2toCS1(), 0);

        HvdcAngleDroopActivePowerControl activePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        Assert.assertEquals(5, activePowerControl.getP0(), 0);
        Assert.assertEquals(1, activePowerControl.getDroop(), 0);
        Assert.assertTrue(activePowerControl.isEnabled());

        Assert.assertEquals(HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER, hvdcLine.getConvertersMode());

        Assert.assertEquals(1, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("v1vsc")).count());

        Assert.assertEquals(1, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("v2vsc")).count());

        VscModificationInfos vscModificationInfos = (VscModificationInfos) buildModification();

        {
            VscConverterStation vscConverterStation1 = (VscConverterStation) hvdcLine.getConverterStation1();
            assertNotNull(vscConverterStation1);
            Assert.assertEquals("v1vsc-name", vscConverterStation1.getOptionalName().orElse(""));
            Assert.assertEquals(0.2, vscConverterStation1.getReactivePowerSetpoint(), 0);
            Assert.assertEquals(0.1F, vscConverterStation1.getLossFactor(), 0);
            Assert.assertEquals(ReactiveLimitsKind.CURVE, vscConverterStation1.getReactiveLimits().getKind());
            ReactiveCapabilityCurve reactiveLimits1 = vscConverterStation1.getReactiveLimits(ReactiveCapabilityCurve.class);
            Assert.assertEquals(2, reactiveLimits1.getPointCount());
            Collection<ReactiveCapabilityCurve.Point> points = vscConverterStation1.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints();
            List<ReactiveCapabilityCurve.Point> vscPoints = new ArrayList<>(points);
            List<ReactiveCapabilityCurveModificationInfos> modificationPoints = vscModificationInfos.getConverterStation1().getReactiveCapabilityCurvePoints();
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
            Assert.assertEquals(0.3, vscConverterStation1.getVoltageSetpoint(), 0);
            Assert.assertEquals("v1", vscConverterStation1.getTerminal().getVoltageLevel().getId());
        }
        {
            VscConverterStation vscConverterStation2 = (VscConverterStation) hvdcLine.getConverterStation2();
            assertNotNull(vscConverterStation2);
            Assert.assertEquals("v2vsc-name", vscConverterStation2.getOptionalName().orElse(""));
            Assert.assertEquals(0.2, vscConverterStation2.getReactivePowerSetpoint(), 0);
            Assert.assertEquals(0.1F, vscConverterStation2.getLossFactor(), 0);
            Assert.assertEquals(ReactiveLimitsKind.MIN_MAX, vscConverterStation2.getReactiveLimits().getKind());
            MinMaxReactiveLimits reactiveLimits2 = vscConverterStation2.getReactiveLimits(MinMaxReactiveLimits.class);
            Assert.assertEquals(0.5, reactiveLimits2.getMaxQ(), 0);
            Assert.assertEquals(0.4, reactiveLimits2.getMinQ(), 0);
            Assert.assertEquals(0.3, vscConverterStation2.getVoltageSetpoint(), 0);
            Assert.assertEquals("v2", vscConverterStation2.getTerminal().getVoltageLevel().getId());
        }
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        String type = modificationInfos.getMessageType();
        Assert.assertEquals("VSC_MODIFICATION", type);
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() {
        });
        Assertions.assertEquals("hvdcLine", createdValues.get("equipmentId")); //TODO : implement equipement id change and change hvdcLine to vsc1 for example
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getHvdcLine("vsc1"));
        Assert.assertEquals(0, getNetwork().getVoltageLevel("v1").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId1")).count());

        Assert.assertEquals(0, getNetwork().getVoltageLevel("v2").getVscConverterStationStream()
                .filter(converterStation -> converterStation.getId().equals("stationId2")).count());
    }

    @Test
    public void testActivateHvdcAngleDroopActivePowerControl() throws Exception {
        var networkuuid = UUID.randomUUID();
        Network networkWitoutExt = NetworkCreation.createWithVSC(networkuuid, false);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setAngleDroopActivePowerControl(new AttributeModification<>(true, OperationType.SET));
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        vscModification.apply(networkWitoutExt, true, computationManager, subReporter);

        HvdcLine hvdcLine = networkWitoutExt.getHvdcLine("hvdcLine");
        assertNotNull(hvdcLine);

        HvdcAngleDroopActivePowerControl activePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        Assert.assertEquals(5, activePowerControl.getP0(), 0);
        Assert.assertEquals(1, activePowerControl.getDroop(), 0);
        Assert.assertTrue(activePowerControl.isEnabled());

    }

    @Test
    public void testUnchangedHVDCangleDroopActivePowerControl() throws Exception {
        var networkuuid = UUID.randomUUID();
        Network networkWitoutExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setConverterStation1(null);
        modificationInfos.setConverterStation2(null);
        modificationInfos.setAngleDroopActivePowerControl(null);
        modificationInfos.setDroop(null);
        modificationInfos.setP0(null);
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        vscModification.apply(networkWitoutExt, true, computationManager, subReporter);
        HvdcLine hvdcLine = networkWitoutExt.getHvdcLine("hvdcLine");
        HvdcAngleDroopActivePowerControl activePowerControl = hvdcLine.getExtension(HvdcAngleDroopActivePowerControl.class);
        Assert.assertEquals(0, activePowerControl.getP0(), 0);
        Assert.assertEquals(10, activePowerControl.getDroop(), 0);
        Assert.assertTrue(activePowerControl.isEnabled());
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("VSC_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vsc1Edited", updatedValues.get("equipmentId"));
    }

    @Test
    public void testIscheckIfChangeRequestedOnDropActiveControl() {
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setAngleDroopActivePowerControl(new AttributeModification<>(true, OperationType.SET));
        VscModification vscModification = new VscModification(modificationInfos);
        Assert.assertFalse(vscModification.checkIfChangeRequestedOnDropActiveControl());
    }

    @Test
    public void testDtoContainRequiredData() {
        VscModificationInfos modificationInfos = VscModificationInfos.builder()
                .stashed(false)
                .equipmentId("hvdcLine")
                .build();

        var networkuuid = UUID.randomUUID();
        Network networkWitoutExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModification vscModification = new VscModification(modificationInfos);
        Assert.assertThrows(NetworkModificationException.class, () -> vscModification.check(networkWitoutExt));
    }

    @Test
    public void testModifyOperatorActiveRange() throws IOException {
        VscModificationInfos modificationInfos = VscModificationInfos.builder()
                .stashed(false)
                .equipmentId("hvdcLine")
                .operatorActivePowerLimitFromSide2ToSide1(new AttributeModification<>(99.f, OperationType.SET))
                .operatorActivePowerLimitFromSide1ToSide2(new AttributeModification<>(100.f, OperationType.SET))
                .build();

        var networkuuid = UUID.randomUUID();
        Network networkWitoutExt = NetworkCreation.createWithVSC(networkuuid, true);
        var hvdcLine = networkWitoutExt.getHvdcLine("hvdcLine");
        hvdcLine.newExtension(HvdcOperatorActivePowerRangeAdder.class)
                .withOprFromCS1toCS2(10)
                .withOprFromCS2toCS1(12)
                .add();
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        VscModification vscModification = new VscModification(modificationInfos);
        vscModification.apply(networkWitoutExt, true, computationManager, subReporter);
        var hvdcOperatorActivePowerRange = hvdcLine.getExtension(HvdcOperatorActivePowerRange.class);
        Assert.assertEquals(100.f, hvdcOperatorActivePowerRange.getOprFromCS1toCS2(), 0.1);
        Assert.assertEquals(99.f, hvdcOperatorActivePowerRange.getOprFromCS2toCS1(), 0.1);
    }

    @Test
    public void testNoChangeOnConverterStation() throws IOException {
        var networkuuid = UUID.randomUUID();
        ConverterStationModificationInfos emptyConverterStation = buildEmptyConverterStation();
        Network networkWitoutExt = NetworkCreation.createWithVSC(networkuuid, true);
        VscModificationInfos modificationInfos = (VscModificationInfos) buildModification();
        modificationInfos.setConverterStation1(emptyConverterStation); // no change on converter station
        VscModification vscModification = new VscModification(modificationInfos);
        ReportNode subReporter = ReportNode.NO_OP;
        ComputationManager computationManager = new LocalComputationManager();
        vscModification.apply(networkWitoutExt, true, computationManager, subReporter);
        assertDoesNotThrow(() -> vscModification.apply(networkWitoutExt, true, computationManager, subReporter));
    }
}
