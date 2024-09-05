/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.simple;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.GeneratorField;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.simple.BooleanModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.simple.DoubleModificationByFilterInfos;
import org.junit.Test;
import org.junit.jupiter.api.Tag;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Tag("IntegrationTest")
public class GeneratorBySimpleModificationTest extends AbstractBySimpleModificationTest {
    private static final String GENERATOR_ID_1 = "idGenerator";
    private static final String GENERATOR_ID_2 = "v5generator";
    private static final String GENERATOR_ID_3 = "v6generator";
    private static final String GENERATOR_ID_4 = "gen4";
    private static final String GENERATOR_ID_5 = "gen5";
    private static final String GENERATOR_ID_6 = "gen6";
    private static final String GENERATOR_ID_7 = "gen7";
    private static final String GENERATOR_ID_8 = "gen8";
    private static final String GENERATOR_ID_9 = "gen9";
    private static final String GENERATOR_ID_10 = "gen10";

    @Test
    public void testCreateWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(GENERATOR_ID_1, 1.0);
        IdentifierListFilterEquipmentAttributes wrongIdAttributes = getIdentifiableAttributes("wrongId", 1.0);

        DoubleModificationByFilterInfos simpleInfos = DoubleModificationByFilterInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .value(55.)
                .build();

        checkCreateWithWarning(List.of(simpleInfos), List.of(identifiableAttributes, wrongIdAttributes));
        assertEquals(55, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0);
    }

    @Test
    public void testWithNullValue() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(GENERATOR_ID_1, 1.0);
        AbstractFilter filterEquipments = getFilterEquipments(FILTER_ID_1, List.of(identifiableAttributes));

        DoubleModificationByFilterInfos simpleInfos = DoubleModificationByFilterInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "filter1")))
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .value(null)
                .build();

        checkCreateWithError(List.of(simpleInfos), List.of(filterEquipments));
    }

    protected void createEquipments() {
        getNetwork().getGenerator(GENERATOR_ID_1)
                .setTargetP(100)
                .setMaxP(500)
                .setMinP(0)
                .setTargetV(10)
                .setTargetQ(20)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(25.)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(55.)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_2)
                .setTargetP(200)
                .setMaxP(2000)
                .setMinP(50)
                .setTargetV(10)
                .setTargetQ(20)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(25.)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(55.)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_3)
                .setTargetP(300)
                .setMaxP(2000)
                .setMinP(70)
                .newExtension(GeneratorShortCircuitAdder.class)
                .withDirectTransX(40.)
                .withStepUpTransformerX(38.)
                .add();

        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_4, 3, 400, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP, 700, 110);
        getNetwork().getGenerator(GENERATOR_ID_4)
                        .newExtension(GeneratorShortCircuitAdder.class)
                        .withDirectTransX(46.)
                        .withStepUpTransformerX(50.)
                        .add();

        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP, 2000, 50);
        getNetwork().getGenerator(GENERATOR_ID_5).newExtension(ActivePowerControlAdder.class).withDroop(2).add();

        createGenerator(getNetwork().getVoltageLevel("v2"), GENERATOR_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_6).newExtension(ActivePowerControlAdder.class).withDroop(3).add();

        createGenerator(getNetwork().getVoltageLevel("v6"), GENERATOR_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP, 2000, 50);
        getNetwork().getGenerator(GENERATOR_ID_7).newExtension(CoordinatedReactiveControlAdder.class)
                        .withQPercent(6)
                        .add();
        getNetwork().getGenerator(GENERATOR_ID_7).newExtension(GeneratorStartupAdder.class).withMarginalCost(50).add();

        createGenerator(getNetwork().getVoltageLevel("v3"), GENERATOR_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_8).newExtension(CoordinatedReactiveControlAdder.class)
                .withQPercent(12)
                .add();
        getNetwork().getGenerator(GENERATOR_ID_8).newExtension(GeneratorStartupAdder.class).withMarginalCost(60).add();

        createGenerator(getNetwork().getVoltageLevel("v4"), GENERATOR_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP, 2000, 50);
        getNetwork().getGenerator(GENERATOR_ID_9).setRatedS(60.);

        createGenerator(getNetwork().getVoltageLevel("v5"), GENERATOR_ID_10, 10, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP, 500, 20);
        getNetwork().getGenerator(GENERATOR_ID_10).setRatedS(30.);
    }

    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_4, 5.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_5, 6.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_7, 3.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_8, 8.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_9, 0.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_10, 9.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected List<AbstractSimpleModificationByFilterInfos<?>> getSimpleModificationInfos() {
        DoubleModificationByFilterInfos simpleInfos1 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.REACTIVE_POWER_SET_POINT.name())
                .value(50.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleModificationByFilterInfos simpleInfos2 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.DROOP.name())
                .value(2.)
                .filters(List.of(filter3))
                .build();

        DoubleModificationByFilterInfos simpleInfos3 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.RATED_NOMINAL_POWER.name())
                .value(2.)
                .filters(List.of(filter5))
                .build();

        DoubleModificationByFilterInfos simpleInfos4 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.MARGINAL_COST.name())
                .value(2.)
                .filters(List.of(filter1))
                .build();

        DoubleModificationByFilterInfos simpleInfos5 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.VOLTAGE_SET_POINT.name())
                .value(2.)
                .filters(List.of(filter4))
                .build();

        DoubleModificationByFilterInfos simpleInfos6 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name())
                .value(10.)
                .filters(List.of(filter1))
                .build();

        DoubleModificationByFilterInfos simpleInfos7 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.MINIMUM_ACTIVE_POWER.name())
                .value(2.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleModificationByFilterInfos simpleInfos8 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.PLANNED_OUTAGE_RATE.name())
                .value(0.1)
                .filters(List.of(filter1))
                .build();

        DoubleModificationByFilterInfos simpleInfos9 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.FORCED_OUTAGE_RATE.name())
                .value(0.05)
                .filters(List.of(filter1))
                .build();

        DoubleModificationByFilterInfos simpleInfos10 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.MAXIMUM_ACTIVE_POWER.name())
                .value(50.)
                .filters(List.of(filter1, filter2, filter3, filter4, filter5))
                .build();

        DoubleModificationByFilterInfos simpleInfos11 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.TRANSIENT_REACTANCE.name())
                .value(0.2)
                .filters(List.of(filter2))
                .build();

        DoubleModificationByFilterInfos simpleInfos12 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name())
                .value(0.3)
                .filters(List.of(filter2))
                .build();

        DoubleModificationByFilterInfos simpleInfos13 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.Q_PERCENT.name())
                .value(0.25)
                .filters(List.of(filter4))
                .build();

        BooleanModificationByFilterInfos simpleInfos14 = BooleanModificationByFilterInfos.builder()
                .editedField(GeneratorField.VOLTAGE_REGULATOR_ON.name())
                .value(true)
                .filters(List.of(filter1))
                .build();

        return List.of(simpleInfos1,
                simpleInfos2,
                simpleInfos3,
                simpleInfos4,
                simpleInfos5,
                simpleInfos6,
                simpleInfos7,
                simpleInfos8,
                simpleInfos9,
                simpleInfos10,
                simpleInfos11,
                simpleInfos12,
                simpleInfos13,
                simpleInfos14
        );
    }

    @Override
    protected List<AbstractSimpleModificationByFilterInfos<?>> getUpdatedSimpleModificationInfos() {
        DoubleModificationByFilterInfos simpleInfos1 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.REACTIVE_POWER_SET_POINT.name())
                .value(2.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleModificationByFilterInfos simpleInfos2 = DoubleModificationByFilterInfos.builder()
                .editedField(GeneratorField.MINIMUM_ACTIVE_POWER.name())
                .value(0.5)
                .filters(List.of(filter3))
                .build();

        return List.of(simpleInfos1, simpleInfos2);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        Generator generator1 = getNetwork().getGenerator(GENERATOR_ID_1);
        GeneratorStartup generatorStartup1 = generator1.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup1);
        assertEquals(100, generator1.getTargetP(), 0);
        assertEquals(2, generatorStartup1.getMarginalCost(), 0);
        assertEquals(55, generatorStartup1.getPlannedOutageRate(), 0);
        assertEquals(0.05, generatorStartup1.getForcedOutageRate(), 0);
        assertEquals(10, generatorStartup1.getPlannedActivePowerSetpoint(), 0);
        assertEquals(50, generator1.getMaxP(), 0);
        assertEquals(2, generator1.getMinP(), 0);
        assertEquals(true, generator1.isVoltageRegulatorOn());

        Generator generator2 = getNetwork().getGenerator(GENERATOR_ID_2);
        GeneratorStartup generatorStartup2 = generator2.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup2);
        assertEquals(200, generator2.getTargetP(), 0);
        assertEquals(2, generatorStartup2.getMarginalCost(), 0);
        assertEquals(55, generatorStartup2.getPlannedOutageRate(), 0);
        assertEquals(0.05, generatorStartup2.getForcedOutageRate(), 0);
        assertEquals(10, generatorStartup2.getPlannedActivePowerSetpoint(), 0);
        assertEquals(50, generator2.getMaxP(), 0);
        assertEquals(2, generator2.getMinP(), 0);

        Generator generator3 = getNetwork().getGenerator(GENERATOR_ID_3);
        GeneratorShortCircuit generatorShortCircuit3 = generator3.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit3);
        assertEquals(300, generator3.getTargetP(), 0);
        assertEquals(0.2, generatorShortCircuit3.getDirectTransX(), 0);
        assertEquals(0.3, generatorShortCircuit3.getStepUpTransformerX(), 0);
        assertEquals(50, generator3.getMaxP(), 0);
        assertEquals(2, generator3.getMinP(), 0);

        Generator generator4 = getNetwork().getGenerator(GENERATOR_ID_4);
        GeneratorShortCircuit generatorShortCircuit4 = generator4.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit4);
        assertEquals(0.2, generatorShortCircuit4.getDirectTransX(), 0);
        assertEquals(0.3, generatorShortCircuit4.getStepUpTransformerX(), 0);
        assertEquals(400, generator4.getTargetP(), 0);
        assertEquals(50, generator4.getMaxP(), 0);
        assertEquals(2, generator4.getMinP(), 0);

        Generator generator5 = getNetwork().getGenerator(GENERATOR_ID_5);
        ActivePowerControl activePowerControl5 = generator5.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(50, generator5.getMaxP(), 0);
        assertEquals(2, activePowerControl5.getDroop(), 0);

        Generator generator6 = getNetwork().getGenerator(GENERATOR_ID_6);
        ActivePowerControl activePowerControl6 = generator6.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl6);
        assertEquals(50, generator6.getMaxP(), 0);
        assertEquals(2, activePowerControl6.getDroop(), 0);

        Generator generator7 = getNetwork().getGenerator(GENERATOR_ID_7);
        CoordinatedReactiveControl coordinatedReactiveControl7 = generator7.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl7);
        GeneratorStartup generatorStartup7 = generator7.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup7);
        assertEquals(50, generatorStartup7.getMarginalCost(), 0);
        assertEquals(0.25, coordinatedReactiveControl7.getQPercent(), 0);

        Generator generator8 = getNetwork().getGenerator(GENERATOR_ID_8);
        CoordinatedReactiveControl coordinatedReactiveControl8 = generator8.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl8);
        GeneratorStartup generatorStartup8 = generator8.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup8);
        assertEquals(60, generatorStartup8.getMarginalCost(), 0);
        assertEquals(0.25, coordinatedReactiveControl8.getQPercent(), 0);

        assertEquals(2, getNetwork().getGenerator(GENERATOR_ID_9).getRatedS(), 0);
        assertEquals(2, getNetwork().getGenerator(GENERATOR_ID_10).getRatedS(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        Generator generator1 = getNetwork().getGenerator(GENERATOR_ID_1);
        GeneratorStartup generatorStartup1 = generator1.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup1);
        assertEquals(100, generator1.getTargetP(), 0);
        assertEquals(30, generatorStartup1.getMarginalCost(), 0);
        assertEquals(25, generatorStartup1.getPlannedOutageRate(), 0);
        assertEquals(55, generatorStartup1.getForcedOutageRate(), 0);
        assertEquals(40, generatorStartup1.getPlannedActivePowerSetpoint(), 0);
        assertEquals(500, generator1.getMaxP(), 0);
        assertEquals(0, generator1.getMinP(), 0);

        Generator generator2 = getNetwork().getGenerator(GENERATOR_ID_2);
        GeneratorStartup generatorStartup2 = generator2.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup2);
        assertEquals(200, generator2.getTargetP(), 0);
        assertEquals(30, generatorStartup2.getMarginalCost(), 0);
        assertEquals(25, generatorStartup2.getPlannedOutageRate(), 0);
        assertEquals(55, generatorStartup2.getForcedOutageRate(), 0);
        assertEquals(40, generatorStartup2.getPlannedActivePowerSetpoint(), 0);
        assertEquals(2000, generator2.getMaxP(), 0);
        assertEquals(50, generator2.getMinP(), 0);

        Generator generator3 = getNetwork().getGenerator(GENERATOR_ID_3);
        GeneratorShortCircuit generatorShortCircuit3 = generator3.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit3);
        assertEquals(300, generator3.getTargetP(), 0);
        assertEquals(40, generatorShortCircuit3.getDirectTransX(), 0);
        assertEquals(38, generatorShortCircuit3.getStepUpTransformerX(), 0);
        assertEquals(2000, generator3.getMaxP(), 0);
        assertEquals(70, generator3.getMinP(), 0);

        Generator generator4 = getNetwork().getGenerator(GENERATOR_ID_4);
        GeneratorShortCircuit generatorShortCircuit4 = generator4.getExtension(GeneratorShortCircuit.class);
        assertNotNull(generatorShortCircuit4);
        assertEquals(46, generatorShortCircuit4.getDirectTransX(), 0);
        assertEquals(50, generatorShortCircuit4.getStepUpTransformerX(), 0);
        assertEquals(400, generator4.getTargetP(), 0);
        assertEquals(700, generator4.getMaxP(), 0);
        assertEquals(110, generator4.getMinP(), 0);

        Generator generator5 = getNetwork().getGenerator(GENERATOR_ID_5);
        ActivePowerControl activePowerControl5 = generator5.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(2000, generator5.getMaxP(), 0);
        assertEquals(2, activePowerControl5.getDroop(), 0);

        Generator generator6 = getNetwork().getGenerator(GENERATOR_ID_6);
        ActivePowerControl activePowerControl6 = generator6.getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl6);
        assertEquals(500, generator6.getMaxP(), 0);
        assertEquals(3, activePowerControl6.getDroop(), 0);

        Generator generator7 = getNetwork().getGenerator(GENERATOR_ID_7);
        CoordinatedReactiveControl coordinatedReactiveControl7 = generator7.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl7);
        GeneratorStartup generatorStartup7 = generator7.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup7);
        assertEquals(50, generatorStartup7.getMarginalCost(), 0);
        assertEquals(6, coordinatedReactiveControl7.getQPercent(), 0);

        Generator generator8 = getNetwork().getGenerator(GENERATOR_ID_8);
        CoordinatedReactiveControl coordinatedReactiveControl8 = generator8.getExtension(CoordinatedReactiveControl.class);
        assertNotNull(coordinatedReactiveControl8);
        GeneratorStartup generatorStartup8 = generator8.getExtension(GeneratorStartup.class);
        assertNotNull(generatorStartup8);
        assertEquals(60, generatorStartup8.getMarginalCost(), 0);
        assertEquals(12, coordinatedReactiveControl8.getQPercent(), 0);

        assertEquals(60, getNetwork().getGenerator(GENERATOR_ID_9).getRatedS(), 0);
        assertEquals(30, getNetwork().getGenerator(GENERATOR_ID_10).getRatedS(), 0);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.GENERATOR;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.GENERATOR;
    }
}
