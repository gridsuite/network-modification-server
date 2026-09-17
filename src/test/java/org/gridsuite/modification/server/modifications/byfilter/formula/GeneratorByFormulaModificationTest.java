/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.GeneratorField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
class GeneratorByFormulaModificationTest extends AbstractByFormulaModificationTest {
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
    void testCreateWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(GENERATOR_ID_1, 1.0);
        IdentifierListFilterEquipmentAttributes wrongIdAttributes = getIdentifiableAttributes("wrongId", 1.0);

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(20.).build())
                .build();

        checkCreateWithWarning(List.of(formulaInfos), List.of(identifiableAttributes, wrongIdAttributes));
        assertEquals(75, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0);
    }

    @Test
    void testWithNullReferenceFieldOrValue() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(GENERATOR_ID_1, 1.0);
        AbstractFilter filterEquipments = getFilterEquipments(FILTER_ID_1, List.of(identifiableAttributes));

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(new FilterInfos(FILTER_ID_1, "filter1")))
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().build())
                .build();

        checkCreateWithError(List.of(formulaInfos), List.of(filterEquipments));
    }

    protected void createEquipments() {
        getNetwork().getGenerator(GENERATOR_ID_1)
                .setTargetP(100)
                .setMaxP(500)
                .setMinP(0)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_2)
                .setTargetP(200)
                .setMaxP(2000)
                .setMinP(50)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
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

    @Override
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
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(GeneratorField.ACTIVE_POWER_SET_POINT.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(GeneratorField.DROOP.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.DROOP.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos3 = getFormulaInfo(GeneratorField.RATED_NOMINAL_POWER.name(),
                List.of(filter5),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MAXIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build());

        FormulaInfos formulaInfos4 = getFormulaInfo(GeneratorField.MARGINAL_COST.name(),
                List.of(filter1),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MARGINAL_COST.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos5 = getFormulaInfo(GeneratorField.VOLTAGE_SET_POINT.name(),
                List.of(filter4),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.VOLTAGE_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos6 = getFormulaInfo(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name(),
                List.of(filter1),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(10.).build());

        FormulaInfos formulaInfos7 = getFormulaInfo(GeneratorField.MINIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos8 = getFormulaInfo(GeneratorField.PLANNED_OUTAGE_RATE.name(),
                List.of(filter1),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.PLANNED_OUTAGE_RATE.name()).build(),
                ReferenceFieldOrValue.builder().value(0.1).build());

        FormulaInfos formulaInfos9 = getFormulaInfo(GeneratorField.FORCED_OUTAGE_RATE.name(),
                List.of(filter1),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.FORCED_OUTAGE_RATE.name()).build(),
                ReferenceFieldOrValue.builder().value(10.).build());

        FormulaInfos formulaInfos10 = getFormulaInfo(GeneratorField.MAXIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2, filter3, filter4, filter5),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MAXIMUM_ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos11 = getFormulaInfo(GeneratorField.TRANSIENT_REACTANCE.name(),
                List.of(filter2),
                Operator.SUBTRACTION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.TRANSIENT_REACTANCE.name()).build(),
                ReferenceFieldOrValue.builder().value(0.2).build());

        FormulaInfos formulaInfos12 = getFormulaInfo(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name(),
                List.of(filter2),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name()).build(),
                ReferenceFieldOrValue.builder().value(0.3).build());

        FormulaInfos formulaInfos13 = getFormulaInfo(GeneratorField.Q_PERCENT.name(),
                List.of(filter4),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(GeneratorField.Q_PERCENT.name()).build(),
                ReferenceFieldOrValue.builder().value(0.25).build());

        return List.of(formulaInfos1,
                formulaInfos2,
                formulaInfos3,
                formulaInfos4,
                formulaInfos5,
                formulaInfos6,
                formulaInfos7,
                formulaInfos8,
                formulaInfos9,
                formulaInfos10,
                formulaInfos11,
                formulaInfos12,
                formulaInfos13);
    }

    @Override
    protected List<FormulaInfos> getUpdatedFormulaInfos() {
        FormulaInfos formulaInfos1 = FormulaInfos.builder()
                .editedField(GeneratorField.REACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(2.).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(50.).build())
                .operator(Operator.MULTIPLICATION)
                .filters(List.of(filter1, filter2))
                .build();

        FormulaInfos formulaInfos2 = FormulaInfos.builder()
                .editedField(GeneratorField.MINIMUM_ACTIVE_POWER.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().equipmentField(GeneratorField.MINIMUM_ACTIVE_POWER.name()).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(0.5).build())
                .operator(Operator.DIVISION)
                .filters(List.of(filter3))
                .build();

        return List.of(formulaInfos1, formulaInfos2);
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
