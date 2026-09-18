/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.*;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.BooleanAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.GeneratorField;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Tag("IntegrationTest")
class GeneratorModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
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

        DoubleAssignmentInfos assignmentInfos = DoubleAssignmentInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .value(55.)
                .build();

        checkCreateWithWarning(List.of(assignmentInfos), List.of(identifiableAttributes, wrongIdAttributes));
    }

    @Override
    protected void createEquipments() {
        getNetwork().getGenerator(GENERATOR_ID_1)
                .setTargetP(100)
                .setMaxP(500)
                .setMinP(0)
                .setTargetV(10)
                .setTargetQ(20)
                .newExtension(GeneratorStartupAdder.class)
                .withMarginalCost(30.)
                .withPlannedOutageRate(0.25)
                .withPlannedActivePowerSetpoint(40.)
                .withForcedOutageRate(0.55)
                .add();

        getNetwork().getGenerator(GENERATOR_ID_2)
                .setTargetP(200)
                .setMaxP(2000)
                .setMinP(10)
                .setTargetV(10)
                .setTargetQ(20)
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
    protected List<AssignmentInfos<?>> getAssignmentInfos() {

        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.REACTIVE_POWER_SET_POINT.name())
                .value(50.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.DROOP.name())
                .value(2.)
                .filters(List.of(filter3))
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.RATED_NOMINAL_POWER.name())
                .value(2.)
                .filters(List.of(filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MARGINAL_COST.name())
                .value(2.)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.VOLTAGE_SET_POINT.name())
                .value(2.)
                .filters(List.of(filter4))
                .build();

        DoubleAssignmentInfos assignmentInfos6 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.PLANNED_ACTIVE_POWER_SET_POINT.name())
                .value(10.)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos7 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MINIMUM_ACTIVE_POWER.name())
                .value(2.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos8 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.PLANNED_OUTAGE_RATE.name())
                .value(0.1)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos9 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.FORCED_OUTAGE_RATE.name())
                .value(0.05)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos10 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MAXIMUM_ACTIVE_POWER.name())
                .value(300.)
                .filters(List.of(filter1, filter2, filter3, filter4, filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos11 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.TRANSIENT_REACTANCE.name())
                .value(0.2)
                .filters(List.of(filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos12 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.STEP_UP_TRANSFORMER_REACTANCE.name())
                .value(0.3)
                .filters(List.of(filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos13 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.Q_PERCENT.name())
                .value(0.25)
                .filters(List.of(filter4))
                .build();

        BooleanAssignmentInfos assignmentInfos14 = BooleanAssignmentInfos.builder()
                .editedField(GeneratorField.VOLTAGE_REGULATOR_ON.name())
                .value(true)
                .filters(List.of(filter1))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(
                assignmentInfos1,
                assignmentInfos2,
                assignmentInfos3,
                assignmentInfos4,
                assignmentInfos5,
                assignmentInfos6,
                assignmentInfos7,
                assignmentInfos8,
                assignmentInfos9,
                assignmentInfos10,
                assignmentInfos11,
                assignmentInfos12,
                assignmentInfos13,
                assignmentInfos14
        ));

        return infosList;
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.REACTIVE_POWER_SET_POINT.name())
                .value(2.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(GeneratorField.MINIMUM_ACTIVE_POWER.name())
                .value(0.5)
                .filters(List.of(filter3))
                .build();

        return List.of(assignmentInfos1, assignmentInfos2);
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
