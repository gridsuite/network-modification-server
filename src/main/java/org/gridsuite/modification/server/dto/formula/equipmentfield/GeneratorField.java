package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.MinMaxReactiveLimits;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.CoordinatedReactiveControl;
import com.powsybl.iidm.network.extensions.CoordinatedReactiveControlAdder;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuit;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import com.powsybl.iidm.network.extensions.GeneratorStartupAdder;
import org.gridsuite.modification.server.NetworkModificationException;

public enum GeneratorField implements EquipmentField {
    MINIMUM_ACTIVE_POWER,
    MAXIMUM_ACTIVE_POWER,
    RATED_NOMINAL_POWER,
    ACTIVE_POWER_SET_POINT,
    REACTIVE_POWER_SET_POINT,
    VOLTAGE_SET_POINT,
    PLANNING_ACTIVE_POWER_SET_POINT,
    MARGINAL_COST,
    PLANNING_OUTAGE_RATE,
    FORCED_OUTAGE_RATE,
    MINIMUM_REACTIVE_POWER,
    MAXIMUM_REACTIVE_POWER,
    DROOP,
    TRANSIENT_REACTANCE,
    STEP_UP_TRANSFORMER_REACTANCE,
    Q_PERCENT;

    @Override
    public IdentifiableType getIdentifiableType() {
        return IdentifiableType.GENERATOR;
    }

    public static Double getReferenceValue(Generator generator, GeneratorField generatorField) {
        MinMaxReactiveLimits minMaxReactiveLimits = generator.getReactiveLimits(MinMaxReactiveLimits.class);
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        CoordinatedReactiveControl coordinatedReactiveControl = generator.getExtension(CoordinatedReactiveControl.class);
        return switch (generatorField) {
            case MAXIMUM_ACTIVE_POWER -> generator.getMaxP();
            case MINIMUM_ACTIVE_POWER -> generator.getMinP();
            case ACTIVE_POWER_SET_POINT -> generator.getTargetP();
            case RATED_NOMINAL_POWER -> generator.getRatedS();
            case REACTIVE_POWER_SET_POINT -> generator.getTargetQ();
            case VOLTAGE_SET_POINT -> generator.getTargetV();
            case PLANNING_ACTIVE_POWER_SET_POINT -> generatorStartup != null ? generatorStartup.getPlannedActivePowerSetpoint() : null;
            case MARGINAL_COST -> generatorStartup != null ? generatorStartup.getMarginalCost() : null;
            case PLANNING_OUTAGE_RATE -> generatorStartup != null ? generatorStartup.getPlannedOutageRate() : null;
            case FORCED_OUTAGE_RATE -> generatorStartup != null ? generatorStartup.getForcedOutageRate() : null;
            case MINIMUM_REACTIVE_POWER -> minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : null;
            case MAXIMUM_REACTIVE_POWER -> minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : null;
            case DROOP -> activePowerControl != null ? activePowerControl.getDroop() : null;
            case TRANSIENT_REACTANCE -> generatorShortCircuit != null ? generatorShortCircuit.getDirectTransX() : null;
            case STEP_UP_TRANSFORMER_REACTANCE -> generatorShortCircuit != null ? generatorShortCircuit.getStepUpTransformerX() : null;
            case Q_PERCENT -> coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent() : null;
        };
    }

    public static void setNewValue(Generator generator, GeneratorField generatorField, Double newValue) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        MinMaxReactiveLimits minMaxReactiveLimits = generator.getReactiveLimits(MinMaxReactiveLimits.class);
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        CoordinatedReactiveControl coordinatedReactiveControl = generator.getExtension(CoordinatedReactiveControl.class);
        switch (generatorField) {
            case MAXIMUM_ACTIVE_POWER -> generator.setMaxP(newValue);
            case MINIMUM_ACTIVE_POWER -> generator.setMinP(newValue);
            case ACTIVE_POWER_SET_POINT -> generator.setTargetP(newValue);
            case RATED_NOMINAL_POWER -> generator.setRatedS(newValue);
            case REACTIVE_POWER_SET_POINT -> generator.setTargetQ(newValue);
            case VOLTAGE_SET_POINT -> generator.setTargetV(newValue);
            case PLANNING_ACTIVE_POWER_SET_POINT -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                             .withPlannedActivePowerSetpoint(newValue)
                             .add();
                } else {
                    generatorStartup.setPlannedActivePowerSetpoint(newValue);
                }
            }
            case MARGINAL_COST -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(newValue)
                            .add();
                } else {
                    generatorStartup.setMarginalCost(newValue);
                }
            }
            case PLANNING_OUTAGE_RATE -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withPlannedOutageRate(newValue)
                            .add();
                } else {
                    generatorStartup.setPlannedOutageRate(newValue);
                }
            }
            case FORCED_OUTAGE_RATE -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withForcedOutageRate(newValue)
                            .add();
                } else {
                    generatorStartup.setForcedOutageRate(newValue);
                }
            }
            case MINIMUM_REACTIVE_POWER -> {
                if (minMaxReactiveLimits ==null) {
                    throw new NetworkModificationException(NetworkModificationException.Type.MODIFY_GENERATOR_ERROR, "TODO");
                }
                generator.newMinMaxReactiveLimits()
                        .setMinQ(newValue)
                        .setMaxQ(minMaxReactiveLimits.getMaxQ())
                        .add();
            }
            case MAXIMUM_REACTIVE_POWER -> {
                if (minMaxReactiveLimits ==null) {
                    throw new NetworkModificationException(NetworkModificationException.Type.MODIFY_GENERATOR_ERROR, "TODO");
                }
                generator.newMinMaxReactiveLimits()
                        .setMaxQ(newValue)
                        .setMinQ(minMaxReactiveLimits.getMinQ())
                        .add();
            }
            case DROOP -> {
                if (activePowerControl == null) {
                    generator.newExtension(ActivePowerControlAdder.class)
                            .withDroop(newValue)
                            .add();
                } else {
                    activePowerControl.setDroop(newValue);
                }
            }
            case TRANSIENT_REACTANCE -> generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(newValue)
                    .withStepUpTransformerX(generatorShortCircuit == null ? Double.NaN : generatorShortCircuit.getStepUpTransformerX())
                    .add();
            case STEP_UP_TRANSFORMER_REACTANCE -> generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(generatorShortCircuit == null ? 0.0D : generatorShortCircuit.getDirectTransX())
                    .withStepUpTransformerX(newValue)
                    .add();
            case Q_PERCENT -> generator.newExtension(CoordinatedReactiveControlAdder.class)
                    .withQPercent(newValue)
                    .add();
        }
    }
}