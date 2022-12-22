package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.GeneratorScalableInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class GeneratorScaling extends AbstractModification {
    private final GeneratorScalableInfos generatorScalableInfos;

    public GeneratorScaling(GeneratorScalableInfos generatorScalableInfos) {
        this.generatorScalableInfos = generatorScalableInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        generatorScalableInfos.getGeneratorScalingVariations()
                .stream()
                .forEach(variation -> variation.getFilterInfos().getEquipments()
                        .stream()
                        .forEach(equipment -> Scalable.onGenerator(equipment.getId())));
    }

    private void applyVariation(Network network, GeneratorScalingVariation generatorScalingVariation, boolean isIterative) {
        var scalableList = generatorScalingVariation.getFilterInfos().getEquipments()
                .stream()
                .map(equipment -> getScalable(equipment.getId()))
                .collect(Collectors.toList());
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        switch (generatorScalingVariation.getVariationMode()) {
            case PROPORTIONAL:
                generatorScalingVariation.getFilterInfos().getEquipments()
                                .stream()
                                .forEach(equipment -> {
                                    Generator generator = network.getGenerator(equipment.getId());
                                    if (generator != null) {
                                        targetPMap.put(generator.getId(), generator.getTargetP());
                                        sum.set(sum.get() + generator.getTargetP());
                                    }
                                });
                targetPMap.forEach((id, p) -> {
                    percentages.add((float) ((p / sum.get()) * 100));
                    scalables.add(getScalable(id));
                });
                var proportionalScalable = Scalable.proportional(percentages, scalables, isIterative);
                proportionalScalable.scale(network,
                        getAsked(generatorScalingVariation, sum),
                        Scalable.ScalingConvention.GENERATOR);
                break;
            case PROPORTIONAL_TO_PMAX:
                generatorScalingVariation.getFilterInfos().getEquipments()
                        .stream()
                        .forEach(equipment -> {
                            Generator generator = network.getGenerator(equipment.getId());
                            if (generator != null) {
                                targetPMap.put(generator.getId(), generator.getMaxP());
                                sum.set(sum.get() + generator.getMaxP());
                            }
                        });
                targetPMap.forEach((id, p) -> {
                    percentages.add((float) ((p / sum.get()) * 100));
                    scalables.add(getScalable(id));
                });
                Scalable.proportional(percentages, scalables, isIterative);

                var proportionalToPmaxScalable = Scalable.proportional(percentages, scalables, isIterative);
                proportionalToPmaxScalable.scale(network,
                        getAsked(generatorScalingVariation, sum),
                        Scalable.ScalingConvention.GENERATOR);
                break;
            case REGULAR_DISTRIBUTION:
                scalables.addAll(generatorScalingVariation.getFilterInfos().getEquipments()
                        .stream()
                        .map(equipment -> {
                            sum.set(sum.get() + network.getGenerator(equipment.getId()).getTargetP());
                            return getScalable(equipment.getId());
                        })
                        .collect(Collectors.toList()));
                if (scalables.size() > 0) {
                    for (Scalable scalable : scalables) {
                        percentages.add((float) (100 / scalables.size()));
                    }
                }

                var regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
                regularDistributionScalable.scale(network,
                        getAsked(generatorScalingVariation, sum),
                        Scalable.ScalingConvention.GENERATOR);
            case VENTILATION:
                var distributionKeys = generatorScalingVariation.getFilterInfos().getEquipments().stream()
                        .mapToDouble(equipment -> equipment.getDistributionKey())
                        .sum();
                generatorScalingVariation.getFilterInfos().getEquipments().forEach(equipment -> {
                    scalables.add(getScalable(equipment.getId()));
                    percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
                });
                Scalable.proportional(percentages, scalables, isIterative);
                var ventilationScalable = Scalable.proportional(percentages, scalables, isIterative);
                ventilationScalable.scale(network,
                        getAsked(generatorScalingVariation, sum),
                        Scalable.ScalingConvention.GENERATOR);
                break;
            case STACKING_UP:
                Double variationValue = generatorScalingVariation.getVariationValue();
                generatorScalingVariation.getFilterInfos().getEquipments()
                        .stream()
                        .forEach(equipment -> {
                            var availableCoieff = variationValue;
                            while (availableCoieff != 0) {
                                Generator generator = network.getGenerator(equipment.getId());
                                if (generator != null) {
                                    sum.set(sum.get() + generator.getTargetP());
                                    var availableUp = generator.getMaxP() - generator.getTargetP();
                                    var addedValue = availableCoieff <= availableUp ? availableCoieff : availableUp;
                                    availableCoieff = availableCoieff - addedValue;
                                    targetPMap.put(generator.getId(), addedValue);
                                }
                            }
                        });
                targetPMap.forEach((id, p) -> {
                    percentages.add((float) ((p / variationValue) * 100));
                    scalables.add(getScalable(id));
                });
                Scalable.stack(generatorScalingVariation.getFilterInfos().getEquipments().toArray(new String[0]));
            default:
                throw new PowsyblException("");
        }
    }

    private double getAsked(GeneratorScalingVariation generatorScalingVariation, AtomicReference<Double> sum) {
        return generatorScalableInfos.getVariationType() == VariationType.DELTA_P
                ? generatorScalingVariation.getVariationValue()
                : generatorScalingVariation.getVariationValue() - sum.get();
    }

    private Scalable getScalable(String id) {
        return Scalable.onGenerator(id);
    }
}
