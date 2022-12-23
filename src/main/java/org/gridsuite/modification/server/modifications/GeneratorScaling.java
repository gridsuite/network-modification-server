package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.service.FilterService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

public class GeneratorScaling extends AbstractModification {
    private final GeneratorScalingInfos generatorScalableInfos;

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        this.generatorScalableInfos = generatorScalableInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        boolean isIterative = generatorScalableInfos.isIterative();
        List<GeneratorScalingVariation> generatorScalingVariations = generatorScalableInfos.getGeneratorScalingVariations();
        List<String> filterIds = generatorScalingVariations.stream()
                .map(GeneratorScalingVariation::getFilterId)
                .collect(Collectors.toList());
        List<FilterAttributes> filters = new FilterService(null, null).getFiltersMetadata(filterIds);

        for (GeneratorScalingVariation generatorScalingVariation : generatorScalingVariations) {
            var filter = filters.stream()
                    .filter(f -> Objects.equals(generatorScalingVariation.getFilterId(), f.getId()))
                    .findFirst();
            if (filter.isPresent()) {
                applyVariation(network, filter.get(), generatorScalingVariation, isIterative);
            }
        }
    }

    private void applyVariation(Network network,
                                FilterAttributes filter,
                                GeneratorScalingVariation generatorScalingVariation,
                                boolean isIterative) {

        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        switch (generatorScalingVariation.getVariationMode()) {
            case PROPORTIONAL:
                filter.getFilterEquipmentsAttributes()
                    .stream()
                    .forEach(equipment -> {
                        Generator generator = network.getGenerator(equipment.getEquipmentID());
                        if (generator != null) {
                            targetPMap.put(generator.getId(), generator.getTargetP());
                            sum.set(sum.get() + generator.getTargetP());
                        }
                    });
                targetPMap.forEach((id, p) -> {
                    percentages.add((float) ((p / sum.get()) * 100));
                    scalables.add(getScalable(id));
                });
                Scalable proportionalScalable = Scalable.proportional(percentages, scalables, isIterative);
                proportionalScalable.scale(network,
                        getAsked(generatorScalingVariation, sum),
                        Scalable.ScalingConvention.GENERATOR);
                break;
            case PROPORTIONAL_TO_PMAX:
                filter.getFilterEquipmentsAttributes()
                    .stream()
                    .forEach(equipment -> {
                        Generator generator = network.getGenerator(equipment.getEquipmentID());
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

                Scalable proportionalToPmaxScalable = Scalable.proportional(percentages, scalables, isIterative);
                proportionalToPmaxScalable.scale(network,
                        getAsked(generatorScalingVariation, sum),
                        Scalable.ScalingConvention.GENERATOR);
                break;
            case REGULAR_DISTRIBUTION:
                scalables.addAll(filter.getFilterEquipmentsAttributes()
                        .stream()
                        .map(equipment -> {
                            Generator generator = network.getGenerator(equipment.getEquipmentID());
                            if (generator != null) {
                                sum.set(sum.get() + generator.getTargetP());
                                return getScalable(equipment.getEquipmentID());
                            }
                            return null;
                        })
                        .filter(scalable -> scalable != null)
                        .collect(Collectors.toList()));
                if (scalables.size() > 0) {
                    for (Scalable scalable : scalables) {
                        percentages.add((float) (100 / scalables.size()));
                    }

                    Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
                    regularDistributionScalable.scale(network,
                            getAsked(generatorScalingVariation, sum));
                }
                break;
            case VENTILATION:
                var distributionKeys = filter.getFilterEquipmentsAttributes().stream()
                        .mapToDouble(equipment -> equipment.getDistributionKey())
                        .sum();
                if (distributionKeys != 0) {
                    filter.getFilterEquipmentsAttributes().forEach(equipment -> {
                        scalables.add(getScalable(equipment.getEquipmentID()));
                        percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
                    });
                    Scalable ventilationScalable = Scalable.proportional(percentages, scalables, isIterative);
                    ventilationScalable.scale(network, getAsked(generatorScalingVariation, sum));
                }
                break;
            case STACKING_UP:
                Scalable stackingUpScalable = Scalable.stack(filter.getFilterEquipmentsAttributes()
                        .stream()
                        .map(equipment -> getScalable(equipment.getEquipmentID()))
                        .collect(Collectors.toList()).toArray(new Scalable[0]));
                stackingUpScalable.scale(network, getAsked(generatorScalingVariation, sum));
                break;
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
