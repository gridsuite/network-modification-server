package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.FilterEquipmentAttributes;
import org.gridsuite.modification.server.dto.FilterInfo;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.ScalingUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.ScalingUtils.createReport;

public class GeneratorScaling extends AbstractModification {

    @Autowired
    FilterService filterService;

    private final GeneratorScalingInfos generatorScalableInfos;

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        this.generatorScalableInfos = generatorScalableInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        boolean isIterative = generatorScalableInfos.isIterative();
        List<GeneratorScalingVariation> generatorScalingVariations = generatorScalableInfos.getGeneratorScalingVariations();
        List<String> filterIds = new ArrayList<>();
        generatorScalingVariations.forEach(variation -> {
            filterIds.addAll(variation.getFilters().stream().map(FilterInfo::getId).collect(Collectors.toList()));
        });

        List<FilterAttributes> filters = new FilterService("http://localhost:5027").getFilters(filterIds);

        if (!filters.isEmpty()) {
            for (GeneratorScalingVariation generatorScalingVariation : generatorScalingVariations) {
                var filterIdsList = generatorScalingVariation.getFilters().stream().map(FilterInfo::getId).collect(Collectors.toList());
                var filterList = filters.stream()
                        .filter(f -> filterIdsList.contains(f.getId()))
                        .collect(Collectors.toList());
                if (!filterList.isEmpty()) {
                    filterList.forEach(filter -> applyVariation(network, filter, generatorScalingVariation, isIterative, subReporter));
                }
            }
        } else {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "Filters list is Empty");
        }
    }

    private void applyVariation(Network network,
                                FilterAttributes filter,
                                GeneratorScalingVariation generatorScalingVariation,
                                boolean isIterative,
                                Reporter subReporter) {

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
                        getAsked(generatorScalingVariation, sum));
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
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList()));

                if (!scalables.isEmpty()) {
                    percentages.addAll(Collections.nCopies(scalables.size(), (float)(100 / scalables.size())));

                    Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
                    regularDistributionScalable.scale(network,
                            getAsked(generatorScalingVariation, sum));
                }
                break;
            case VENTILATION:
                var distributionKeys = filter.getFilterEquipmentsAttributes().stream()
                        .mapToDouble(FilterEquipmentAttributes::getDistributionKey)
                        .sum();
                if (distributionKeys != 0) {
                    filter.getFilterEquipmentsAttributes().forEach(equipment -> {
                        scalables.add(getScalable(equipment.getEquipmentID()));
                        percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
                    });
                    Scalable ventilationScalable = Scalable.proportional(percentages, scalables, isIterative);
                    ventilationScalable.scale(network, getAsked(generatorScalingVariation, sum));
                } else {
                    throw new PowsyblException("");
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
        createReport(subReporter,"loadScalingCreated","new load scaling created", TypedValue.INFO_SEVERITY);
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
