package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.FilterEquipmentAttributes;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.SpringContext;

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

    FilterService filterService;

    private final GeneratorScalingInfos generatorScalableInfos;

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        this.generatorScalableInfos = generatorScalableInfos;
    }

    public void setFilterService(FilterService filterService) {
        this.filterService = filterService;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        List<GeneratorScalingVariation> generatorScalingVariations = generatorScalableInfos.getGeneratorScalingVariations();
        List<String> filterIds = new ArrayList<>();
        generatorScalingVariations.forEach(variation -> {
            filterIds.addAll(variation.getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList()));
        });

        List<FilterAttributes> filters = SpringContext.getBean(FilterService.class)
                .getFilters(filterIds.stream().distinct().collect(Collectors.toList()));

        if (!filters.isEmpty()) {
            for (GeneratorScalingVariation generatorScalingVariation : generatorScalingVariations) {
                var filterIdsList = generatorScalingVariation.getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList());
                var filterList = filters.stream()
                        .filter(f -> filterIdsList.contains(f.getId()))
                        .collect(Collectors.toList());
                if (!filterList.isEmpty()) {
                    filterList.forEach(filter -> applyVariation(network, filter, generatorScalingVariation, subReporter));
                } else {
                    throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "one of the variations does not have a correct filters");
                }
            }
        } else {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "Filters list is Empty");
        }
    }

    private void applyVariation(Network network,
                                FilterAttributes filter,
                                GeneratorScalingVariation generatorScalingVariation,
                                Reporter subReporter) {
        switch (generatorScalingVariation.getVariationMode()) {
            case PROPORTIONAL:
                applyProportionalVariation(network, subReporter, filter, generatorScalingVariation);
                break;
            case PROPORTIONAL_TO_PMAX:
                applyProportionalToPmaxVariation(network, subReporter, filter, generatorScalingVariation);
                break;
            case REGULAR_DISTRIBUTION:
                applyRegularDistributionVariation(network, subReporter, filter, generatorScalingVariation);
                break;
            case VENTILATION:
                scaleWithVentilationMode(network, filter, generatorScalingVariation);
                break;
            case STACKING_UP:
                scaleWithStackingUpMode(network, filter, generatorScalingVariation);
                break;
            default:
                throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "This variation mode is not supported : " + generatorScalingVariation.getVariationMode().name());
        }
        createReport(subReporter,"generatorScalingCreated","new load scaling created", TypedValue.INFO_SEVERITY);
    }

    private void scaleWithStackingUpMode(Network network, FilterAttributes filter, GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Scalable stackingUpScalable = Scalable.stack(filter.getFilterEquipmentsAttributes()
                .stream()
                .map(equipment -> getScalable(equipment.getEquipmentID())).toArray(Scalable[]::new));
        stackingUpScalable.scale(network, getAsked(generatorScalingVariation, sum));
    }

    private void scaleWithVentilationMode(Network network,
                                          FilterAttributes filter,
                                          GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        var distributionKeys = filter.getFilterEquipmentsAttributes()
                .stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(FilterEquipmentAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "This mode is available only for equipment with distribution key");
        }

        filter.getFilterEquipmentsAttributes().forEach(equipment -> {
            scalables.add(getScalable(equipment.getEquipmentID()));
            percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
        });
        Scalable ventilationScalable = Scalable.proportional(percentages, scalables, isIterative);
        ventilationScalable.scale(network, getAsked(generatorScalingVariation, sum));
    }

    private void applyRegularDistributionVariation(Network network,
                                                   Reporter subReporter,
                                                   FilterAttributes filter,
                                                   GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative =generatorScalableInfos.isIterative();
        List<String> notFoundEquipments = new ArrayList<>();
        scalables.addAll(filter.getFilterEquipmentsAttributes()
                .stream()
                .map(equipment -> {
                    Generator generator = network.getGenerator(equipment.getEquipmentID());
                    if (generator != null) {
                        sum.set(sum.get() + generator.getTargetP());
                        return getScalable(equipment.getEquipmentID());
                    } else {
                        notFoundEquipments.add(equipment.getEquipmentID());
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList()));

        if (scalables.isEmpty()) {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR,
                    "Error while creating generator scaling : All generators of filter " + filter.getId() +  " not found : " + String.join(", ", notFoundEquipments));
        }

        if (!notFoundEquipments.isEmpty()) {
            createReport(subReporter,
                    NetworkModificationException.Type.GENERATOR_SCALING_ERROR.name(),
                    "Cannot find the following generators : " + String.join(", ", notFoundEquipments),
                    TypedValue.WARN_SEVERITY);
        }

        List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), (float) (100 / scalables.size())));

        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
        regularDistributionScalable.scale(network,
                getAsked(generatorScalingVariation, sum));
    }

    private void applyProportionalToPmaxVariation(Network network,
                                                  Reporter subReporter,
                                                  FilterAttributes filter,
                                                  GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        List<String> notFoundEquipments = new ArrayList<>();
        filter.getFilterEquipmentsAttributes()
            .forEach(equipment -> {
                Generator generator = network.getGenerator(equipment.getEquipmentID());
                if (generator != null) {
                    targetPMap.put(generator.getId(), generator.getMaxP());
                    sum.set(sum.get() + generator.getMaxP());
                } else {
                    notFoundEquipments.add(equipment.getEquipmentID());
                }
            });
        checkVariationFilter(filter, subReporter, targetPMap, notFoundEquipments);
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalToPmaxScalable = Scalable.proportional(percentages, scalables, isIterative);
        proportionalToPmaxScalable.scale(network,
                getAsked(generatorScalingVariation, sum),
                Scalable.ScalingConvention.GENERATOR);
    }

    private void applyProportionalVariation(Network network,
                                            Reporter subReporter,
                                            FilterAttributes filter,
                                            GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        List<String> notFoundEquipments = new ArrayList<>();
        filter.getFilterEquipmentsAttributes()
            .forEach(equipment -> {
                Generator generator = network.getGenerator(equipment.getEquipmentID());
                if (generator != null) {
                    targetPMap.put(generator.getId(), generator.getTargetP());
                    sum.set(sum.get() + generator.getTargetP());
                } else {
                    notFoundEquipments.add(equipment.getEquipmentID());
                }
            });
        checkVariationFilter(filter, subReporter, targetPMap, notFoundEquipments);
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables, isIterative);
        proportionalScalable.scale(network,
                getAsked(generatorScalingVariation, sum));
    }

    private void checkVariationFilter(FilterAttributes filter, Reporter subReporter, Map<String, Double> targetPMap, List<String> notFoundEquipments) {
        if (!notFoundEquipments.isEmpty() && notFoundEquipments.size() != filter.getFilterEquipmentsAttributes().size()) {
            // TODO check if this the right behavior
            // Send a warning when some of the generator in one filter cannot be found
            createReport(subReporter,
                    "GENERATOR_SCALING_ERROR",
                    "Generators of filter :" + filter.getId() + " not found : " + String.join(", ", notFoundEquipments),
                    TypedValue.WARN_SEVERITY);
        }

        if (targetPMap.isEmpty()) {
            // TODO check if this the right behavior
            // throw error when all of the generator in one filter cannot be found
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR,
                    "Error while creating generator scaling : All generators of filter " + filter.getId() +  " not found : " + String.join(", ", notFoundEquipments));
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
