package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.FilterEquipmentAttributes;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.SpringContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.ScalingUtils.createReport;

public class GeneratorScaling extends AbstractModification {

    private final GeneratorScalingInfos generatorScalableInfos;

    public GeneratorScaling(GeneratorScalingInfos generatorScalableInfos) {
        this.generatorScalableInfos = generatorScalableInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        List<GeneratorScalingVariation> generatorScalingVariations = generatorScalableInfos.getGeneratorScalingVariations();
        List<String> filterIds = new ArrayList<>();
        generatorScalingVariations.forEach(variation -> {
            filterIds.addAll(variation.getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList()));
        });

        List<FilterEquipments> exportFilters = SpringContext.getBean(FilterService.class)
                .exportFilters(filterIds.stream().distinct().collect(Collectors.toList()), ((NetworkImpl) network).getUuid(), null);

        generatorScalingVariations.forEach(variation -> {
            variation.getFilters().forEach(filter -> {
                FilterEquipments filterEquipments = exportFilters.stream()
                        .filter(f -> Objects.equals(f.getFilterId().toString(), filter.getId()))
                        .findAny()
                        .orElse(null);

                if (filterEquipments != null) {
                    if (filterEquipments.getNotFoundEquipments().isEmpty()) {
                        List<IdentifiableAttributes> identifiableAttributes = filterEquipments.getIdentifiableAttributes();
                        if (!identifiableAttributes.isEmpty()) {
                            applyVariation(network, filter.getName(), identifiableAttributes, variation, subReporter);
                        }
                    }
                }
                /*List<IdentifiableAttributes> identifiableAttributes = exportFilters.get(filter.getId());
                if (identifiableAttributes.isEmpty()) {
                    createReport(subReporter,
                            NetworkModificationException.Type.GENERATOR_SCALING_ERROR.name(),
                            "Cannot find generators for filter : " + filter.getName(),
                            TypedValue.WARN_SEVERITY);
                } else {
                    applyVariation(network, filter.getName(), identifiableAttributes, variation, subReporter);
                }*/
            });
        });

        /*if (!filters.isEmpty()) {
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
        }*/
    }

    private void applyVariation(Network network,
                                String filterName,
                                List<IdentifiableAttributes> identifiableAttributes,
                                GeneratorScalingVariation generatorScalingVariation,
                                Reporter subReporter) {
        switch (generatorScalingVariation.getVariationMode()) {
            case PROPORTIONAL:
                List<Generator> proportionalGenerators = identifiableAttributes
                        .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
                /*if (proportionalGenerators.size() != identifiableAttributes.size())
                {
                    var generatorIds = proportionalGenerators.stream().map(Generator::getId).collect(Collectors.toList());
                    var notFound = identifiableAttributes.stream()
                            .map(IdentifiableAttributes::getId)
                            .filter(id -> !generatorIds.contains(id))
                            .collect(Collectors.joining(","));
                    if (generatorIds.isEmpty()) {
                        throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR,
                                "Cannot find the following generators : " + notFound);
                    } else {
                        createReport(subReporter,
                                NetworkModificationException.Type.GENERATOR_SCALING_ERROR.name(),
                                "Cannot find the following generators : " + notFound + " of filter : " + filterName,
                                TypedValue.WARN_SEVERITY);
                        break;
                    }
                }*/
                applyProportionalVariation(network, subReporter, proportionalGenerators, generatorScalingVariation);
                break;
            case PROPORTIONAL_TO_PMAX:
                List<Generator> proportionalToPmaxGenerators = identifiableAttributes
                        .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
                applyProportionalToPmaxVariation(network, subReporter, proportionalToPmaxGenerators, generatorScalingVariation);
                break;
            case REGULAR_DISTRIBUTION:
                List<Generator> regularDistributionGenerators = identifiableAttributes
                        .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
                applyRegularDistributionVariation(network, subReporter, regularDistributionGenerators, generatorScalingVariation);
                break;
            case VENTILATION:
                scaleWithVentilationMode(network, identifiableAttributes, generatorScalingVariation);
                break;
            case STACKING_UP:
                scaleWithStackingUpMode(network, identifiableAttributes, generatorScalingVariation);
                break;
            default:
                throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "This variation mode is not supported : " + generatorScalingVariation.getVariationMode().name());
        }
        createReport(subReporter,"generatorScalingCreated","new generator scaling created", TypedValue.INFO_SEVERITY);
    }

    private void scaleWithStackingUpMode(Network network, List<IdentifiableAttributes> identifiableAttributes, GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Scalable stackingUpScalable = Scalable.stack(identifiableAttributes.stream()
                .map(equipment -> getScalable(equipment.getId())).toArray(Scalable[]::new));
        stackingUpScalable.scale(network, getAsked(generatorScalingVariation, sum));
    }

    private void scaleWithVentilationMode(Network network,
                                          List<IdentifiableAttributes> identifiableAttributes,
                                          GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        var distributionKeys = identifiableAttributes.stream()
                .filter(equipment -> equipment.getDistributionKey() != null)
                .mapToDouble(IdentifiableAttributes::getDistributionKey)
                .sum();
        if (distributionKeys == 0) {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "This mode is available only for equipment with distribution key");
        }

        identifiableAttributes.forEach(equipment -> {
            scalables.add(getScalable(equipment.getId()));
            percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
        });
        Scalable ventilationScalable = Scalable.proportional(percentages, scalables, isIterative);
        ventilationScalable.scale(network, getAsked(generatorScalingVariation, sum));
    }

    private void applyRegularDistributionVariation(Network network,
                                                   Reporter subReporter,
                                                   List<Generator> generators,
                                                   GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        boolean isIterative =generatorScalableInfos.isIterative();
        //List<String> notFoundEquipments = new ArrayList<>();
        List<Scalable> scalables = generators.stream()
                .map(generator -> {
                    sum.set(sum.get() + generator.getTargetP());
                    return getScalable(generator.getId());
                }).collect(Collectors.toList());

        /*if (scalables.isEmpty()) {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR,
                    "Error while creating generator scaling : All generators of filter  not found : " + String.join(", ", notFoundEquipments));
        }

        if (!notFoundEquipments.isEmpty()) {
            createReport(subReporter,
                    NetworkModificationException.Type.GENERATOR_SCALING_ERROR.name(),
                    "Cannot find the following generators : " + String.join(", ", notFoundEquipments),
                    TypedValue.WARN_SEVERITY);
        }*/

        List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), (float) (100 / scalables.size())));

        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
        regularDistributionScalable.scale(network,
                getAsked(generatorScalingVariation, sum));
    }

    private void applyProportionalToPmaxVariation(Network network,
                                                  Reporter subReporter,
                                                  List<Generator> generators,
                                                  GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> maxPSum = new AtomicReference<>(0D);
        AtomicReference<Double> targetPSum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        //List<String> notFoundEquipments = new ArrayList<>();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getMaxP());
            maxPSum.set(maxPSum.get() + generator.getMaxP());
            targetPSum.set(targetPSum.get() + generator.getTargetP());
            });
        //checkVariationFilter(filter, subReporter, targetPMap, notFoundEquipments);
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / maxPSum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalToPmaxScalable = Scalable.proportional(percentages, scalables, isIterative);
        proportionalToPmaxScalable.scale(network,
                getAsked(generatorScalingVariation, targetPSum),
                Scalable.ScalingConvention.GENERATOR);
    }

    private void applyProportionalVariation(Network network,
                                            Reporter subReporter,
                                            List<Generator> generators,
                                            GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        List<String> notFoundEquipments = new ArrayList<>();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getTargetP());
            sum.set(sum.get() + generator.getTargetP());
        });
        //checkVariationFilter(identifiableAttributes, subReporter, targetPMap, notFoundEquipments);
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
