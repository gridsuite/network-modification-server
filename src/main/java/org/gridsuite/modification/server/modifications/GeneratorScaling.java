package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.SpringContext;
import org.springframework.util.CollectionUtils;

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

        String workingVariantId = network.getVariantManager().getWorkingVariantId();
        UUID uuid = ((NetworkImpl) network).getUuid();
        List<FilterEquipments> exportFilters = SpringContext.getBean(FilterService.class)
                .exportFilters(filterIds.stream().distinct().collect(Collectors.toList()), uuid, workingVariantId);

        var filterWithWrongIds = exportFilters.stream()
                        .filter(f -> !CollectionUtils.isEmpty(f.getNotFoundEquipments()))
                        .collect(Collectors.toList());

        if (filterWithWrongIds.size() == exportFilters.size()) {
            throw new NetworkModificationException(NetworkModificationException.Type.GENERATOR_SCALING_ERROR, "All filters contains equipments with wrong ids");
        }

        if (!filterWithWrongIds.isEmpty()) {
            filterWithWrongIds.forEach(f -> {
                var generatorsIds = String.join(", ", f.getNotFoundEquipments());
                createReport(subReporter,
                        "generatorScalingCreatedWarning" + f.getFilterId(),
                        "Cannot find the following generators " + generatorsIds + " in filter " + f.getFilterId(),
                        TypedValue.WARN_SEVERITY);
            });
        }

        var wrongFiltersId = filterWithWrongIds.stream().map(f -> f.getFilterId().toString()).collect(Collectors.toList());

        generatorScalingVariations.forEach(variation -> {
            variation.getFilters().forEach(filter -> {
                FilterEquipments filterEquipments = exportFilters.stream()
                        .filter(f -> Objects.equals(f.getFilterId().toString(), filter.getId()))
                        .findAny()
                        .orElse(null);

                if (wrongFiltersId.contains(filter.getId()) || filterEquipments == null) {
                    return;
                }

                List<IdentifiableAttributes> identifiableAttributes = filterEquipments.getIdentifiableAttributes();
                applyVariation(network, identifiableAttributes, variation);
            });
        });

        createReport(subReporter, "generatorScalingCreated", "new generator scaling created", TypedValue.INFO_SEVERITY);
    }

    private void applyVariation(Network network,
                                List<IdentifiableAttributes> identifiableAttributes,
                                GeneratorScalingVariation generatorScalingVariation) {
        switch (generatorScalingVariation.getVariationMode()) {
            case PROPORTIONAL:
                List<Generator> proportionalGenerators = identifiableAttributes
                        .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
                applyProportionalVariation(network, proportionalGenerators, generatorScalingVariation);
                break;
            case PROPORTIONAL_TO_PMAX:
                List<Generator> proportionalToPmaxGenerators = identifiableAttributes
                        .stream().map(attribute -> network.getGenerator(attribute.getId())).collect(Collectors.toList());
                applyProportionalToPmaxVariation(network, proportionalToPmaxGenerators, generatorScalingVariation);
                break;
            case REGULAR_DISTRIBUTION:
                network.getGenerator(identifiableAttributes.get(0).getId());
                List<Generator> regularDistributionGenerators = identifiableAttributes
                        .stream()
                        .map(attribute -> network.getGenerator(attribute.getId()))
                        .filter(Objects::nonNull)
                        .collect(Collectors.toList());
                applyRegularDistributionVariation(network, regularDistributionGenerators, generatorScalingVariation);
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
                                                   List<Generator> generators,
                                                   GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        boolean isIterative = generatorScalableInfos.isIterative();
        List<Scalable> scalables = generators.stream()
                .map(generator -> {
                    sum.set(sum.get() + generator.getTargetP());
                    return getScalable(generator.getId());
                }).collect(Collectors.toList());

        List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), (float) (100 / scalables.size())));

        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables, isIterative);
        regularDistributionScalable.scale(network,
                getAsked(generatorScalingVariation, sum));
    }

    private void applyProportionalToPmaxVariation(Network network,
                                                  List<Generator> generators,
                                                  GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> maxPSum = new AtomicReference<>(0D);
        AtomicReference<Double> targetPSum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getMaxP());
            maxPSum.set(maxPSum.get() + generator.getMaxP());
            targetPSum.set(targetPSum.get() + generator.getTargetP());
        });
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
                                            List<Generator> generators,
                                            GeneratorScalingVariation generatorScalingVariation) {
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        boolean isIterative = generatorScalableInfos.isIterative();
        generators.forEach(generator -> {
            targetPMap.put(generator.getId(), generator.getTargetP());
            sum.set(sum.get() + generator.getTargetP());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables, isIterative);
        proportionalScalable.scale(network,
                getAsked(generatorScalingVariation, sum));
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
