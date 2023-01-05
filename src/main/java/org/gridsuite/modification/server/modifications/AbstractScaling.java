package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.AbstractScalingVariationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.ScalingInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.service.SpringContext;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.ScalingUtils.createReport;

public abstract class AbstractScaling extends AbstractModification {
    protected final ScalingInfos scalingInfos;

    public AbstractScaling(ScalingInfos scalingInfos) {
        this.scalingInfos = scalingInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        var variationsInfos = scalingInfos.getVariations();
        List<String> filterIds = new ArrayList<>();
        variationsInfos.forEach(variation -> {
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
            throw new NetworkModificationException(getExceptionType(), "All filters contains equipments with wrong ids");
        }

        if (!filterWithWrongIds.isEmpty()) {
            filterWithWrongIds.forEach(f -> {
                var equipmentIds = String.join(", ", f.getNotFoundEquipments());
                createReport(subReporter,
                        getExceptionType().name() + f.getFilterId(),
                        "Cannot find the following equipments " + equipmentIds + " in filter " + f.getFilterId(),
                        TypedValue.WARN_SEVERITY);
            });
        }

        var wrongFiltersId = filterWithWrongIds.stream().map(f -> f.getFilterId().toString()).collect(Collectors.toList());

        variationsInfos.forEach(variation -> {
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

        createReport(subReporter, getModificationType().name(), "new scaling created", TypedValue.INFO_SEVERITY);
    }

    private void applyVariation(Network network,
                                List<IdentifiableAttributes> identifiableAttributes,
                                AbstractScalingVariationInfos variation) {
        switch (variation.getVariationMode()) {
            case PROPORTIONAL:
                applyProportionalVariation(network, identifiableAttributes, variation);
                break;
            case PROPORTIONAL_TO_PMAX:
                applyProportionalToPmaxVariation(network, identifiableAttributes, variation);
                break;
            case REGULAR_DISTRIBUTION:
                applyRegularDistributionVariation(network, identifiableAttributes, variation);
                break;
            case VENTILATION:
                applyVentilationVariation(network, identifiableAttributes, variation);
                break;
            case STACKING_UP:
                applyStackingUpVariation(network, identifiableAttributes, variation);
                break;
            default:
                throw new NetworkModificationException(getExceptionType(), "This variation mode is not supported : " + variation.getVariationMode().name());
        }
    }

    public void applyStackingUpVariation(Network network, List<IdentifiableAttributes> identifiableAttributes, AbstractScalingVariationInfos variationInfos) {
        throw new NetworkModificationException(getExceptionType());
    }

    public void applyVentilationVariation(Network network,
                                           List<IdentifiableAttributes> identifiableAttributes,
                                           AbstractScalingVariationInfos generatorScalingVariation) {
        throw new NetworkModificationException(getExceptionType());
    }

    public void applyRegularDistributionVariation(Network network,
                                                   List<IdentifiableAttributes> identifiableAttributes,
                                                   AbstractScalingVariationInfos generatorScalingVariation) {
        throw new NetworkModificationException(getExceptionType());
    }

    public void applyProportionalToPmaxVariation(Network network,
                                                  List<IdentifiableAttributes> identifiableAttributes,
                                                  AbstractScalingVariationInfos generatorScalingVariation) {
        throw new NetworkModificationException(getExceptionType());
    }

    public void applyProportionalVariation(Network network,
                                            List<IdentifiableAttributes> identifiableAttributes,
                                            AbstractScalingVariationInfos variationInfos) {
        throw new NetworkModificationException(getExceptionType());
    }

    public abstract double getAsked(AbstractScalingVariationInfos variationInfos, AtomicReference<Double> sum);

    public abstract Scalable getScalable(String id);

    public abstract NetworkModificationException.Type getExceptionType();

    public abstract ModificationType getModificationType();
}
