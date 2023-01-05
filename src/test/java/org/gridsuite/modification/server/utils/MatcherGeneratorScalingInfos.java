package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.AbstractScalingVariationInfos;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;

import java.util.List;
import java.util.Objects;

public class MatcherGeneratorScalingInfos extends MatcherModificationInfos<GeneratorScalingInfos> {
    protected MatcherGeneratorScalingInfos(GeneratorScalingInfos ref) {
        super(ref);
    }

    public static MatcherGeneratorScalingInfos createMatcherGeneratorScalingInfos(GeneratorScalingInfos generatorScalingInfos) {
        return new MatcherGeneratorScalingInfos(generatorScalingInfos);
    }

    public boolean matchesSafely(GeneratorScalingInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getVariationType(), m.getVariationType())
                && Objects.equals(reference.isIterative(), m.isIterative())
                && matchesVariations(m.getVariations());
    }

    private boolean matchesVariations(List<AbstractScalingVariationInfos> generatorScalingVariations) {
        if (!matchesList(reference.getVariations(), generatorScalingVariations)) {
            return false;
        }

        for (int index = 0; index < generatorScalingVariations.size(); index++) {
            if (!matchesVariation(reference.getVariations().get(index),
                    generatorScalingVariations.get(index))) {
                return false;
            }
        }

        return true;
    }

    private boolean matchesVariation(AbstractScalingVariationInfos variation1, AbstractScalingVariationInfos variation2) {
        return Objects.equals(variation1.getVariationValue(), variation2.getVariationValue())
                && Objects.equals(variation1.getVariationMode(), variation2.getVariationMode())
                && matchesFilters(variation1.getFilters(), variation2.getFilters());
    }

    private boolean matchesFilters(List<FilterInfos> filterList1, List<FilterInfos> filterList2) {
        if (!matchesList(filterList1, filterList2)) {
            return false;
        }

        for (int index = 0; index < filterList1.size(); index++) {
            if (!matchesFilter(filterList1.get(index), filterList2.get(index))) {
                return false;
            }
        }

        return true;
    }

    private boolean matchesFilter(FilterInfos filter1, FilterInfos filter2) {
        return Objects.equals(filter1.getName(), filter2.getName());
    }

    private boolean matchesList(List<?> list1, List<?> list2) {
        if ((list1 == null) != (list2 == null)) {
            return false;
        }

        return list1 == null || list1.size() == list2.size();
    }
}
