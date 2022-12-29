package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.LoadScalingVariation;

import java.util.List;
import java.util.Objects;

public class MatcherLoadScalingInfos extends MatcherModificationInfos<LoadScalingInfos> {
    protected MatcherLoadScalingInfos(LoadScalingInfos ref) {
        super(ref);
    }

    public static MatcherLoadScalingInfos createMatcherLoadScalingInfos(LoadScalingInfos generatorScalingInfos) {
        return new MatcherLoadScalingInfos(generatorScalingInfos);
    }

    public boolean matchesSafely(LoadScalingInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getVariationType(), m.getVariationType())
                && matchesVariations(m.getLoadScalingVariations());

    }

    private boolean matchesVariations(List<LoadScalingVariation> generatorScalingVariations) {
        if (!matchesList(reference.getLoadScalingVariations(), generatorScalingVariations)) {
            return false;
        }

        for (int index = 0; index < generatorScalingVariations.size(); index++) {
            if (!matchesVariation(reference.getLoadScalingVariations().get(index),
                    generatorScalingVariations.get(index))) {
                return false;
            }
        }

        return true;
    }

    private boolean matchesVariation(LoadScalingVariation variation1, LoadScalingVariation variation2) {
        return Objects.equals(variation1.getVariationValue(), variation2.getVariationValue())
                && Objects.equals(variation1.getActiveVariationMode(), variation2.getActiveVariationMode())
                && Objects.equals(variation1.getReactiveVariationMode(), variation2.getReactiveVariationMode())
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
