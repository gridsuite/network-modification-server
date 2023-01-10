/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import java.util.List;
import java.util.Objects;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class MatcherLoadScalingInfos extends MatcherModificationInfos<LoadScalingInfos> {
    protected MatcherLoadScalingInfos(LoadScalingInfos ref) {
        super(ref);
    }

    public static MatcherLoadScalingInfos createMatcherLoadScalingInfos(LoadScalingInfos loadScalingInfos) {
        return new MatcherLoadScalingInfos(loadScalingInfos);
    }

    public boolean matchesSafely(LoadScalingInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getVariationType(), m.getVariationType())
                && matchesVariations(m.getVariations());

    }

    private boolean matchesVariations(List<ScalingVariationInfos> loadScalingVariations) {
        if (!matchesList(reference.getVariations(), loadScalingVariations)) {
            return false;
        }

        for (int index = 0; index < loadScalingVariations.size(); index++) {
            if (!matchesVariation(reference.getVariations().get(index),
                     loadScalingVariations.get(index))) {
                return false;
            }
        }

        return true;
    }

    private boolean matchesVariation(ScalingVariationInfos variation1, ScalingVariationInfos variation2) {
        return Objects.equals(variation1.getVariationValue(), variation2.getVariationValue())
                && Objects.equals(variation1.getVariationMode(), variation2.getVariationMode())
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
        return Objects.equals(filter1.getName(), filter2.getName()) &&
                Objects.equals(filter1.getId(), filter2.getId());
    }

    private boolean matchesList(List<?> list1, List<?> list2) {
        if ((list1 == null) != (list2 == null)) {
            return false;
        }

        return list1 == null || list1.size() == list2.size();
    }
}
