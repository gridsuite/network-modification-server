/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import java.util.List;
import java.util.Objects;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class MatcherGeneratorScalingInfos extends MatcherModificationInfos<GeneratorScalingInfos> {
    public MatcherGeneratorScalingInfos(GeneratorScalingInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(GeneratorScalingInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(getReference().getVariationType(), m.getVariationType())
                && matchesVariations(m.getVariations());
    }

    private boolean matchesVariations(List<ScalingVariationInfos> generatorScalingVariations) {
        if (!matchesList(getReference().getVariations(), generatorScalingVariations)) {
            return false;
        }

        for (int index = 0; index < generatorScalingVariations.size(); index++) {
            if (!matchesVariation(getReference().getVariations().get(index),
                    generatorScalingVariations.get(index))) {
                return false;
            }
        }

        return true;
    }

    private static boolean matchesVariation(ScalingVariationInfos variation1, ScalingVariationInfos variation2) {
        return Objects.equals(variation1.getVariationValue(), variation2.getVariationValue())
                && Objects.equals(variation1.getVariationMode(), variation2.getVariationMode())
                && matchesFilters(variation1.getFilters(), variation2.getFilters());
    }

    private static boolean matchesFilters(List<FilterInfos> filterList1, List<FilterInfos> filterList2) {
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

    private static boolean matchesFilter(FilterInfos filter1, FilterInfos filter2) {
        return Objects.equals(filter1.getName(), filter2.getName()) &&
               Objects.equals(filter1.getId(), filter2.getId());
    }

    private static boolean matchesList(List<?> list1, List<?> list2) {
        if ((list1 == null) != (list2 == null)) {
            return false;
        }

        return list1 == null || list1.size() == list2.size();
    }
}
