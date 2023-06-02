/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GenerationDispatchInfos;
import org.gridsuite.modification.server.dto.GeneratorsFilterInfos;
import org.gridsuite.modification.server.dto.GeneratorsFrequencyReserveInfos;

import java.util.List;
import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGenerationDispatchInfos extends MatcherModificationInfos<GenerationDispatchInfos> {
    protected MatcherGenerationDispatchInfos(GenerationDispatchInfos ref) {
        super(ref);
    }

    public static MatcherGenerationDispatchInfos createMatcherGenerationDispatchInfos(GenerationDispatchInfos generationDispatchInfos) {
        return new MatcherGenerationDispatchInfos(generationDispatchInfos);
    }

    private boolean matchesFilter(GeneratorsFilterInfos filter1, GeneratorsFilterInfos filter2) {
        return Objects.equals(filter1.getName(), filter2.getName()) &&
            Objects.equals(filter1.getId(), filter2.getId());
    }

    private boolean matchesGeneratorsFilters(List<GeneratorsFilterInfos> generatorsFilterInfos,
                                             List<GeneratorsFilterInfos> refGeneratorsFilterInfos) {
        if (!matchesList(refGeneratorsFilterInfos, generatorsFilterInfos)) {
            return false;
        }
        for (int index = 0; index < generatorsFilterInfos.size(); index++) {
            if (!matchesFilter(refGeneratorsFilterInfos.get(index), generatorsFilterInfos.get(index))) {
                return false;
            }
        }
        return true;
    }

    private boolean matchesFrequencyReserve(GeneratorsFrequencyReserveInfos generatorsFrequencyReserve,
                                            GeneratorsFrequencyReserveInfos refGeneratorsFrequencyReserve) {
        if (!Objects.equals(refGeneratorsFrequencyReserve.getFrequencyReserve(), generatorsFrequencyReserve.getFrequencyReserve())) {
            return false;
        }

        if (!matchesList(generatorsFrequencyReserve.getGeneratorsFilters(), refGeneratorsFrequencyReserve.getGeneratorsFilters())) {
            return false;
        }
        for (int index = 0; index < generatorsFrequencyReserve.getGeneratorsFilters().size(); index++) {
            if (!matchesFilter(generatorsFrequencyReserve.getGeneratorsFilters().get(index), refGeneratorsFrequencyReserve.getGeneratorsFilters().get(index))) {
                return false;
            }
        }
        return true;
    }

    private boolean matchesGeneratorsFrequencyReserve(List<GeneratorsFrequencyReserveInfos> generatorsFrequencyReserveInfos,
                                                      List<GeneratorsFrequencyReserveInfos> refGeneratorsFrequencyReserveInfos) {
        if (!matchesList(refGeneratorsFrequencyReserveInfos, generatorsFrequencyReserveInfos)) {
            return false;
        }
        for (int index = 0; index < generatorsFrequencyReserveInfos.size(); index++) {
            if (!matchesFrequencyReserve(refGeneratorsFrequencyReserveInfos.get(index), generatorsFrequencyReserveInfos.get(index))) {
                return false;
            }
        }
        return true;
    }

    public boolean matchesSafely(GenerationDispatchInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getLossCoefficient(), m.getLossCoefficient())
                && Objects.equals(reference.getDefaultOutageRate(), m.getDefaultOutageRate())
                && matchesGeneratorsFilters(m.getGeneratorsWithoutOutage(), reference.getGeneratorsWithoutOutage())
                && matchesGeneratorsFilters(m.getGeneratorsWithFixedSupply(), reference.getGeneratorsWithFixedSupply())
                && matchesGeneratorsFrequencyReserve(m.getGeneratorsFrequencyReserve(), reference.getGeneratorsFrequencyReserve());
    }

    private boolean matchesList(List<?> list1, List<?> list2) {
        if ((list1 == null) != (list2 == null)) {
            return false;
        }
        return list1 == null || list1.size() == list2.size();
    }
}
