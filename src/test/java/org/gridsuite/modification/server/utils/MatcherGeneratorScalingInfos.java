package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.FilterInfo;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;

import java.util.Objects;

public class MatcherGeneratorScalingInfos extends MatcherModificationInfos<GeneratorScalingInfos>{
    protected MatcherGeneratorScalingInfos(GeneratorScalingInfos ref) {
        super(ref);
    }

    public static MatcherGeneratorScalingInfos createMatcherGeneratorScalingInfos(GeneratorScalingInfos generatorScalingInfos) {
        return new MatcherGeneratorScalingInfos(generatorScalingInfos);
    }

    public boolean matchesSafely(GeneratorScalingInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getVariationType(), m.getVariationType())
                && Objects.equals(reference.isIterative(), m.isIterative());
    }

    private boolean matchesVariation(GeneratorScalingVariation variation1, GeneratorScalingVariation variation2) {
        return Objects.equals(variation1.getVariationValue(), variation2.getVariationValue())
                && Objects.equals(variation1.getVariationMode(), variation2.getVariationMode())
                && (variation1.getFilters() == null && variation2.getFilters() == null);
    }

    private boolean matchesFilters(FilterInfo filter1, FilterInfo filter2) {
        return Objects.equals(filter1.getId(), filter2.getId())
                && Objects.equals(filter1.getName(), filter2.getName());
    }
}
