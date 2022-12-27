package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterInfo;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherGeneratorScalingInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertEquals;

public class GeneratorScalingModificationTest extends AbstractNetworkModificationTest{
    private static final UUID GENERATOR_SCALING_ID = UUID.randomUUID();
    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfo.builder()
                .id("filter1Id")
                .name("filter 1")
                .build();

        var filter2 = FilterInfo.builder()
                .id("filter2Id")
                .name("filter 2")
                .build();

        var filter3 = FilterInfo.builder()
                .id("filter2Id")
                .name("filter 2")
                .build();

        var variation1 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.PROPORTIONAL_TO_PMAX)
                .variationValue(100D)
                .filters(List.of(filter1, filter2))
                .build();

        var variation2 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(50D)
                .filters(List.of(filter1, filter3))
                .build();

        var variation3 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.STACKING_UP)
                .variationValue(200D)
                .filters(List.of(filter1, filter2, filter3
                ))
                .build();

        var variation4 = GeneratorScalingVariation.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(150D)
                .filters(List.of(filter1))
                .build();

        return GeneratorScalingInfos.builder()
                .uuid(GENERATOR_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(true)
                .variationType(VariationType.DELTA_P)
                .generatorScalingVariations(List.of(variation1, variation2, variation3, variation4))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return null;
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGeneratorScalingInfos.createMatcherGeneratorScalingInfos((GeneratorScalingInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(getNetwork().getGenerator(GENERATOR_SCALING_ID.toString()).getTargetP(), 34, 0);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(getNetwork().getGenerator(GENERATOR_SCALING_ID.toString()).getTargetP(), 20, 0);
    }
}
