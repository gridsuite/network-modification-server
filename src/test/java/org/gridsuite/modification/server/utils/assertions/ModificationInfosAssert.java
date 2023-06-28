package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.*;

import java.time.temporal.Temporal;
import java.util.Objects;
import java.util.UUID;

/**
 * Base implementation of AssertJ assertions for all subclass of {@link ModificationInfos}
 */
public class ModificationInfosAssert<T extends ModificationInfos> extends AbstractAssert<ModificationInfosAssert<T>, T> {
    public ModificationInfosAssert(T actual) {
        super(actual, ModificationInfosAssert.class);
    }

    public ModificationInfosAssert<T> hasUuid(UUID uuid) {
        isNotNull();
        if (!Objects.equals(actual.getUuid(), uuid)) {
            failWithMessage("Expected Modification UUID to be <%s> but was <%s>", uuid, actual.getUuid());
        }
        return myself;
    }

    public ModificationInfosAssert<T> hasDate(Temporal dateTime) {
        isNotNull();
        Assertions.assertThat(actual.getDate())
                  .withFailMessage("Expected Modification date-time to be <%s> but was <%s>", dateTime, actual.getDate())
                  .isEqualTo(dateTime);
        return myself;
    }

    /**
     * Configuration for the comparison of all fields recursively, except some fields.
     * We don't use the {@code .equals(Object)} function because we need to ignore some fields,
     *   and we don't want to test the {@literal equals()} implementation but the DTO content.
     * The configuration can be completed by subtypes.
     * @return the comparison configuration
     */
    @SuppressWarnings("ResultOfMethodCallIgnored")
    protected RecursiveComparisonConfiguration recursiveConfiguration(final Object obj) {
        final RecursiveComparisonConfiguration.Builder configuration = RecursiveComparisonConfiguration.builder()
            .withIgnoreAllOverriddenEquals(true)
            .withIgnoredFieldsMatchingRegexes("^(\\w+\\.)?date$", "^(\\w+\\.)?uuid$") //simply ignore fields "date" and "uuid"
            .withIgnoreCollectionOrder(false); //maybe the order is not needed on some types
        /* we aso have some specific rules */
        //we test type at runtime for commons test in AbstractNetworkModificationTest, not good but work
        if ((obj instanceof LineAttachToVoltageLevelInfos) || (obj instanceof LineSplitWithVoltageLevelInfos)) {
            configuration.withEqualsForFields((Double d1, Double d2) -> Math.abs(d1 - d2) < 0.2d, "percent");
        } else if (obj instanceof ScalingInfos) {
            configuration.withIgnoredFields("variations.id");
        } else if (obj instanceof TwoWindingsTransformerCreationInfos) {
            configuration.withIgnoredCollectionOrderInFields("phaseTapChanger.steps");
        } else if (obj instanceof VoltageLevelCreationInfos) {
            configuration.withIgnoredCollectionOrderInFields("switchKinds");
            configuration.withEqualsForFields((Double d1, Double d2) -> Math.abs(d1 - d2) < 0.2d, "nominalVoltage");
            configuration.withIgnoredFields("lowVoltageLimit", "highVoltageLimit",
                                            "ipMin", "ipMax",
                                            "busbarCount", "sectionCount"
            ); //these fields weren't compared in the old mather
        }
        return configuration.build();
    }

    public ModificationInfosAssert<T> recursivelyEquals(Object other) {
        isNotNull();
        usingRecursiveComparison(this.recursiveConfiguration(other)).isEqualTo(other);
        return myself;
    }
}
