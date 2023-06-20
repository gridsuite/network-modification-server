package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.ModificationInfos;

import java.time.temporal.Temporal;
import java.util.Objects;
import java.util.UUID;

/**
 * Base implementation of AssertJ assertions for all subclass of {@link ModificationInfos}
 * @param <SELF>
 * @param <ACTUAL>
 */
public abstract class AbstractModificationInfosAssert<SELF extends AbstractModificationInfosAssert<SELF, ACTUAL>, ACTUAL extends ModificationInfos>
    extends AbstractAssert<SELF, ACTUAL> {
    protected AbstractModificationInfosAssert(ACTUAL actual, Class<SELF> assertClass) {
        super(actual, assertClass);
    }

    public SELF hasUuid(UUID uuid) {
        isNotNull();
        if (!Objects.equals(actual.getUuid(), uuid)) {
            failWithMessage("Expected Modification UUID to be <%s> but was <%s>", uuid, actual.getUuid());
        }
        return myself;
    }

    public SELF hasDate(Temporal dateTime) {
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
    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        return RecursiveComparisonConfiguration.builder()
            .withIgnoreAllOverriddenEquals(true)
            .withIgnoredFieldsMatchingRegexes("^(\\w+\\.)?date$", "^(\\w+\\.)?uuid$")
            .withIgnoreCollectionOrder(false) //maybe the order is not needed on some types
            .build();
    }

    public SELF recursivelyEquals(ModificationInfos other) {
        isNotNull();
        usingRecursiveComparison(this.recursiveConfiguration()).isEqualTo(other);
        return myself;
    }
}
