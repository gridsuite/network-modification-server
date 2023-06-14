package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.Assertions;
import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.ModificationInfos;

import java.time.temporal.Temporal;
import java.util.Objects;
import java.util.UUID;

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

    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        return RecursiveComparisonConfiguration.builder()
            .withIgnoreAllOverriddenEquals(true)
            .withIgnoredFieldsMatchingRegexes("^(\\w+\\.)?date$", "^(\\w+\\.)?uuid$")
            .withIgnoredFieldsOfTypes(UUID.class)
            .withIgnoreCollectionOrder(false) //maybe the order is not needed on some types
            .build();
    }

    public SELF recursivelyEquals(ModificationInfos other) {
        isNotNull();
        usingRecursiveComparison(this.recursiveConfiguration()).isEqualTo(other);
        return myself;
    }
}
