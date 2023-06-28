package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.NetworkModificationResult;

public class NetworkModificationResultAssert extends AbstractAssert<NetworkModificationResultAssert, NetworkModificationResult> {
    public NetworkModificationResultAssert(NetworkModificationResult actual) {
        super(actual, NetworkModificationResultAssert.class);
    }

    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        return RecursiveComparisonConfiguration.builder()
           .withIgnoreAllOverriddenEquals(true)
           .withIgnoredFieldsMatchingRegexes("^(\\w+\\.)?date$", "^(\\w+\\.)?uuid$")
           .withIgnoredCollectionOrderInFields("networkImpacts", "networkImpacts.substationIds")
           .build();
    }

    public NetworkModificationResultAssert recursivelyEquals(NetworkModificationResult other) {
        isNotNull();
        usingRecursiveComparison(this.recursiveConfiguration()).isEqualTo(other);
        return myself;
    }
}
