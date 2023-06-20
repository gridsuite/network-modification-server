package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.ScalingInfos;

public class ScalingInfosAssert extends AbstractModificationInfosAssert<ScalingInfosAssert, ScalingInfos> {
    public ScalingInfosAssert(ScalingInfos actual) {
        super(actual, ScalingInfosAssert.class);
    }

    @Override
    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        final RecursiveComparisonConfiguration configuration = super.recursiveConfiguration();
        configuration.ignoreFields("variations.id");
        return configuration;
    }
}
