package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;

public class TwoWindingsTransformerCreationInfosAssert extends AbstractModificationInfosAssert<TwoWindingsTransformerCreationInfosAssert, TwoWindingsTransformerCreationInfos> {
    public TwoWindingsTransformerCreationInfosAssert(TwoWindingsTransformerCreationInfos actual) {
        super(actual, TwoWindingsTransformerCreationInfosAssert.class);
    }

    @Override
    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        final RecursiveComparisonConfiguration configuration = super.recursiveConfiguration();
        configuration.ignoreCollectionOrderInFields("phaseTapChanger.steps");
        return configuration;
    }
}
