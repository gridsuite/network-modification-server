package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

public class VoltageLevelCreationInfosAssert extends AbstractModificationInfosAssert<VoltageLevelCreationInfosAssert, VoltageLevelCreationInfos> {
    public VoltageLevelCreationInfosAssert(VoltageLevelCreationInfos actual) {
        super(actual, VoltageLevelCreationInfosAssert.class);
    }

    @Override
    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        final RecursiveComparisonConfiguration configuration = super.recursiveConfiguration();
        configuration.ignoreCollectionOrderInFields("switchKinds");
        configuration.registerEqualsForFields((Double d1, Double d2) -> Math.abs(d1 - d2) < 0.2d, "nominalVoltage");
        configuration.ignoreFields("lowVoltageLimit", "highVoltageLimit",
                                   "ipMin", "ipMax",
                                   "busbarCount", "sectionCount"
        ); //these fields weren't compared in the old mather
        return configuration;
    }
}
