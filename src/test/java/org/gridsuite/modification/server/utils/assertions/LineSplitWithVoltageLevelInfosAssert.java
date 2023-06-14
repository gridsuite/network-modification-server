package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;

public class LineSplitWithVoltageLevelInfosAssert extends AbstractModificationInfosAssert<LineSplitWithVoltageLevelInfosAssert, LineSplitWithVoltageLevelInfos> {
    public LineSplitWithVoltageLevelInfosAssert(LineSplitWithVoltageLevelInfos actual) {
        super(actual, LineSplitWithVoltageLevelInfosAssert.class);
    }

    @Override
    protected RecursiveComparisonConfiguration recursiveConfiguration() {
        final RecursiveComparisonConfiguration configuration = super.recursiveConfiguration();
        configuration.registerEqualsForFields((Double d1, Double d2) -> Math.abs(d1 - d2) < 0.2d, "percent");
        return configuration;
    }
}
