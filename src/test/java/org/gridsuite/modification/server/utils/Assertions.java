package org.gridsuite.modification.server.utils;

import org.assertj.core.util.CheckReturnValue;
import org.gridsuite.modification.server.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.assertions.AbstractModificationInfosAssert;
import org.gridsuite.modification.server.utils.assertions.LineAttachToVoltageLevelInfosAssert;
import org.gridsuite.modification.server.utils.assertions.LineSplitWithVoltageLevelInfosAssert;
import org.gridsuite.modification.server.utils.assertions.ModificationInfosAssert;
import org.gridsuite.modification.server.utils.assertions.TwoWindingsTransformerCreationInfosAssert;
import org.gridsuite.modification.server.utils.assertions.VoltageLevelCreationInfosAssert;

import java.util.UUID;

public class Assertions extends org.assertj.core.api.Assertions {
    /* *
     * Create assertion for {@link ModificationInfos}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     * /
    @CheckReturnValue
    public static ModificationInfosAssert assertThat(ModificationInfos actual) {
            return new ModificationInfosAssert(actual);
    }*/

    /**
     * Create assertion for {@link ModificationInfos}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     *
     * @implNote because of {@link org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest#testUpdate() AbstractNetworkModificationTest#testUpdate()}
     *      limitation due to the generic return type of {@link NetworkModificationRepository#getModifications(UUID, boolean, boolean)}
     *      who use {@link ModificationInfos#toEntity()}, the {@literal test{Create,Read,Update,Delete,Copy}} tests cannot
     *      use the specialized {@literal Assertions#assertThat(*Type)} assertion.
     */
    @CheckReturnValue
    public static AbstractModificationInfosAssert<?, ?> assertThat(ModificationInfos actual) {
        //we test type at runtime for commons test in AbstractNetworkModificationTest, not good but work
        if (actual instanceof LineAttachToVoltageLevelInfos) {
            return assertThat((LineAttachToVoltageLevelInfos) actual);
        } else if (actual instanceof LineSplitWithVoltageLevelInfos) {
            return assertThat((LineSplitWithVoltageLevelInfos) actual);
        } else if (actual instanceof VoltageLevelCreationInfos) {
            return assertThat((VoltageLevelCreationInfos) actual);
        } else if (actual instanceof TwoWindingsTransformerCreationInfos) {
            return assertThat((TwoWindingsTransformerCreationInfos) actual);
        } else {
            return new ModificationInfosAssert(actual);
        }
    }

    /**
     * Create assertion for {@link LineAttachToVoltageLevelInfos}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     */
    @CheckReturnValue
    public static LineAttachToVoltageLevelInfosAssert assertThat(LineAttachToVoltageLevelInfos actual) {
        return new LineAttachToVoltageLevelInfosAssert(actual);
    }

    /**
     * Create assertion for {@link LineSplitWithVoltageLevelInfos}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     */
    @CheckReturnValue
    public static LineSplitWithVoltageLevelInfosAssert assertThat(LineSplitWithVoltageLevelInfos actual) {
        return new LineSplitWithVoltageLevelInfosAssert(actual);
    }

    /**
     * Create assertion for {@link VoltageLevelCreationInfos}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     */
    @CheckReturnValue
    public static VoltageLevelCreationInfosAssert assertThat(VoltageLevelCreationInfos actual) {
        return new VoltageLevelCreationInfosAssert(actual);
    }

    /**
     * Create assertion for {@link TwoWindingsTransformerCreationInfos}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     */
    @CheckReturnValue
    public static TwoWindingsTransformerCreationInfosAssert assertThat(TwoWindingsTransformerCreationInfos actual) {
        return new TwoWindingsTransformerCreationInfosAssert(actual);
    }
}
