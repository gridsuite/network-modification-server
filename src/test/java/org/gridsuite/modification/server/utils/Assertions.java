package org.gridsuite.modification.server.utils;

import org.assertj.core.util.CheckReturnValue;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.assertions.*;

import java.util.UUID;

/**
 * {@link org.assertj.core.api.Assertions Assertions} completed with our custom assertions classes.
 */
public class Assertions extends org.assertj.core.api.Assertions {
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
    public static <T extends ModificationInfos> ModificationInfosAssert<T> assertThat(T actual) {
        return new ModificationInfosAssert<>(actual);
    }

    /**
     * Create assertion for {@link NetworkModificationResult}.
     *
     * @param actual the actual value.
     * @return the created assertion object.
     */
    @CheckReturnValue
    public static NetworkModificationResultAssert assertThat(NetworkModificationResult actual) {
        return new NetworkModificationResultAssert(actual);
    }
}
