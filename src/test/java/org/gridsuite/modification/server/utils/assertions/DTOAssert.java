/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils.assertions;

import org.assertj.core.api.AbstractAssert;
import org.assertj.core.api.recursive.comparison.RecursiveComparisonConfiguration;

import java.time.Instant;
import java.util.Date;
import java.util.Objects;
import java.util.UUID;

/**
 *  @author Tristan Chuine <tristan.chuine at rte-france.com>
 *  @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class DTOAssert<T> extends AbstractAssert<DTOAssert<T>, T> {
    public DTOAssert(T actual) {
        super(actual, DTOAssert.class);
    }

    public DTOAssert<T> recursivelyEquals(T other) {
        isNotNull();
        usingRecursiveComparison(getRecursiveConfiguration()).isEqualTo(other);
        return myself;
    }

    public static RecursiveComparisonConfiguration getRecursiveConfiguration() {
        return getRecursiveConfiguration(true);
    }

    public static RecursiveComparisonConfiguration getRecursiveConfiguration(boolean ignoreCollectionOrder) {
        RecursiveComparisonConfiguration.Builder builder = RecursiveComparisonConfiguration.builder()
            .withIgnoreAllOverriddenEquals(true)                                    // For equals test, need specific tests
            .withIgnoredFieldsOfTypes(UUID.class, Date.class, Instant.class)        // For these types, need specific tests (uuid from db for example)
            // DTO builders may leave activated null while it's set to true at creation
            .withEqualsForFieldsMatchingRegexes(DTOAssert::activationFlagsAreEqualWithDefaultValue, ".*activated");
        if (ignoreCollectionOrder) {
            builder.withIgnoreCollectionOrder(true);                                // For collection order test, need specific tests
        }
        return builder.build();
    }

    private static boolean activationFlagsAreEqualWithDefaultValue(Object actual, Object expected) {
        return Objects.equals(defaultActivation(actual), defaultActivation(expected));
    }

    private static Object defaultActivation(Object value) {
        return value == null ? true : value;
    }
}
