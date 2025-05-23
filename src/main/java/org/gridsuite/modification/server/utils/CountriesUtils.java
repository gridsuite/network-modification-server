/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.iidm.network.Country;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class CountriesUtils {
    public static final String DELIMITER = ",";

    private CountriesUtils() {
        // Should not be instantiated
    }

    public static String stringify(List<Country> countries) {
        return countries
            .stream()
            .map(Enum::name)
            .collect(Collectors.joining(DELIMITER));
    }

    public static List<Country> toList(String countries) {
        if (countries.isEmpty()) {
            return List.of();
        }
        return Arrays.stream(countries.split(DELIMITER)).map(Country::valueOf).toList();
    }
}
