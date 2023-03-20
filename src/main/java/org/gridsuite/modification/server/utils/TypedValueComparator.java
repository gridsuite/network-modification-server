/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import com.powsybl.commons.reporter.TypedValue;

import java.util.Comparator;
import java.util.List;

public class TypedValueComparator implements Comparator<TypedValue> {

    private static List<TypedValue> orderedTypedValues = List.of(
            TypedValue.TRACE_SEVERITY,
            TypedValue.DEBUG_SEVERITY,
            TypedValue.INFO_SEVERITY,
            TypedValue.WARN_SEVERITY,
            TypedValue.ERROR_SEVERITY
    );

    @Override
    public int compare(TypedValue firstTypedValue, TypedValue secondTypedValue) {
        return Integer.compare(orderedTypedValues.indexOf(firstTypedValue), orderedTypedValues.indexOf(secondTypedValue));
    }
}
