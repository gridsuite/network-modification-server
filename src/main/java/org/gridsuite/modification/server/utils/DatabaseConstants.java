/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

public final class DatabaseConstants {
    public static final int SQL_SUB_MODIFICATION_SAVE_BATCH_SIZE = 5000;
    public static final int SQL_SUB_MODIFICATION_WITH_LIMITSET_DELETION_BATCH_SIZE = 2000;
    public static final int SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE = 5000;

    private DatabaseConstants() {
        // Should not be instantiated
    }
}
