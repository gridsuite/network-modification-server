/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public class TabularModificationRepositoryException extends RuntimeException {

    public TabularModificationRepositoryException(String message) {
        super(message);
    }
}
