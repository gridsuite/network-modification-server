/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public class BuildException extends RuntimeException {
    public BuildException(String message, Throwable e) {
        super(message, e);
    }
}
