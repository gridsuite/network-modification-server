/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.google.common.collect.ImmutableSet;
import org.gridsuite.modification.server.dto.GroovyScriptResult;
import org.junit.Test;

import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GroovyScriptResultTest {

    @Test
    public void test() {
        Set<String> modifications = ImmutableSet.of("s1");
        GroovyScriptResult result = new GroovyScriptResult(true, modifications);
        assertTrue(result.isOk());
        assertEquals(modifications, result.getModifications());
    }
}
