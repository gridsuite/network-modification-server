package org.gridsuite.modification.server;

import com.google.common.collect.ImmutableSet;
import org.gridsuite.modification.server.dto.GroovyScriptResult;
import org.junit.Test;

import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class GroovyScriptResultTest {

    @Test
    public void test() {
        Set<String> modifications = ImmutableSet.of("s1");
        GroovyScriptResult result = new GroovyScriptResult(true, modifications);
        assertTrue(result.isOk());
        assertEquals(modifications, result.getModifications());
    }
}
