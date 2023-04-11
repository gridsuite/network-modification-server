/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GroovyScriptInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGroovyScriptInfos extends MatcherModificationInfos<GroovyScriptInfos> {

    public static MatcherGroovyScriptInfos createMatcherGroovyScriptInfos(String script) {
        return new MatcherGroovyScriptInfos(GroovyScriptInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .script(script)
                .build());
    }

    public static MatcherGroovyScriptInfos createMatcherGroovyScriptInfos(GroovyScriptInfos groovyScriptInfos) {
        return new MatcherGroovyScriptInfos(groovyScriptInfos);
    }

    protected MatcherGroovyScriptInfos(GroovyScriptInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(GroovyScriptInfos m) {
        return super.matchesSafely(m)
            && m.getScript().equals(reference.getScript());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
