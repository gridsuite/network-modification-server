/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GroovyScriptModificationInfos;
import org.hamcrest.Description;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGroovyScriptModificationInfos extends MatcherModificationInfos<GroovyScriptModificationInfos> {

    public static MatcherGroovyScriptModificationInfos createMatcherGroovyScriptModificationInfos(String script,
                                                                                                  Set<String> substationIds) {
        return new MatcherGroovyScriptModificationInfos(GroovyScriptModificationInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .type(ModificationType.GROOVY_SCRIPT)
                .script(script)
                .substationIds(substationIds)
                .build());
    }

    public static MatcherGroovyScriptModificationInfos createMatcherGroovyScriptModificationInfos(GroovyScriptModificationInfos groovyScriptModificationInfos) {
        return new MatcherGroovyScriptModificationInfos(groovyScriptModificationInfos);
    }

    protected MatcherGroovyScriptModificationInfos(GroovyScriptModificationInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(GroovyScriptModificationInfos m) {
        return super.matchesSafely(m)
            && m.getScript().equals(reference.getScript())
            && m.getSubstationIds().equals(reference.getSubstationIds());
    }

    @Override
    public void describeTo(Description description) {
        description.appendValue(reference);
    }
}
