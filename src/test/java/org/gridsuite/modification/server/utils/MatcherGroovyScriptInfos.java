/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GroovyScriptInfos;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGroovyScriptInfos extends MatcherModificationInfos<GroovyScriptInfos> {
    public MatcherGroovyScriptInfos(String script) {
        this(GroovyScriptInfos.builder()
                .date(ZonedDateTime.now(ZoneOffset.UTC))
                .script(script)
                .build());
    }

    public MatcherGroovyScriptInfos(GroovyScriptInfos ref) {
        super(ref);
    }

    @Override
    public boolean matchesSafely(GroovyScriptInfos m) {
        return super.matchesSafely(m)
            && Objects.equals(m.getScript(), getReference().getScript());
    }
}
