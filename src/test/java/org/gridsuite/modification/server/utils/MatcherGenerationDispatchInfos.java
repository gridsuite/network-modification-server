/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.GenerationDispatchInfos;

import java.util.Objects;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class MatcherGenerationDispatchInfos extends MatcherModificationInfos<GenerationDispatchInfos> {
    protected MatcherGenerationDispatchInfos(GenerationDispatchInfos ref) {
        super(ref);
    }

    public static MatcherGenerationDispatchInfos createMatcherGenerationDispatchInfos(GenerationDispatchInfos generationDispatchInfos) {
        return new MatcherGenerationDispatchInfos(generationDispatchInfos);
    }

    public boolean matchesSafely(GenerationDispatchInfos m) {
        return super.matchesSafely(m)
                && Objects.equals(reference.getLossCoefficient(), m.getLossCoefficient());
    }
}
