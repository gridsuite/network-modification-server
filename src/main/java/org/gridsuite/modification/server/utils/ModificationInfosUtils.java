/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;

import java.util.List;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public final class ModificationInfosUtils {
    private ModificationInfosUtils() {
        // Should not be instantiated
    }

    /**
     * @return what a modification holds: the content of a composite, and the shared modification a reference points
     * to. The content of a tabular is left out: it holds modifications of a single type, which is never a composite
     * nor a reference.
     */
    public static List<ModificationInfos> contentOf(ModificationInfos modificationInfos) {
        if (modificationInfos instanceof CompositeModificationInfos composite) {
            return composite.getModificationsInfos() == null ? List.of() : composite.getModificationsInfos();
        }
        if (modificationInfos instanceof ModificationReferenceInfos reference && reference.getReferencedInfos() != null) {
            return List.of(reference.getReferencedInfos());
        }
        return List.of();
    }
}
