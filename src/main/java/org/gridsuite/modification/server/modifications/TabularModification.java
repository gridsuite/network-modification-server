package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.dto.TabularModificationInfos;

public class TabularModification extends AbstractModification {
    private final TabularModificationInfos modificationInfos;

    public TabularModification(TabularModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

}
