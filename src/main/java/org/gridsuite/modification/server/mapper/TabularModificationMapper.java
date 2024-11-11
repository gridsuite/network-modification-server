package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.TabularModificationInfos;
import org.gridsuite.modification.server.entities.TabularModificationEntity;

public class TabularModificationMapper extends ModificationMapper<TabularModificationInfos, TabularModificationEntity> {
    public TabularModificationMapper() {
        super(TabularModificationEntity.class, TabularModificationInfos.class);
    }
}
