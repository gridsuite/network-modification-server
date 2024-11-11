package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.TabularCreationInfos;
import org.gridsuite.modification.server.entities.TabularCreationEntity;

public class TabularCreationMapper extends ModificationMapper<TabularCreationInfos, TabularCreationEntity> {
    public TabularCreationMapper() {
        super(TabularCreationEntity.class, TabularCreationInfos.class);
    }
}
