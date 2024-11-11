package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.GroovyScriptInfos;
import org.gridsuite.modification.server.entities.GroovyScriptEntity;

public class GroovyScriptMapper extends ModificationMapper<GroovyScriptInfos, GroovyScriptEntity> {
    public GroovyScriptMapper() {
        super(GroovyScriptEntity.class, GroovyScriptInfos.class);
    }
}
