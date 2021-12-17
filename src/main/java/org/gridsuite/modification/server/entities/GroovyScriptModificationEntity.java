/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GroovyScriptModificationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "groovyScriptModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "groovyScriptModification_id_fk_constraint"))
public class GroovyScriptModificationEntity extends ModificationEntity {
    @Column(name = "script", columnDefinition = "CLOB")
    private String script;

    public GroovyScriptModificationEntity(String script) {
        super(ModificationType.GROOVY_SCRIPT);
        this.script = script;
    }

    @Override
    public GroovyScriptModificationInfos toModificationInfos() {
        return toGroovyScriptModificationInfosBuilder().build();
    }

    @Override
    public GroovyScriptModificationInfos toModificationInfos(Set<String> uuids) {
        return toGroovyScriptModificationInfosBuilder().substationIds(uuids).build();
    }

    private GroovyScriptModificationInfos.GroovyScriptModificationInfosBuilder<?, ?> toGroovyScriptModificationInfosBuilder() {
        return GroovyScriptModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .script(getScript());
    }
}
