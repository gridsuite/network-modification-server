/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.GroovyScriptInfos;
import org.gridsuite.modification.dto.ModificationInfos;

import jakarta.persistence.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "groovyScriptModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "groovyScriptModification_id_fk_constraint"))
public class GroovyScriptEntity extends ModificationEntity {
    @Column(name = "script", columnDefinition = "CLOB")
    private String script;

    public GroovyScriptEntity(GroovyScriptInfos groovyScriptInfos) {
        super(groovyScriptInfos);
        assignAttributes(groovyScriptInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((GroovyScriptInfos) modificationInfos);
    }

    private void assignAttributes(GroovyScriptInfos modificationInfos) {
        this.script = modificationInfos.getScript();
    }

    @Override
    public GroovyScriptInfos toModificationInfos() {
        return toGroovyScriptInfosBuilder().build();
    }

    private GroovyScriptInfos.GroovyScriptInfosBuilder<?, ?> toGroovyScriptInfosBuilder() {
        return GroovyScriptInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .script(getScript());
    }
}
