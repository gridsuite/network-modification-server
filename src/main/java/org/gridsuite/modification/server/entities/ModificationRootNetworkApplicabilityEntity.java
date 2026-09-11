/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Id;
import jakarta.persistence.IdClass;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serializable;
import java.util.UUID;

/**
 * Whether a modification is applied on the root network a tag names. A modification without an entry for a tag is
 * applicable on it, so only the tags a user has decided on are stored here.
 *
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Entity
@IdClass(ModificationRootNetworkApplicabilityEntity.ModificationRootNetworkApplicabilityId.class)
@Table(name = "modification_root_network_applicability")
public class ModificationRootNetworkApplicabilityEntity {

    // no index on modification_id: the (modification_id, root_network_tag) primary key already covers it
    @Id
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "modification_id", foreignKey = @ForeignKey(name = "modification_root_network_applicability_fk"))
    private ModificationEntity modification;

    @Id
    @Column(name = "root_network_tag", length = ModificationEntity.ROOT_NETWORK_TAG_MAX_LENGTH)
    private String rootNetworkTag;

    @Setter
    @Column(name = "applicable")
    private Boolean applicable;

    /**
     * The primary key of the table: a modification carries at most one applicability per root network tag.
     */
    @NoArgsConstructor
    @EqualsAndHashCode
    public static class ModificationRootNetworkApplicabilityId implements Serializable {
        private UUID modification;
        private String rootNetworkTag;
    }
}
