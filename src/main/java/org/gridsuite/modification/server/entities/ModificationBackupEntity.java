package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@Embeddable
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name = "modification_backup")
public class ModificationBackupEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private Long id;

    @Column(name = "network_uuid")
    UUID networkUuid;

    @Column(name = "index_infos", columnDefinition = "CLOB")
    String indexInfos;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "modification_uuid", foreignKey = @ForeignKey(name = "modification_uuid_fk_constraint"))
    ModificationEntity modification;

    public void setModification(ModificationEntity modification) {
//        modification.getModificationBackups().add(this);
        this.modification = modification;
    }
}
