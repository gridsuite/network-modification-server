package org.gridsuite.modification.server.entities.equipment.modification.byfilter;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.dto.BySimpleModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationByFilterInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.SimpleModificationEntity;

import java.util.List;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "bySimpleModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "bySimpleModification_id_fk_constraint"))
public class BySimpleModificationEntity extends ModificationEntity {
    @Column
    private IdentifiableType identifiableType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "by_simple_modification_id",
            foreignKey = @ForeignKey(name = "by_simple_modification_id_fk"))
    private List<SimpleModificationEntity> simpleModificationEntities;

    public BySimpleModificationEntity(BySimpleModificationInfos bySimpleModificationInfos) {
        super(bySimpleModificationInfos);
        assignAttributes(bySimpleModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BySimpleModificationInfos) modificationInfos);
    }

    private void assignAttributes(BySimpleModificationInfos bySimpleModificationInfos) {
        this.identifiableType = bySimpleModificationInfos.getIdentifiableType();
        if (simpleModificationEntities == null) {
            simpleModificationEntities = bySimpleModificationInfos.getSimpleModificationInfosList()
                    .stream()
                    .map(AbstractSimpleModificationByFilterInfos::toEntity)
                    .toList();
        } else {
            simpleModificationEntities.clear();
            simpleModificationEntities.addAll(bySimpleModificationInfos.getSimpleModificationInfosList()
                    .stream()
                    .map(AbstractSimpleModificationByFilterInfos::toEntity)
                    .toList());
        }
    }

    @Override
    public BySimpleModificationInfos toModificationInfos() {
        return toBySimpleModificationInfosBuilder().build();
    }

    private BySimpleModificationInfos.BySimpleModificationInfosBuilder<?, ?> toBySimpleModificationInfosBuilder() {
        return BySimpleModificationInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .identifiableType(getIdentifiableType())
            .simpleModificationInfosList(simpleModificationEntities.stream()
                .map(SimpleModificationEntity::toSimpleModificationInfos)
                .toList()
            );
    }
}
