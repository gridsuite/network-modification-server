package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.dto.LimitsPropertyInfos;

import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "limits_property")
public class LimitsPropertyEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "value", nullable = false)
    private String value;

    public static LimitsPropertyEntity fromLimitsPropertyInfos(LimitsPropertyInfos propertyInfos) {
        return new LimitsPropertyEntity(UUID.randomUUID(), propertyInfos.name(), propertyInfos.value());
    }

    public LimitsPropertyInfos toCurrentLimitsInfos() {
        return new LimitsPropertyInfos(name, value);
    }
}
