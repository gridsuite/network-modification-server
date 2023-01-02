package org.gridsuite.modification.server.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.util.Date;
import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class FilterAttributes {
    private String id;
    private Date modificationDate;
    private String equipmentType;
    private List<FilterEquipmentAttributes> filterEquipmentsAttributes;
}
