package org.gridsuite.modification.server.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class FilterInfos {
    private String id;
    private List<FilterEquipmentAttributes> equipments;
}
