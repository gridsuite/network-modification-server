package org.gridsuite.modification.server.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
public class FilterInfo {

    public FilterInfo(String id) {
        this.id = id;
    }

    private String id;
    private String name;
}
