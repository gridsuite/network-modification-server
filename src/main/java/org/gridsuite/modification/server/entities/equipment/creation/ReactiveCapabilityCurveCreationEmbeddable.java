package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Embeddable;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ReactiveCapabilityCurveCreationEmbeddable {
    @Column
    private Double qminP;

    @Column
    private Double qmaxP;

    @Column
    private Double p;
}
