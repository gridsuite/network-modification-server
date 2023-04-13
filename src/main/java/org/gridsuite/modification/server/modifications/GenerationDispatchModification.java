/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.modification.generation.dispatch.GenerationDispatch;
import com.powsybl.iidm.modification.generation.dispatch.GenerationDispatchBuilder;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GenerationDispatchInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATION_DISPATCH_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GenerationDispatchModification extends AbstractModification {

    private final GenerationDispatchInfos generationDispatchInfos;

    public GenerationDispatchModification(GenerationDispatchInfos generationDispatchInfos) {
        this.generationDispatchInfos = generationDispatchInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        double lossCoefficient = generationDispatchInfos.getLossCoefficient();
        if (lossCoefficient < 0. || lossCoefficient > 100.) {
            throw new NetworkModificationException(GENERATION_DISPATCH_ERROR, "The loss coefficient must be between 0 and 100");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        GenerationDispatch generationDispatch = new GenerationDispatchBuilder().withLossCoefficient(generationDispatchInfos.getLossCoefficient()).build();
        generationDispatch.apply(network, subReporter);
    }
}
