package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
public class VoltageLevelCreation extends AbstractModification {

    private final VoltageLevelCreationInfos modificationInfos;

    public VoltageLevelCreation(VoltageLevelCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        ModificationUtils.getInstance().createVoltageLevelAction(modificationInfos, subReporter, network);
    }
}
