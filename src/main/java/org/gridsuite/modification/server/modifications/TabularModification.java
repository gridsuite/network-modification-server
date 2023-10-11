package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.TabularModificationInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.TABULAR_MODIFICATION_ERROR;

public class TabularModification extends AbstractModification {
    private final TabularModificationInfos modificationInfos;

    public TabularModification(TabularModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (modificationInfos == null) {
            throw new NetworkModificationException(TABULAR_MODIFICATION_ERROR, "No tabular modification to apply !!");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        modificationInfos.getModifications().forEach(modificationInfos -> {
            modificationInfos.toModification().apply(network);
//            modificationInfos.toModification().apply(network, subReporter);
        });
    }
}
