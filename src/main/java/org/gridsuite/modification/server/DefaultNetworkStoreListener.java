package org.gridsuite.modification.server;

import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Injection;
import com.powsybl.iidm.network.NetworkListener;
import com.powsybl.iidm.network.Switch;

import java.util.HashSet;
import java.util.Set;

public class DefaultNetworkStoreListener implements NetworkListener {

    private Set<String> substationsIds = new HashSet<>();  // set of substations modified

    private void storeModification(Identifiable identifiable) {
        if (identifiable instanceof Switch) {
            substationsIds.add(((Switch) identifiable).getVoltageLevel().getSubstation().getId());
        } else if (identifiable instanceof Injection) {
            substationsIds.add(((Injection) identifiable).getTerminal().getVoltageLevel().getSubstation().getId());
        }
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, Object oldValue, Object newValue) {
        storeModification(identifiable);
    }

    @Override
    public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
        storeModification(identifiable);
    }

    @Override
    public void onCreation(Identifiable identifiable) {
        // empty default implementation
    }

    @Override
    public void onRemoval(Identifiable identifiable) {
        // empty default implementation
    }

    public Set<String> getModifications() {
        return substationsIds;
    }
}
