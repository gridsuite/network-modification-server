/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.service.NetworkVariantsListener;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.NETWORK_NOT_FOUND;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Service
public class ModificationNetworkService {

    private final NetworkStoreService networkStoreService;
    private final EquipmentInfosService equipmentInfosService;
    private final ModificationApplicationInfosService applicationInfosService;

    @Value("${impacts.collection-threshold:50}")
    private Integer collectionThreshold;

    public ModificationNetworkService(NetworkStoreService networkStoreService, EquipmentInfosService equipmentInfosService, ModificationApplicationInfosService applicationInfosService) {
        this.networkStoreService = networkStoreService;
        this.equipmentInfosService = equipmentInfosService;
        this.applicationInfosService = applicationInfosService;
    }

    public ModificationNetwork getNetwork(UUID networkUuid, PreloadingStrategy preloadingStrategy) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid, preloadingStrategy);
        } catch (PowsyblException e) {
            throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
        }
        network.addListener(new NetworkVariantsListener(networkUuid, equipmentInfosService));
        NetworkStoreListener listener = NetworkStoreListener.create(
            network,
            networkUuid,
            networkStoreService,
            equipmentInfosService,
            applicationInfosService,
            collectionThreshold
        );
        return new ModificationNetwork(networkUuid, network, listener);
    }
}
