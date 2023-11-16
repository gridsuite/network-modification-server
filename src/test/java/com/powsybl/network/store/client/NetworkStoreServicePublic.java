/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package com.powsybl.network.store.client;

import com.powsybl.network.store.iidm.impl.NetworkStoreClient;
import com.powsybl.network.store.iidm.impl.util.TriFunction;

import java.util.concurrent.ExecutorService;

/**
 * set to {@code public} all constructors
 */
public class NetworkStoreServicePublic extends NetworkStoreService {
    public NetworkStoreServicePublic(String baseUri) {
        super(baseUri);
    }

    public NetworkStoreServicePublic(String baseUri, PreloadingStrategy defaultPreloadingStrategy) {
        super(baseUri, defaultPreloadingStrategy);
    }

    public NetworkStoreServicePublic(RestClient restClient, PreloadingStrategy defaultPreloadingStrategy) {
        super(restClient, defaultPreloadingStrategy);
    }

    public NetworkStoreServicePublic(RestClient restClient, PreloadingStrategy defaultPreloadingStrategy, TriFunction<RestClient, PreloadingStrategy, ExecutorService, NetworkStoreClient> decorator) {
        super(restClient, defaultPreloadingStrategy, decorator);
    }

    public NetworkStoreServicePublic(String baseUri, PreloadingStrategy defaultPreloadingStrategy, TriFunction<RestClient, PreloadingStrategy, ExecutorService, NetworkStoreClient> decorator) {
        super(baseUri, defaultPreloadingStrategy, decorator);
    }
}
