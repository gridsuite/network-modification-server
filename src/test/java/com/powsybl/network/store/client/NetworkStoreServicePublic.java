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
