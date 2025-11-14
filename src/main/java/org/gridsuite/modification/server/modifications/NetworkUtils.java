/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VariantManagerConstants;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.NetworkModificationException;

import static org.gridsuite.modification.NetworkModificationException.Type.VARIANT_NOT_FOUND;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public final class NetworkUtils {

    private NetworkUtils() {
        // Do not instantiate
    }

    public static boolean switchOnExistingVariant(Network network, String variantId) {
        boolean isVariantPresent = true;
        if (variantId != null) {
            if (network.getVariantManager().getVariantIds().stream().anyMatch(id -> id.equals(variantId))) {
                network.getVariantManager().setWorkingVariant(variantId);
            } else {
                isVariantPresent = false;
            }
        }
        return isVariantPresent;
    }

    public static void switchOnNewVariant(Network network, String originVariantId, String destinationVariantId) {
        String startingVariant = StringUtils.isBlank(originVariantId) ? VariantManagerConstants.INITIAL_VARIANT_ID : originVariantId;
        try {
            network.getVariantManager().cloneVariant(startingVariant, destinationVariantId, true);  // cloning variant
            network.getVariantManager().setWorkingVariant(destinationVariantId);  // set current variant to destination variant
        } catch (PowsyblException e) {
            throw new NetworkModificationException(VARIANT_NOT_FOUND, startingVariant);
        }
    }
}
