/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.TapChanger;
import com.powsybl.iidm.network.TapChangerStep;
import com.powsybl.iidm.network.ThreeWindingsTransformer;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import com.powsybl.network.store.client.NetworkStoreService;
import org.apache.commons.math3.util.Precision;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.util.AbstractMap;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@ComponentScan(basePackageClasses = {NetworkStoreService.class})
@Service
class NetworkModificationService {

    @Autowired
    private NetworkStoreService networkStoreService;

    private Network getNetwork(UUID networkUuid) {
        try {
            return networkStoreService.getNetwork(networkUuid);
        } catch (PowsyblException e) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Network '" + networkUuid + "' not found");
        }
    }

    void changeSwitchState(UUID networkUuid, String switchId, String open) {
        Network network = getNetwork(networkUuid);
        Switch sw = network.getSwitch(switchId);
        if (sw == null) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Switch " + switchId + " not found");
        }

        sw.setOpen(Boolean.parseBoolean(open));

        networkStoreService.flush(network);
    }

    private interface IApplyChange<T> {
        boolean apply(T obj, String change, String target);
    }

    <T> Map<String, Boolean> applyChanges(T obj, Map<String, String> changeRequest, IApplyChange<T> func) {
        return changeRequest.entrySet().stream()
            .map(cr -> new AbstractMap.SimpleEntry<>(cr.getKey(), func.apply(obj, cr.getKey(), cr.getValue())))
            .collect(HashMap::new, (map, res) -> map.put(res.getKey(), res.getValue()), HashMap::putAll);
    }

    Map<String, Boolean> changEquipmentState(UUID networkUuid, ModifiableEquipmentType equipmentType, String id, Map<String, String> changeRequest) {
        Network network = getNetwork(networkUuid);
        try {
            switch (equipmentType) {
                case TWO_WINDING_TRANSFORMER:
                    return applyChanges(network.getTwoWindingsTransformer(id), changeRequest, this::applyModification);
                case THREE_WINDING_TRANSFORMER:
                    return applyChanges(network.getThreeWindingsTransformer(id), changeRequest, this::applyModification);
                case GENERATOR:
                    return applyChanges(network.getGenerator(id), changeRequest, this::applyModification);
            }
        } finally {
            networkStoreService.flush(network);
        }
        return Collections.emptyMap();
    }

    private <C extends TapChanger<C, S>, S extends TapChangerStep<S>>
        boolean setTapChanger(Optional<? extends TapChanger<C, S>> tapChanger, Integer newStep, Identifiable<?> transformer) {
        if (tapChanger.isPresent()) {
            var ratioTapChanger = tapChanger.get();
            if (ratioTapChanger.getLowTapPosition() <= newStep && ratioTapChanger.getHighTapPosition() >= newStep) {
                ratioTapChanger.setTapPosition(newStep);
                transformer.setFictitious(transformer.isFictitious()); // pour forcer l'update du transfo...
                return true;
            }
        }
        return false;
    }

    private boolean setTargetQ(Generator generator, Double target) {
        generator.setTargetQ(target);
        return Precision.equals(generator.getTargetQ(), target);
    }

    private boolean setTargetP(Generator generator, Double target) {
        if (generator.getMaxP() >= target && generator.getMinP() <= target) {
            generator.setTargetP(target);
        }
        return Precision.equals(generator.getTargetP(), target);
    }

    private boolean applyModification(ThreeWindingsTransformer transformer, String change, String value) {
        switch (change) {
            case "phaseTapChanger1Position":
                return setTapChanger(transformer.getLeg1().getOptionalPhaseTapChanger(), Integer.valueOf(value), transformer);
            case "phaseTapChanger2Position":
                return setTapChanger(transformer.getLeg2().getOptionalPhaseTapChanger(), Integer.valueOf(value), transformer);
            case "phaseTapChanger3Position":
                return setTapChanger(transformer.getLeg3().getOptionalPhaseTapChanger(), Integer.valueOf(value), transformer);
            case "ratioTapChanger1Position":
                return setTapChanger(transformer.getLeg1().getOptionalRatioTapChanger(), Integer.valueOf(value), transformer);
            case "ratioTapChanger2Position":
                return setTapChanger(transformer.getLeg2().getOptionalRatioTapChanger(), Integer.valueOf(value), transformer);
            case "ratioTapChanger3Position":
                return setTapChanger(transformer.getLeg3().getOptionalRatioTapChanger(), Integer.valueOf(value), transformer);
            default:
                return false;
        }
    }

    private boolean applyModification(Generator generator, String change, String value) {
        switch (change) {
            case "targetP":
                return setTargetP(generator, Double.valueOf(value));
            case "targetQ":
                return setTargetQ(generator, Double.valueOf(value));
        }
        return false;
    }

    private boolean applyModification(TwoWindingsTransformer transformer, String change, String value) {
        switch (change) {
            case "phaseTapChangerPosition":
                return setTapChanger(transformer.getOptionalPhaseTapChanger(), Integer.valueOf(value), transformer);
            case "ratioTapChangerPosition":
                return setTapChanger(transformer.getOptionalRatioTapChanger(), Integer.valueOf(value), transformer);
            default:
                return false;
        }
    }

}
