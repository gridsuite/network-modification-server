/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_SCALING_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScaling extends AbstractScaling {

    public LoadScaling(LoadScalingInfos loadScalableInfos) {
        super(loadScalableInfos);
    }

    @Override
    protected void applyVentilationVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos, Double distributionKeys) {
        if (distributionKeys != null) {
            AtomicReference<Double> sum = new AtomicReference<>(0D);
            List<Float> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            identifiableAttributes.forEach(equipment -> {
                sum.set(network.getLoad(equipment.getId()).getP0() + sum.get());
                scalables.add(getScalable(equipment.getId()));
                percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            var asked = getAsked(scalingVariationInfos, sum);
            var done = scale(network, scalingVariationInfos, asked, ventilationScalable);
            createReport(subReporter, "scalingApplied", String.format("Successfully scaling variation in ventilation mode with variation value asked is %s and variation done is %s", asked, done), TypedValue.INFO_SEVERITY);
        }
    }

    @Override
    protected void applyRegularDistributionVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        List<Load> loads = identifiableAttributes
                .stream()
                .map(attribute -> network.getLoad(attribute.getId()))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        AtomicReference<Double> sum = new AtomicReference<>(0D);

        List<Scalable> scalables = loads.stream()
                .map(load -> {
                    sum.set(sum.get() + load.getP0());
                    return getScalable(load.getId());
                }).collect(Collectors.toList());

        List<Float> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), (float) (100.0 / scalables.size())));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, regularDistributionScalable);
        createReport(subReporter, "scalingApplied", String.format("Successfully scaling variation in regular Distribution mode with variation value asked is %s and variation done is %s", asked, done), TypedValue.INFO_SEVERITY);
    }

    @Override
    protected void applyProportionalVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        List<Load> loads = identifiableAttributes
                .stream().map(attribute -> network.getLoad(attribute.getId())).collect(Collectors.toList());
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Float> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        loads.forEach(load -> {
            targetPMap.put(load.getId(), load.getP0());
            sum.set(sum.get() + load.getP0());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, proportionalScalable);
        createReport(subReporter, "scalingApplied", String.format("Successfully scaling variation in proportional mode with variation value asked is %s and variation done is %s", asked, done), TypedValue.INFO_SEVERITY);
    }

    @Override
    protected void applyProportionalToPmaxVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        // no implementation for load scaling
        throw new NetworkModificationException(scalingInfos.getErrorType(), String.format("This variation mode is not supported : %s", scalingVariationInfos.getVariationMode().name()));
    }

    @Override
    protected void applyStackingUpVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        // no implementation for load scaling
        throw new NetworkModificationException(scalingInfos.getErrorType(), String.format("This variation mode is not supported : %s", scalingVariationInfos.getVariationMode().name()));
    }

    private double scale(Network network, ScalingVariationInfos scalingVariationInfos, double asked, Scalable proportionalScalable) {
        switch (scalingVariationInfos.getReactiveVariationMode()) {
            case CONSTANT_Q:
                return proportionalScalable.scale(network, asked, Scalable.ScalingConvention.LOAD);
            case TAN_PHI_FIXED:
                return proportionalScalable.scaleWithConstantPowerFactor(network, asked, Scalable.ScalingConvention.LOAD); //TODO waiting for to be fixed by PowSyBl
            default:
                throw new NetworkModificationException(LOAD_SCALING_ERROR, "Reactive Variation mode not recognised");
        }
    }

    @Override
    public double getAsked(ScalingVariationInfos scalingVariationInfos, AtomicReference<Double> sum) {
        return scalingInfos.getVariationType() == VariationType.DELTA_P
                ? scalingVariationInfos.getVariationValue()
                : scalingVariationInfos.getVariationValue() - sum.get();
    }

    @Override
    protected Scalable getScalable(String id) {
        return Scalable.onLoad(id, -Double.MAX_VALUE, Double.MAX_VALUE);
    }

}
