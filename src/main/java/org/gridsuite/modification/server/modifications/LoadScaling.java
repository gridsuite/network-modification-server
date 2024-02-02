/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationMode;
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
            List<Double> percentages = new ArrayList<>();
            List<Scalable> scalables = new ArrayList<>();

            identifiableAttributes.forEach(equipment -> {
                sum.set(network.getLoad(equipment.getId()).getP0() + sum.get());
                scalables.add(getScalable(equipment.getId()));
                percentages.add((equipment.getDistributionKey() / distributionKeys) * 100);
            });
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            var asked = getAsked(scalingVariationInfos, sum);
            var done = scale(network, scalingVariationInfos, asked, ventilationScalable);
            reportScaling(subReporter, scalingVariationInfos.getVariationMode(), asked, done);
        }
    }

    @Override
    protected void applyRegularDistributionVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        List<Load> loads = identifiableAttributes
                .stream()
                .map(attribute -> network.getLoad(attribute.getId()))
                .filter(Objects::nonNull)
                .toList();

        AtomicReference<Double> sum = new AtomicReference<>(0D);

        List<Scalable> scalables = loads.stream()
                .map(load -> {
                    sum.set(sum.get() + load.getP0());
                    return getScalable(load.getId());
                }).collect(Collectors.toList());

        List<Double> percentages = new ArrayList<>(Collections.nCopies(scalables.size(), 100.0 / scalables.size()));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, regularDistributionScalable);
        reportScaling(subReporter, scalingVariationInfos.getVariationMode(), asked, done);
    }

    @Override
    protected void applyProportionalVariation(Network network, Reporter subReporter, List<IdentifiableAttributes> identifiableAttributes, ScalingVariationInfos scalingVariationInfos) {
        List<Load> loads = identifiableAttributes
                .stream().map(attribute -> network.getLoad(attribute.getId())).toList();
        AtomicReference<Double> sum = new AtomicReference<>(0D);
        Map<String, Double> targetPMap = new HashMap<>();
        List<Double> percentages = new ArrayList<>();
        List<Scalable> scalables = new ArrayList<>();
        loads.forEach(load -> {
            targetPMap.put(load.getId(), load.getP0());
            sum.set(sum.get() + load.getP0());
        });
        targetPMap.forEach((id, p) -> {
            percentages.add((p / sum.get()) * 100);
            scalables.add(getScalable(id));
        });

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, proportionalScalable);
        reportScaling(subReporter, scalingVariationInfos.getVariationMode(), asked, done);
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
        return switch (scalingVariationInfos.getReactiveVariationMode()) {
            case CONSTANT_Q ->
                    proportionalScalable.scale(network, asked, new ScalingParameters().setScalingConvention(Scalable.ScalingConvention.LOAD));
            case TAN_PHI_FIXED ->
                    proportionalScalable.scale(network, asked, new ScalingParameters().setScalingConvention(Scalable.ScalingConvention.LOAD).setConstantPowerFactor(true));
        };
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

    private void reportScaling(Reporter subReporter, VariationMode variationMode, double askedValue, double actualValue) {
        subReporter.report(Report.builder()
                .withKey("scalingApplied")
                .withDefaultMessage("Successfully scaling variation in ${variationMode} mode with variation value asked is ${askedValue} and variation done is ${actualValue}")
                .withValue("variationMode", variationMode.name())
                .withValue("askedValue", askedValue)
                .withValue("actualValue", actualValue)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }
}
