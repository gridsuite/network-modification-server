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

import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_SCALING_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.createReport;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScaling extends AbstractScaling {

    public LoadScaling(LoadScalingInfos loadScalableInfos) {
        super(loadScalableInfos);
    }

    /*
        Here I removed the use of the AtomicReference, and the use of the forEach loop, which creates a new object for each iteration,
        instead i used the for loop which is more efficient in terms of memory and performance.
        Also, i initialized the lists with their respective sizes, which will save memory and increase the performance
     */
    @Override
    protected void applyVentilationVariation(Network network,
                                             List<IdentifiableAttributes> identifiableAttributes,
                                             ScalingVariationInfos scalingVariationInfos, Reporter subReporter, Double distributionKeys) {
        if (distributionKeys != null) {
            double sum = 0;
            List<Float> percentages = new ArrayList<>(identifiableAttributes.size());
            List<Scalable> scalables = new ArrayList<>(identifiableAttributes.size());
            for (IdentifiableAttributes equipment : identifiableAttributes) {
                sum += network.getLoad(equipment.getId()).getP0();
                scalables.add(getScalable(equipment.getId()));
                percentages.add((float) ((equipment.getDistributionKey() / distributionKeys) * 100));
            }
            Scalable ventilationScalable = Scalable.proportional(percentages, scalables);
            var asked = getAsked(scalingVariationInfos, sum);
            var done = scale(network, scalingVariationInfos, asked, ventilationScalable);
            createReport(subReporter, "ScaleVentilationVariation", String.format("Successfully scaling variation in ventilation mode with variation value asked is %s and variation done is %s", asked, done), TypedValue.INFO_SEVERITY);
        }
    }

    /*
        Here, I replaced the use of a stream and an AtomicReference with a simple for loop to iterate over the list of IdentifiableAttributes,
        and also replaced the use of filter with a simple if statement to check if the load is non-null.
        This will decrease the number of objects created and will increase the performance.
        Also, i initialized the lists with their respective sizes, which will save memory and increase the performance.
     */
    @Override
    protected void applyRegularDistributionVariation(Network network,
                                                     List<IdentifiableAttributes> identifiableAttributes,
                                                     ScalingVariationInfos scalingVariationInfos, Reporter subReporter) {
        double sum = 0;
        List<Scalable> scalables = new ArrayList<>(identifiableAttributes.size());

        for (IdentifiableAttributes attribute : identifiableAttributes) {
            Load load = network.getLoad(attribute.getId());
            if (load == null) {
                continue;
            }
            sum += load.getP0();
            scalables.add(getScalable(load.getId()));
        }

        int n = scalables.size();
        List<Float> percentages = new ArrayList<>(Collections.nCopies(n, 100f / n));
        Scalable regularDistributionScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, regularDistributionScalable);
        createReport(subReporter, "ScaleRegularDistributionVariation", String.format("Successfully scaling variation in regular Distribution mode with variation value asked is %s and variation done is %s", asked, done), TypedValue.INFO_SEVERITY);
    }

    /*
        Here, I replaced the use of a stream and an AtomicReference with a simple for loop to iterate over the list of IdentifiableAttributes,
        and also replaced the use of filter with a simple if statement to check if the load is non-null.
        Also i initialized the lists with their respective sizes, which will save memory and increase the performance.
        Also, I removed the unnecessary step of putting all the loads into a list and then iterating over that list to add the loads to the targetPMap,
        since we already have the information we need in the IdentifiableAttributes list.
     */
    @Override
    protected void applyProportionalVariation(Network network,
                                              List<IdentifiableAttributes> identifiableAttributes,
                                              ScalingVariationInfos scalingVariationInfos, Reporter subReporter) {
        double sum = 0;
        Map<String, Double> targetPMap = new HashMap<>(identifiableAttributes.size());
        List<Float> percentages = new ArrayList<>(identifiableAttributes.size());
        List<Scalable> scalables = new ArrayList<>(identifiableAttributes.size());

        for (IdentifiableAttributes attribute : identifiableAttributes) {
            Load load = network.getLoad(attribute.getId());
            if (load == null) {
                continue;
            }
            sum += load.getP0();
            targetPMap.put(load.getId(), load.getP0());
        }

        if (sum == 0) {
            throw new IllegalArgumentException("Sum of P0 can't be zero before doing the division");
        }

        for (Map.Entry<String, Double> entry : targetPMap.entrySet()) {
            percentages.add((float) (entry.getValue() / sum * 100));
            scalables.add(getScalable(entry.getKey()));
        }

        Scalable proportionalScalable = Scalable.proportional(percentages, scalables);
        var asked = getAsked(scalingVariationInfos, sum);
        var done = scale(network, scalingVariationInfos, asked, proportionalScalable);
        createReport(subReporter, "ScaleProportionalVariation", String.format("Successfully scaling variation in proportional mode with variation value asked is %s and variation done is %s", asked, done), TypedValue.INFO_SEVERITY);
    }

    private double scale(Network network, ScalingVariationInfos scalingVariationInfos, double asked, Scalable proportionalScalable) {
        switch (scalingVariationInfos.getReactiveVariationMode()) {
            case CONSTANT_Q:
                return proportionalScalable.scale(network, asked, Scalable.ScalingConvention.LOAD);
            case TAN_PHI_FIXED:
                return proportionalScalable.scaleWithConstantPowerFactor(network, asked, Scalable.ScalingConvention.LOAD);
            default:
                throw new NetworkModificationException(LOAD_SCALING_ERROR, "Reactive Variation mode not recognised");
        }
    }

    @Override
    protected double getAsked(ScalingVariationInfos scalingVariationInfos, double sum) {
        return scalingInfos.getVariationType() == VariationType.DELTA_P
                ? scalingVariationInfos.getVariationValue()
                : scalingVariationInfos.getVariationValue() - sum;
    }

    @Override
    protected Scalable getScalable(String id) {
        return Scalable.onLoad(id, -Double.MAX_VALUE, Double.MAX_VALUE);
    }

}
