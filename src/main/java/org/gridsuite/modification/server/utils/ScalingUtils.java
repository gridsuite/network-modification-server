package org.gridsuite.modification.server.utils;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.GeneratorScalingVariation;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

public class ScalingUtils {
    public static void calculateProportionalScalingPercentage(Network network,
                                                              FilterAttributes filter,
                                                              GeneratorScalingVariation generatorScalingVariation,
                                                              AtomicReference<Double> sum,
                                                              List<Float> percentages,
                                                              List<Scalable> scalables) {
        Map<String, Double> targetPMap = new HashMap<>();
        filter.getFilterEquipmentsAttributes()
                .stream()
                .forEach(equipment -> {
                    Identifiable<?> identifiable = network.getIdentifiable(equipment.getEquipmentID());
                    if (identifiable != null) {
                        switch (identifiable.getType()){
                            case GENERATOR:
                                Generator generator = (Generator) identifiable;
                                targetPMap.put(generator.getId(), generator.getTargetP());
                                sum.set(sum.get() + generator.getTargetP());
                                break;
                            default:
                                throw new PowsyblException("not supported");
                        }
                    }
                });
        targetPMap.forEach((id, p) -> {
            percentages.add((float) ((p / sum.get()) * 100));
            scalables.add(getScalable(id));
        });
    }

    public static void createReport(Reporter reporter, String reporterKey, String message, TypedValue errorSeverity) {
        reporter.report(Report.builder()
                .withKey(reporterKey)
                .withDefaultMessage(message)
                .withSeverity(errorSeverity)
                .build());
    }

    private static Scalable getScalable(String id) {
        return Scalable.onGenerator(id);
    }
}
