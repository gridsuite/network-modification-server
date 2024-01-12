package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import org.gridsuite.modification.server.dto.FreePropertyInfos;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;

public final class PropertiesUtils {

    private PropertiesUtils() {
        // Should not be instantiated
    }

    public static void applyProperties(Identifiable<?> identifiable, Reporter subReporter, @Nullable List<FreePropertyInfos> properties) {
        Optional.ofNullable(properties).ifPresent(props ->
            props.forEach(prop -> PropertiesUtils.applyProperty(identifiable, prop, subReporter)));
    }

    private static void applyProperty(Identifiable<?> identifiable, FreePropertyInfos prop, Reporter subReporter) {
        if (prop.isDeletionMark()) {
            if (identifiable.removeProperty(prop.getName())) {
                subReporter.report(Report.builder()
                    .withKey("propertyDeleted")
                    .withDefaultMessage("    Property ${name} deleted")
                    .withValue("name", prop.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        } else {
            String oldValue = identifiable.setProperty(prop.getName(), prop.getValue());
            if (oldValue != null) { // update
                subReporter.report(Report.builder()
                    .withKey("propertyChanged")
                    .withDefaultMessage("    Property ${name} changed : ${from} -> ${to}")
                    .withValue("name", prop.getName())
                    .withValue("from", oldValue)
                    .withValue("to", prop.getValue())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            } else { // insert
                subReporter.report(Report.builder()
                    .withKey("propertyAdded")
                    .withDefaultMessage("    Property ${name} added with value ${value}")
                    .withValue("name", prop.getName())
                    .withValue("value", prop.getValue())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            }
        }
    }
}
