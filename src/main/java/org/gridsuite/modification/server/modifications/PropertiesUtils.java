package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import org.gridsuite.modification.server.dto.FreePropertyInfos;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;

public final class PropertiesUtils {

    private PropertiesUtils() {
        // Should not be instantiated
    }

    public static void applyProperties(Identifiable<?> identifiable, ReportNode subReportNode, @Nullable List<FreePropertyInfos> properties) {
        Optional.ofNullable(properties).ifPresent(props ->
            props.forEach(prop -> PropertiesUtils.applyProperty(identifiable, prop, subReportNode)));
    }

    private static void applyProperty(Identifiable<?> identifiable, FreePropertyInfos prop, ReportNode subReportNode) {
        if (prop.isDeletionMark()) {
            if (identifiable.removeProperty(prop.getName())) {
                subReportNode.newReportNode()
                    .withMessageTemplate("propertyDeleted", "    Property ${name} deleted")
                    .withUntypedValue("name", prop.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            }
        } else {
            String oldValue = identifiable.setProperty(prop.getName(), prop.getValue());
            if (oldValue != null) { // update
                subReportNode.newReportNode()
                    .withMessageTemplate("propertyChanged", "    Property ${name} changed : ${from} -> ${to}")
                    .withUntypedValue("name", prop.getName())
                    .withUntypedValue("from", oldValue)
                    .withUntypedValue("to", prop.getValue())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            } else { // insert
                subReportNode.newReportNode()
                    .withMessageTemplate("propertyAdded", "    Property ${name} added with value ${value}")
                    .withUntypedValue("name", prop.getName())
                    .withUntypedValue("value", prop.getValue())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            }
        }
    }
}
