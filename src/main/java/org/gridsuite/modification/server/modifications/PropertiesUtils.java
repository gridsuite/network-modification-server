package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.*;
import com.powsybl.iidm.network.Identifiable;
import org.gridsuite.modification.server.dto.FreePropertyInfos;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public final class PropertiesUtils {
    public static final String PROPERTIES = "Properties";

    private PropertiesUtils() {
        // Should not be instantiated
    }

    public static void applyProperties(Identifiable<?> identifiable, ReportNode subReportNode, @Nullable List<FreePropertyInfos> properties, String propertiesLabelKey) {
        List<ReportNode> reportNodes = new ArrayList<>();
        Optional.ofNullable(properties).ifPresent(props ->
            props.forEach(prop ->
                Optional.ofNullable(PropertiesUtils.applyProperty(identifiable, prop))
                    .ifPresent(reportNodes::add)
            )
        );
        if (!reportNodes.isEmpty()) {
            ModificationUtils.getInstance().reportModifications(subReportNode, reportNodes,
                propertiesLabelKey, PROPERTIES, Map.of());
        }
    }

    private static ReportNode applyProperty(Identifiable<?> identifiable, FreePropertyInfos prop) {
        ReportNodeBuilder builder = ReportNode.newRootReportNode();
        if (prop.isDeletionMark()) {
            if (identifiable.removeProperty(prop.getName())) {
                reportPropertyDeletion(builder, prop);
                return builder.build();
            }
        } else {
            String oldValue = identifiable.setProperty(prop.getName(), prop.getValue());
            if (oldValue != null) { // update
                reportPropertyModification(builder, prop);
                return builder.build();
            } else { // insert
                reportPropertyCreation(builder, prop);
                return builder.build();
            }
        }
        return null;
    }

    private static void reportPropertyCreation(ReportNodeAdderOrBuilder<?> adderOrBuilder, FreePropertyInfos prop) {
        adderOrBuilder.withMessageTemplate("propertyAdded", "    Property ${name} added with value ${value}")
            .withUntypedValue("name", prop.getName())
            .withUntypedValue("value", prop.getValue())
            .withSeverity(TypedValue.INFO_SEVERITY);
    }

    private static void reportPropertyModification(ReportNodeAdderOrBuilder<?> adderOrBuilder, FreePropertyInfos prop) {
        adderOrBuilder.withMessageTemplate("propertyChanged", "    Property ${name} changed : ${from} -> ${to}")
            .withUntypedValue("name", prop.getName())
            .withUntypedValue("to", prop.getValue())
            .withUntypedValue("from", prop.getPreviousValue() == null ? "null" : prop.getPreviousValue())
            .withSeverity(TypedValue.INFO_SEVERITY);
    }

    private static void reportPropertyDeletion(ReportNodeAdderOrBuilder<?> adderOrBuilder, FreePropertyInfos prop) {
        adderOrBuilder.withMessageTemplate("propertyDeleted", "    Property ${name} deleted")
            .withUntypedValue("name", prop.getName())
            .withSeverity(TypedValue.INFO_SEVERITY);
    }
}
