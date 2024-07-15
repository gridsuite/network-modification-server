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

    public static void applyProperties(Identifiable<?> identifiable, ReportNode subReportNode, @Nullable List<FreePropertyInfos> properties) {
        Optional.ofNullable(properties).ifPresent(props ->
            props.forEach(prop -> PropertiesUtils.applyProperty(identifiable, prop, subReportNode)));
    }

    private static void applyProperty(Identifiable<?> identifiable, FreePropertyInfos prop, ReportNode subReportNode) {
        ReportNodeAdder adder = subReportNode.newReportNode();
        if (prop.isDeletionMark()) {
            if (identifiable.removeProperty(prop.getName())) {
                deletePropertyReportNode(adder, prop);
                adder.add();
            }
        } else {
            String oldValue = identifiable.setProperty(prop.getName(), prop.getValue());
            if (oldValue != null) { // update
                modifyPropertyReportNode(adder, prop);
                adder.add();
            } else { // insert
                createPropertyReportNode(adder, prop);
                adder.add();
            }
        }
    }

    public static void reportPropertiesInfos(Identifiable<?> identifiable, List<FreePropertyInfos> modificationInfosProperties,
                                             String propertiesLabelKey, ReportNode subReportNode) {
        Optional<List<FreePropertyInfos>> properties = Optional.ofNullable(modificationInfosProperties);
        List<ReportNode> reportNodes = new ArrayList<>();
        if (properties.isPresent()) {
            ReportNode propertiesReporter = subReportNode.newReportNode().withMessageTemplate(propertiesLabelKey, PROPERTIES).add();
            properties.get().forEach(property -> reportNodes.add(applyPropertyReportNode(identifiable, property)));
            ModificationUtils.getInstance().reportModifications(propertiesReporter, reportNodes,
                "VscProperties", PROPERTIES, Map.of());
        }
    }

    public static ReportNode applyPropertyReportNode(Identifiable<?> identifiable, FreePropertyInfos prop) {
        ReportNodeBuilder builder = ReportNode.newRootReportNode();
        if (prop.isDeletionMark()) {
            // delete
            if (identifiable.removeProperty(prop.getName())) {
                deletePropertyReportNode(builder, prop);
            }
        } else {
            String oldValue = identifiable.setProperty(prop.getName(), prop.getValue());
            if (oldValue != null) { // update
                modifyPropertyReportNode(builder, prop);
            } else { // insert
                createPropertyReportNode(builder, prop);
            }
        }
        return builder.build();
    }

    private static void createPropertyReportNode(ReportNodeAdderOrBuilder<?> adderOrBuilder, FreePropertyInfos prop) {
        adderOrBuilder.withMessageTemplate("propertyAdded", "    Property ${name} added with value ${value}")
            .withUntypedValue("name", prop.getName())
            .withUntypedValue("value", prop.getValue())
            .withSeverity(TypedValue.INFO_SEVERITY);
    }

    private static void modifyPropertyReportNode(ReportNodeAdderOrBuilder<?> adderOrBuilder, FreePropertyInfos prop) {
        adderOrBuilder.withMessageTemplate("propertyAdded", "    Property ${name} added with value ${value}")
            .withUntypedValue("name", prop.getName())
            .withUntypedValue("value", prop.getValue())
            .withSeverity(TypedValue.INFO_SEVERITY);
    }

    private static void deletePropertyReportNode(ReportNodeAdderOrBuilder<?> adderOrBuilder, FreePropertyInfos prop) {
        adderOrBuilder.withMessageTemplate("propertyDeleted", "    Property ${name} deleted")
            .withUntypedValue("name", prop.getName())
            .withSeverity(TypedValue.INFO_SEVERITY);
    }
}
