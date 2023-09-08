package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.server.dto.SubstationCreationInfos;

import java.util.Map;
import java.util.Optional;

public class SubstationCreation extends AbstractModification {

    private final SubstationCreationInfos modificationInfos;

    public SubstationCreation(SubstationCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {

        Substation substation = network.newSubstation()
                .setId(modificationInfos.getEquipmentId())
                .setName(modificationInfos.getEquipmentName())
                .setCountry(modificationInfos.getSubstationCountry())
                .add();
        Map<String, String> properties = modificationInfos.getProperties();
        if (properties != null) {
            properties.forEach(substation::setProperty);
        }

        subReporter.report(Report.builder()
                .withKey("substationCreated")
                .withDefaultMessage("New substation with id=${id} created")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        // name and country
        if (modificationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReporter, modificationInfos.getEquipmentName(), "Name");
        }
        if (modificationInfos.getSubstationCountry() != null) {
            ModificationUtils.getInstance()
                    .reportElementaryCreation(subReporter, modificationInfos.getSubstationCountry(), "Country");
        }
        // properties
        Optional.ofNullable(modificationInfos.getProperties())
                .filter(propertie -> !properties.isEmpty())
                .ifPresent(propertie -> properties.forEach((key, value) -> subReporter.report(Report.builder()
                        .withKey("propertyAdded")
                        .withDefaultMessage("    Property ${name} added with value ${value}")
                        .withValue("name", key)
                        .withValue("value", value)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build())));

    }
}
