package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Substation;
import org.gridsuite.modification.server.dto.SubstationCreationInfos;

import java.util.Map;

public class SubstationCreation extends AbstractModification {

    private final SubstationCreationInfos modificationInfos;

    public SubstationCreation(SubstationCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void apply(Network network, Reporter subReporter) {

        Substation substation = network.newSubstation()
                .setId(modificationInfos.getId())
                .setName(modificationInfos.getName())
                .setCountry(modificationInfos.getSubstationCountry())
                .add();
        Map<String, String> properties = modificationInfos.getProperties();
        if (properties != null) {
            properties.forEach(substation::setProperty);
        }

        subReporter.report(Report.builder()
                .withKey("substationCreated")
                .withDefaultMessage("New substation with id=${id} created")
                .withValue("id", modificationInfos.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }
}
