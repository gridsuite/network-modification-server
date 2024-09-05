package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.TwoWindingsTransformerField;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.ModificationUtils;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.modifications.ModificationUtils.*;

public abstract class AbstractByFilterModification extends AbstractModification {
    public static final String KEY_FILTER_NAME = "filterName";
    public static final String KEY_FIELD_NAME = "fieldName";
    public static final String KEY_EQPT_NAME = "eqptName";
    public static final String KEY_EQPT_TYPE = "eqptType";
    public static final String KEY_NB_CHANGED = "nbChanged";
    public static final String KEY_NB_UNCHANGED = "nbUnchanged";
    public static final String KEY_VALUE = "value";
    public static final String KEY_EQUIPMENT_MODIFIED_ERROR = "EquipmentModifiedError";
    public static final String KEY_BY_FILTER_MODIFICATION_SOME = "byFilterModificationSome";
    public static final String KEY_BY_FILTER_MODIFICATION_FAILED = "byFilterModificationFailed";
    public static final String KEY_BY_FILTER_MODIFICATION_SUCCESS = "byFilterModificationSuccess";
    public static final String KEY_NUMBER_OF_VALID_EQUIPMENT = "numberOfValidEquipment";
    public static final String KEY_NOT_EDITED_EQUIPMENTS_FILTER = "NotEditedEquipmentsFilter";
    public static final String KEY_EDITED_FIELD_FILTER = "editedFieldFilter";
    public static final String KEY_FILTER_EQUIPMENTS_NOT_FOUND = "filterEquipmentsNotFound";
    public static final String KEY_EQUIPMENT_MODIFIED_REPORT = "EquipmentModifiedReport";
    public static final String KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION = "EquipmentModifiedReportException";
    public static final String KEY_APPLIED_BY_FILTER_MODIFICATIONS = "appliedByFilterModifications";
    public static final String KEY_BY_FILTER_MODIFICATION = "byFilterModification";
    public static final String KEY_BY_FILTER_MODIFICATION_ALL = "byFilterModificationAll";
    public static final String KEY_BY_FILTER_MODIFICATION_NONE = "byFilterModificationNone";
    protected FilterService filterService;
    protected int equipmentNotModifiedCount;
    protected long equipmentCount;
    protected long equipmentNotFoundCount;

    protected AbstractByFilterModification() {
        equipmentNotModifiedCount = 0;
        equipmentCount = 0;
        equipmentNotFoundCount = 0;
    }

    public abstract String getModificationLabel();

    public abstract ModificationInfos getModificationInfos();

    public abstract IdentifiableType getIdentifiableType();

    public abstract NetworkModificationException.Type getExceptionType();

    public abstract List<AbstractModificationByFilterInfos> getModificationByFilterInfosList();

    protected abstract boolean preCheckValue(Identifiable<?> identifiable,
                                             AbstractModificationByFilterInfos filterModificationInfos,
                                             List<ReportNode> reports, List<String> notEditableEquipments);

    protected abstract Object applyValue(Identifiable<?> identifiable,
                                         AbstractModificationByFilterInfos filterModificationInfos);

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (getModificationInfos() == null) {
            throw new NetworkModificationException(getExceptionType(), "Missing required attributes to modify the equipment");
        }

        if (CollectionUtils.isEmpty(getModificationByFilterInfosList())) {
            throw new NetworkModificationException(getExceptionType(), String.format("At least one %s is required", getModificationLabel()));
        }

        if (getModificationByFilterInfosList().stream().anyMatch(modificationByFilterInfos -> CollectionUtils.isEmpty(modificationByFilterInfos.getFilters()))) {
            throw new NetworkModificationException(getExceptionType(), String.format("Every %s must have at least one filter", getModificationLabel()));
        }
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // collect all filters from all variations
        Map<UUID, String> filters = getFilters();

        Map<UUID, FilterEquipments> exportFilters =
                ModificationUtils.getUuidFilterEquipmentsMap(filterService, network, subReportNode, filters, getModificationInfos().getErrorType());

        if (exportFilters != null) {
            ReportNode subReporter = subReportNode.newReportNode().withMessageTemplate(KEY_APPLIED_BY_FILTER_MODIFICATIONS, StringUtils.capitalize(getModificationLabel())).add();
            List<ReportNode> reports = new ArrayList<>();
            // perform modifications
            getModificationByFilterInfosList().forEach(modificationByFilterInfos ->
                    modificationByFilterInfos.getFilters().forEach(filterInfos ->
                            applyOnFilterEquipments(network, exportFilters, reports, modificationByFilterInfos, filterInfos)));
            // reporting
            subReportNode.newReportNode()
                    .withMessageTemplate(KEY_BY_FILTER_MODIFICATION, String.format("New %s on ${%s}", getModificationLabel(), KEY_EQPT_TYPE))
                    .withUntypedValue(KEY_EQPT_TYPE, getIdentifiableType().name())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
            if (equipmentNotModifiedCount == 0 && equipmentNotFoundCount == 0) {
                subReportNode.newReportNode()
                        .withMessageTemplate(KEY_BY_FILTER_MODIFICATION_ALL, "All equipment have been modified : ${" + KEY_VALUE + "} equipment(s)")
                        .withUntypedValue(KEY_VALUE, equipmentCount)
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .add();
                report(subReporter, reports);
            } else {
                if (equipmentNotModifiedCount == equipmentCount) {
                    createReport(subReportNode, KEY_BY_FILTER_MODIFICATION_NONE,
                            "No equipment have been modified",
                            Map.of(), TypedValue.ERROR_SEVERITY);
                } else {
                    subReportNode.newReportNode()
                            .withMessageTemplate(KEY_BY_FILTER_MODIFICATION_SOME, "Some of the equipment have been modified : ${" + KEY_NB_CHANGED + "} equipment(s) modified and ${" + KEY_NB_UNCHANGED + "} equipment(s) not modified")
                            .withUntypedValue(KEY_NB_CHANGED, equipmentCount - equipmentNotModifiedCount)
                            .withUntypedValue(KEY_NB_UNCHANGED, equipmentNotModifiedCount + equipmentNotFoundCount)
                            .withSeverity(TypedValue.WARN_SEVERITY)
                            .add();
                    report(subReporter, reports);
                }
            }
        }
    }

    private boolean isEquipmentEditable(Identifiable<?> identifiable,
                                          AbstractModificationByFilterInfos filterModificationInfos,
                                          List<ReportNode> equipmentsReport) {
        if (filterModificationInfos.getEditedField() == null) {
            return false;
        }

        if (identifiable.getType() == IdentifiableType.TWO_WINDINGS_TRANSFORMER) {
            TwoWindingsTransformerField editedField = TwoWindingsTransformerField.valueOf(filterModificationInfos.getEditedField());
            TwoWindingsTransformer twoWindingsTransformer = (TwoWindingsTransformer) identifiable;
            return switch (editedField) {
                case TARGET_V, RATIO_LOW_TAP_POSITION, RATIO_TAP_POSITION, RATIO_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getRatioTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(ReportNode.newRootReportNode()
                                .withMessageTemplate(KEY_EQUIPMENT_MODIFIED_ERROR + equipmentsReport.size(), "        Cannot modify field ${" + KEY_FIELD_NAME + "} of equipment ${" + KEY_EQPT_NAME + "} : Ratio tab changer is null")
                                .withUntypedValue(KEY_FIELD_NAME, editedField.name())
                                .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                                .withSeverity(TypedValue.TRACE_SEVERITY)
                                .build());
                    }
                    yield isEditable;
                }
                case REGULATION_VALUE, PHASE_LOW_TAP_POSITION, PHASE_TAP_POSITION, PHASE_TARGET_DEADBAND -> {
                    boolean isEditable = twoWindingsTransformer.getPhaseTapChanger() != null;
                    if (!isEditable) {
                        equipmentsReport.add(ReportNode.newRootReportNode()
                                .withMessageTemplate(KEY_EQUIPMENT_MODIFIED_ERROR + equipmentsReport.size(), "        Cannot modify field ${" + KEY_FIELD_NAME + "} of equipment ${" + KEY_EQPT_NAME + "} : Phase tab changer is null")
                                .withUntypedValue(KEY_FIELD_NAME, editedField.name())
                                .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                                .withSeverity(TypedValue.TRACE_SEVERITY)
                                .build());
                    }
                    yield isEditable;
                }
                default -> true;
            };
        }
        return true;
    }

    private void createByFilterModificationReports(List<ReportNode> reports, AbstractModificationByFilterInfos filterModificationInfos,
                                                     FilterInfos filterInfos, FilterEquipments filterEquipments, List<String> notEditableEquipments) {
        if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_BY_FILTER_MODIFICATION_FAILED + reports.size(), "No equipment(s) have been modified on filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_BY_FILTER_MODIFICATION_SUCCESS + reports.size(), String.format("Successful application of %s on filter ${" + KEY_FILTER_NAME + "}", getModificationLabel()))
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_NUMBER_OF_VALID_EQUIPMENT + reports.size(), "      Number of equipment modified : ${" + KEY_NB_CHANGED + "}")
                    .withUntypedValue(KEY_NB_CHANGED, filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                reports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate(KEY_NOT_EDITED_EQUIPMENTS_FILTER + reports.size(), "       ${" + KEY_NB_UNCHANGED + "} equipment(s) were not modified")
                        .withUntypedValue(KEY_NB_UNCHANGED, notEditableEquipments.size())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }
        }

        reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate(KEY_EDITED_FIELD_FILTER + reports.size(), "      Edited field : ${" + KEY_FIELD_NAME + "}")
                .withUntypedValue(KEY_FIELD_NAME, filterModificationInfos.getEditedField())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
            String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_FILTER_EQUIPMENTS_NOT_FOUND + reports.size(), "      Equipment not found : ${" + KEY_VALUE + "}")
                    .withUntypedValue(KEY_VALUE, equipmentIds)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    private void applyModification(Identifiable<?> identifiable,
                                     AbstractModificationByFilterInfos filterModificationInfos,
                                     List<ReportNode> reports,
                                     List<String> notEditableEquipments) {

        // check pre-conditions
        if (!preCheckValue(identifiable, filterModificationInfos, reports, notEditableEquipments)) {
            return;
        }

        // perform to apply new value
        try {
            final Object newValue = applyValue(identifiable, filterModificationInfos);
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_EQUIPMENT_MODIFIED_REPORT + reports.size(), "        ${" + KEY_EQPT_TYPE + "} id : ${" + KEY_EQPT_NAME + "}, new value of ${" + KEY_FIELD_NAME + "} : ${" + KEY_VALUE + "}")
                    .withUntypedValue(KEY_EQPT_TYPE, identifiable.getType().name())
                    .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                    .withUntypedValue(KEY_FIELD_NAME, filterModificationInfos.getEditedField())
                    .withUntypedValue(KEY_VALUE, String.valueOf(newValue))
                    .withSeverity(TypedValue.TRACE_SEVERITY)
                    .build());
        } catch (Exception e) {
            notEditableEquipments.add(identifiable.getId());
            equipmentNotModifiedCount += 1;
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_EQUIPMENT_MODIFIED_REPORT_EXCEPTION + reports.size(), "        Cannot modify equipment ${" + KEY_EQPT_NAME + "} : ${" + KEY_VALUE + "}")
                    .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                    .withUntypedValue(KEY_VALUE, e.getMessage())
                    .withSeverity(TypedValue.TRACE_SEVERITY)
                    .build());
        }
    }

    private Map<UUID, String> getFilters() {
        return getModificationByFilterInfosList().stream()
                .flatMap(v -> v.getFilters().stream())
                .filter(distinctByKey(FilterInfos::getId))
                .collect(Collectors.toMap(FilterInfos::getId, FilterInfos::getName));
    }

    private void applyOnFilterEquipments(Network network,
                                        Map<UUID, FilterEquipments> exportFilters,
                                        List<ReportNode> reports,
                                        AbstractModificationByFilterInfos modificationByFilterInfos,
                                        FilterInfos filterInfos) {
        FilterEquipments filterEquipments = exportFilters.get(filterInfos.getId());

        if (CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes())) {
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate(KEY_BY_FILTER_MODIFICATION_NONE + reports.size(), "No equipments were found for filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            equipmentCount += filterEquipments.getIdentifiableAttributes().size();
            if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
                equipmentNotFoundCount += filterEquipments.getNotFoundEquipments().size();
            }
            List<String> notEditableEquipments = new ArrayList<>();
            List<ReportNode> equipmentsReport = new ArrayList<>();
            filterEquipments.getIdentifiableAttributes()
                    .stream()
                    .map(attributes -> network.getIdentifiable(attributes.getId()))
                    .filter(identifiable -> {
                        boolean isEditableEquipment = isEquipmentEditable(identifiable, modificationByFilterInfos, equipmentsReport);
                        if (!isEditableEquipment) {
                            notEditableEquipments.add(identifiable.getId());
                            equipmentNotModifiedCount += 1;
                        }
                        return isEditableEquipment;
                    })
                    .forEach(identifiable -> applyModification(identifiable, modificationByFilterInfos, equipmentsReport, notEditableEquipments));

            createByFilterModificationReports(reports, modificationByFilterInfos, filterInfos, filterEquipments, notEditableEquipments);

            reports.addAll(equipmentsReport);
        }
    }

    private void report(ReportNode subReportNode, List<ReportNode> reports) {
        subReportNode.newReportNode()
                .withMessageTemplate(KEY_APPLIED_BY_FILTER_MODIFICATIONS, String.format(" %s", StringUtils.capitalize(getModificationLabel())))
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        reports.forEach(report -> insertReportNode(subReportNode, report));
    }

}
