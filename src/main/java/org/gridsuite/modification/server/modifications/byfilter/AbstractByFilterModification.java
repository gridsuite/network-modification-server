package org.gridsuite.modification.server.modifications.byfilter;

import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.AbstractModificationByFilterInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.TwoWindingsTransformerField;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.List;

public abstract class AbstractByFilterModification extends AbstractModification {
    public static final String EQUIPMENT_MODIFIED_REPORT_ERROR = "EquipmentModifiedReportError_";
    public static final String KEY_FILTER_NAME = "filterName";
    public static final String KEY_FIELD_NAME = "fieldName";
    public static final String KEY_EQPT_NAME = "eqptName";
    public static final String KEY_EQPT_TYPE = "eqptType";
    public static final String KEY_NB_CHANGED = "nbChanged";
    public static final String KEY_NB_UNCHANGED = "nbUnchanged";
    public static final String KEY_VALUE = "value";
    protected FilterService filterService;
    protected int equipmentNotModifiedCount;
    protected long equipmentCount;
    protected long equipmentNotFoundCount;

    protected AbstractByFilterModification() {
        equipmentNotModifiedCount = 0;
        equipmentCount = 0;
        equipmentNotFoundCount = 0;
    }

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
    }

    protected boolean isEquipmentEditable(Identifiable<?> identifiable,
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
                                .withMessageTemplate(EQUIPMENT_MODIFIED_REPORT_ERROR + equipmentsReport.size(), "        Cannot modify field ${" + KEY_FIELD_NAME + "} of equipment ${" + KEY_EQPT_NAME + "} : Ratio tab changer is null")
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
                                .withMessageTemplate(EQUIPMENT_MODIFIED_REPORT_ERROR + equipmentsReport.size(), "        Cannot modify field ${" + KEY_FIELD_NAME + "} of equipment ${" + KEY_EQPT_NAME + "} : Phase tab changer is null")
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

    protected void createByFilterModificationReports(List<ReportNode> reports, AbstractModificationByFilterInfos filterModificationInfos,
                                                     FilterInfos filterInfos, FilterEquipments filterEquipments, List<String> notEditableEquipments) {
        if (notEditableEquipments.size() == filterEquipments.getIdentifiableAttributes().size()) {
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("byFilterModificationFailed_" + reports.size(), "No equipment(s) have been modified on filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        } else {
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("byFilterModification_" + reports.size(), "Successful application of new modification by formula on filter ${" + KEY_FILTER_NAME + "}")
                    .withUntypedValue(KEY_FILTER_NAME, filterInfos.getName())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("numberOfValidEquipment" + reports.size(), "      Number of equipment modified : ${" + KEY_NB_CHANGED + "}")
                    .withUntypedValue(KEY_NB_CHANGED, filterEquipments.getIdentifiableAttributes().size() - notEditableEquipments.size())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            if (!CollectionUtils.isEmpty(notEditableEquipments)) {
                reports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("NotEditedEquipmentsFilter_" + reports.size(), "       ${" + KEY_NB_UNCHANGED + "} equipment(s) were not modified")
                        .withUntypedValue(KEY_NB_UNCHANGED, notEditableEquipments.size())
                        .withSeverity(TypedValue.WARN_SEVERITY)
                        .build());
            }
        }

        reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate("editedFieldFilter_" + reports.size(), "      Edited field :${" + KEY_FIELD_NAME + "}")
                .withUntypedValue(KEY_FIELD_NAME, filterModificationInfos.getEditedField())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (!CollectionUtils.isEmpty(filterEquipments.getNotFoundEquipments())) {
            String equipmentIds = String.join(", ", filterEquipments.getNotFoundEquipments());
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("filterEquipmentsNotFound_" + reports.size(), "      Equipment not found : ${" + KEY_VALUE + "}")
                    .withUntypedValue(KEY_VALUE, equipmentIds)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
        }
    }

    protected void applyModification(Identifiable<?> identifiable,
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
                    .withMessageTemplate("EquipmentModifiedReport_" + reports.size(), "        ${" + KEY_EQPT_TYPE + "} id : ${" + KEY_EQPT_NAME + "}, new value of ${" + KEY_FIELD_NAME + "} : ${" + KEY_VALUE + "}")
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
                    .withMessageTemplate("EquipmentModifiedReportExceptionf_" + reports.size(), "        Cannot modify equipment ${" + KEY_EQPT_NAME + "} : ${" + KEY_VALUE + "}")
                    .withUntypedValue(KEY_EQPT_NAME, identifiable.getId())
                    .withUntypedValue(KEY_VALUE, e.getMessage())
                    .withSeverity(TypedValue.TRACE_SEVERITY)
                    .build());
        }
    }

    protected abstract boolean preCheckValue(Identifiable<?> identifiable, AbstractModificationByFilterInfos filterModificationInfos,
                                             List<ReportNode> reports, List<String> notEditableEquipments);

    protected abstract Object applyValue(Identifiable<?> identifiable, AbstractModificationByFilterInfos filterModificationInfos);

}
