package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.dto.LineSegmentInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * @author El Cheikh Bassel <bassel.el-cheikh_externe at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table(name = "line_segments")
@AllArgsConstructor
public class LineSegmentEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "uuid")
    private UUID uuid;

    @Column(name = "segment_type_id")
    private String segmentTypeId;

    @Column(name = "segment_distance_value")
    private Integer segmentDistanceValue;

    @Column(name = "area")
    private String area;

    @Column(name = "temperature")
    private String temperature;

    @Column(name = "shape_factor")
    private Integer shapeFactor;

    public static List<LineSegmentEntity> toLineSegmentEntities(List<LineSegmentInfos> lineSegmentInfos) {
        List<LineSegmentEntity> lineSegments = new ArrayList<>();
        if (CollectionUtils.isEmpty(lineSegmentInfos)) {
            return lineSegments;
        }
        for (LineSegmentInfos segment : lineSegmentInfos) {
            lineSegments.add(new LineSegmentEntity(null,
                                                segment.segmentTypeId(),
                                                segment.segmentDistanceValue(),
                                                segment.area(),
                                                segment.temperature(),
                                                segment.shapeFactor()));
        }
        return lineSegments;
    }

    public static List<LineSegmentInfos> fromLineSegmentsEntity(List<LineSegmentEntity> lineSegmentEntities) {
        List<LineSegmentInfos> lineSegments = new ArrayList<>();
        if (CollectionUtils.isEmpty(lineSegmentEntities)) {
            return lineSegments;
        }
        for (LineSegmentEntity entity : lineSegmentEntities) {
            lineSegments.add(
                new LineSegmentInfos(entity.segmentTypeId,
                    entity.segmentDistanceValue,
                    entity.area, entity.temperature,
                    entity.shapeFactor));
        }
        return lineSegments;
    }
}
