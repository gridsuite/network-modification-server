package org.gridsuite.modification.server.utils;

import org.gridsuite.modification.server.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitModificationInfos;

import java.util.List;
import java.util.Objects;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 * Original code by Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public final class MatcherUtils {

    private MatcherUtils() {
    }

    public static boolean matchesCurrentLimits(CurrentLimitsModificationInfos limits1, CurrentLimitsModificationInfos limits2) {
        return Objects.equals(limits1.getPermanentLimit(), limits2.getPermanentLimit())
                && matchesTemporaryLimits(limits1.getTemporaryLimits(), limits2.getTemporaryLimits());
    }

    public static boolean matchesTemporaryLimits(List<CurrentTemporaryLimitModificationInfos> tempLimits1, List<CurrentTemporaryLimitModificationInfos> tempLimits2) {
        if (tempLimits1 == null && tempLimits2 == null) {
            return true;
        }
        if (tempLimits1 == null || tempLimits2 == null) {
            if (tempLimits1 == null && tempLimits2 != null && tempLimits2.isEmpty()) {
                return true;
            } else {
                return tempLimits2 == null && tempLimits1 != null && tempLimits1.isEmpty();
            }
        }
        if (tempLimits1.size() != tempLimits2.size()) {
            return false;
        }
        for (int i = 0; i < tempLimits1.size(); i++) {
            if (!Objects.equals(tempLimits1.get(i).getAcceptableDuration(), tempLimits2.get(i).getAcceptableDuration())
                    || !Objects.equals(tempLimits1.get(i).getName(), tempLimits2.get(i).getName())
                    || !Objects.equals(tempLimits1.get(i).getValue(), tempLimits2.get(i).getValue())) {
                return false;
            }
        }
        return true;
    }
}
