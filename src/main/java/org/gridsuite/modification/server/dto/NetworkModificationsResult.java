package org.gridsuite.modification.server.dto;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public record NetworkModificationsResult(List<UUID> modificationUuids, List<Optional<NetworkModificationResult>> modificationResults) { }
