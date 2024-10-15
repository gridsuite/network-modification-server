package org.gridsuite.modification.server.migration;

import liquibase.change.custom.CustomTaskChange;
import liquibase.database.Database;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.CustomChangeException;
import liquibase.exception.DatabaseException;
import liquibase.exception.SetupException;
import liquibase.exception.ValidationErrors;
import liquibase.resource.ResourceAccessor;
import org.apache.commons.lang3.tuple.Pair;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.IntStream;

public class ModificationOrderMigration implements CustomTaskChange {

    @Override
    public void execute(Database database) throws CustomChangeException {
        JdbcConnection connection = (JdbcConnection) database.getConnection();
        try {
            ResultSet groupIds = connection.createStatement().executeQuery("select distinct group_id from modification");
            while (groupIds.next()) {
                UUID groupId = UUID.fromString(groupIds.getString(1));
                reorderNetworkModifications(groupId, true, connection);
                reorderNetworkModifications(groupId, false, connection);
            }
        } catch (SQLException | DatabaseException e) {
            throw new CustomChangeException(e);
        }
    }

    private void reorderNetworkModifications(UUID groupId, boolean stashed, JdbcConnection connection) throws SQLException, DatabaseException {
        List<UUID> entities = findAllByGroupId(groupId, stashed, connection);
        List<Pair<UUID, Integer>> entitiesToUpdate = new ArrayList<>();
        if (!entities.isEmpty()) {
            if (Boolean.TRUE.equals(stashed)) {
                IntStream.range(1, entities.size() + 1)
                    .forEach(i -> entitiesToUpdate.add(Pair.of(entities.get(i - 1), -i)));
            } else {
                IntStream.range(0, entities.size())
                    .forEach(i -> entitiesToUpdate.add(Pair.of(entities.get(i), i)));
            }
        }
        saveAll(entitiesToUpdate, connection);
    }

    private List<UUID> findAllByGroupId(UUID groupId, boolean stashed, JdbcConnection connection) throws DatabaseException, SQLException {
        ResultSet resultSet = connection.createStatement().executeQuery(String.format("SELECT id FROM modification m WHERE m.group_id = '%s' AND m.stashed = %b order by modifications_order", groupId, stashed));
        List<UUID> entities = new ArrayList<>();
        while (resultSet.next()) {
            entities.add(UUID.fromString(resultSet.getString(1)));
        }
        return entities;
    }

    private void saveAll(List<Pair<UUID, Integer>> entities, JdbcConnection connection) throws DatabaseException, SQLException {
        Statement statement = connection.createStatement();
        entities.forEach(pair -> {
            try {
                statement.addBatch(String.format("UPDATE modification SET modifications_order=%d WHERE id = '%s'", pair.getValue(), pair.getKey()));
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }

        });
        statement.executeBatch();
    }

    @Override
    public String getConfirmationMessage() {
        return "";
    }

    @Override
    public void setUp() throws SetupException {

    }

    @Override
    public void setFileOpener(ResourceAccessor resourceAccessor) {

    }

    @Override
    public ValidationErrors validate(Database database) {
        return null;
    }
}
