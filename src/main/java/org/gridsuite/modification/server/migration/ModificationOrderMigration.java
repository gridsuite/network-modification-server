package org.gridsuite.modification.server.migration;

import liquibase.change.custom.CustomSqlChange;
import liquibase.database.Database;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.CustomChangeException;
import liquibase.exception.DatabaseException;
import liquibase.exception.SetupException;
import liquibase.exception.ValidationErrors;
import liquibase.resource.ResourceAccessor;
import liquibase.statement.SqlStatement;
import liquibase.statement.core.UpdateStatement;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.IntStream;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
public class ModificationOrderMigration implements CustomSqlChange {

    private static final Logger LOGGER = LoggerFactory.getLogger(ModificationOrderMigration.class);

    @Override
    public SqlStatement[] generateStatements(Database database) throws CustomChangeException {
        JdbcConnection connection = (JdbcConnection) database.getConnection();
        List<SqlStatement> statements = new ArrayList<>();
        try (PreparedStatement stmt = connection.prepareStatement("select distinct group_id from modification where group_id is not null")) {
            ResultSet groupIds = stmt.executeQuery();
            while (groupIds.next()) {
                UUID groupId = UUID.fromString(groupIds.getString(1));
                reorderNetworkModifications(groupId, true, connection, statements, database);
                reorderNetworkModifications(groupId, false, connection, statements, database);
            }
        } catch (SQLException | DatabaseException e) {
            throw new CustomChangeException(e);
        }
        return statements.toArray(new SqlStatement[0]);
    }

    private void reorderNetworkModifications(UUID groupId, boolean stashed, JdbcConnection connection, List<SqlStatement> statements, Database database) throws CustomChangeException {
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
        createMigrationRequests(entitiesToUpdate, statements, database);
    }

    private List<UUID> findAllByGroupId(UUID groupId, boolean stashed, JdbcConnection connection) throws CustomChangeException {
        try (PreparedStatement stmt = connection.prepareStatement("SELECT id FROM modification m WHERE m.group_id = ? AND m.stashed = ? order by modifications_order")) {
            stmt.setObject(1, groupId);
            stmt.setBoolean(2, stashed);
            ResultSet resultSet = stmt.executeQuery();
            List<UUID> entities = new ArrayList<>();
            while (resultSet.next()) {
                entities.add(UUID.fromString(resultSet.getString(1)));
            }
            return entities;
        } catch (SQLException | DatabaseException e) {
            throw new CustomChangeException(e);
        }
    }

    private void createMigrationRequests(List<Pair<UUID, Integer>> entities, List<SqlStatement> statements, Database database) {
        entities.forEach(pair -> statements.add(new UpdateStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "modification")
            .addNewColumnValue("modifications_order", pair.getValue())
            .setWhereClause(String.format("id='%s'", pair.getKey()))));
    }

    @Override
    public String getConfirmationMessage() {
        return "modification order was successfully updated";
    }

    @Override
    public void setUp() throws SetupException {
        LOGGER.info("Set up migration for modification order");
    }

    @Override
    public void setFileOpener(ResourceAccessor resourceAccessor) {
        LOGGER.info("Set file opener for modification order");
    }

    @Override
    public ValidationErrors validate(Database database) {
        return new ValidationErrors();
    }
}
