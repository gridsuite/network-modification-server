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
import liquibase.statement.core.DeleteStatement;
import liquibase.statement.core.InsertStatement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class MergeLimitSetsGroupsTables implements CustomSqlChange {

    private static final Logger LOGGER = LoggerFactory.getLogger(MergeLimitSetsGroupsTables.class);
    private static final String UUID_COL = "uuid";
    private static final String ID_COL = "id";
    private static final String OPERATIONAL_LG_ID = "operational_limits_groups_id";
    private static final String CURRENT_LIMITS_ID = "current_limits_id";

    private String createQueryLineOpLimitsGroups(String id, String tableName) {
        return "select * from " + tableName + " where branch_id = '" + id + "'";
    }

    private List<String> getOperationalLimitsGroupsNames(JdbcConnection connection, String id) throws SQLException, DatabaseException {
        ArrayList<String> names = new ArrayList<>();
        ResultSet resultSet = connection.createStatement().executeQuery(createQueryLineOpLimitsGroups(id, "line_creation_operational_limits_groups1"));
        while (resultSet.next()) {
            String query = "select id from operational_limits_group where uuid = '" + resultSet.getString("operational_limits_groups_id") + "'";
            ResultSet resultSetOp = connection.createStatement().executeQuery(query);
            if (resultSetOp.next()) {
                names.add(resultSetOp.getString(ID_COL));
            }
        }
        return names;
    }

    private String getLimitsGroupId(JdbcConnection connection, String id) throws DatabaseException, SQLException {
        String query = "select id from operational_limits_group where uuid = '" + id + "'";
        ResultSet resultSetOp = connection.createStatement().executeQuery(query);
        if (resultSetOp.next()) {
            return resultSetOp.getString(ID_COL);
        }
        return "";
    }

    private String createQueryLineOpLimitsGroupsWithPos(String tableName, String id, String pos) {
        return "select * from " + tableName + " where branch_id = '" + id + "' and pos_operational_limits_groups = " + pos;
    }

    private int getLength(ResultSet resultSet) throws SQLException {
        resultSet.last();
        int length = resultSet.getRow();
        resultSet.beforeFirst();
        return length;
    }

    private boolean compareOperationalLimitsInfos(JdbcConnection connection, String operationalLimitsIds1, String operationalLimitsIds2) throws DatabaseException, SQLException {
        final String query = "select current_limits_id from operational_limits_group where uuid ='<id>'";
        ResultSet currentLimits1 = connection.createStatement().executeQuery(query.replace("<id>", operationalLimitsIds1));
        ResultSet currentLimits2 = connection.createStatement().executeQuery(query.replace("<id>", operationalLimitsIds2));

        if (currentLimits1.next() && currentLimits2.next()) {
            String currentLimitsQuery = "select permanent_limit from current_limits where id = '<id>'";
            String currentLimitsId1 = currentLimits1.getString(CURRENT_LIMITS_ID);
            String currentLimitsId2 = currentLimits2.getString(CURRENT_LIMITS_ID);
            ResultSet permanentLimit = connection.createStatement().executeQuery(currentLimitsQuery.replace("<id>", currentLimitsId1));
            ResultSet permanentLimit2 = connection.createStatement().executeQuery(currentLimitsQuery.replace("<id>", currentLimitsId2));

            boolean haspermanentLimit = permanentLimit.next();
            boolean haspermanentLimit2 = permanentLimit2.next();
            if (haspermanentLimit != haspermanentLimit2) {
                return false;
            }
            if (haspermanentLimit && !permanentLimit.getString("permanent_limit").equals(permanentLimit2.getString("permanent_limit"))) {
                return false;
            }

            final String queryTemporary = "select * from current_temporary_limits where id ='<id>'";
            Statement statement1 = connection.createStatement(ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY);
            Statement statement2 = connection.createStatement(ResultSet.TYPE_SCROLL_SENSITIVE, ResultSet.CONCUR_READ_ONLY);
            ResultSet temporaryLimit = statement1.executeQuery(queryTemporary.replace("<id>", currentLimitsId1));
            ResultSet temporaryLimit2 = statement2.executeQuery(queryTemporary.replace("<id>", currentLimitsId2));
            int temporaryLimitSize = getLength(temporaryLimit);
            int temporaryLimit2Size = getLength(temporaryLimit2);
            if (temporaryLimitSize != temporaryLimit2Size) {
                return false;
            }

            while (temporaryLimit.next()) {
                String name = temporaryLimit.getString("name");
                boolean found = false;
                temporaryLimit2.beforeFirst();
                while (temporaryLimit2.next()) {
                    if (temporaryLimit2.getString("name").equals(name)) {
                        if (!temporaryLimit.getString("value_").equals(temporaryLimit2.getString("value_")) ||
                            !temporaryLimit.getString("acceptable_duration").equals(temporaryLimit2.getString("acceptable_duration"))) {
                            return false;
                        }
                        found = true;
                        break;
                    }
                }
                // temporary limit not found
                if (!found) {
                    return false;
                }
            }
        }

        return true;
    }

    private String generateNewName(List<String> oldNames, List<String> newNames, String baseName, String suffix) throws SQLException {
        int index = 1;
        String strSuffix = "";
        String newName = baseName + suffix + strSuffix;
        while (oldNames.contains(newName) || newNames.contains(newName)) {
            index++;
            strSuffix = "(" + index + ")";
            newName = baseName + suffix + strSuffix;
        }
        return newName;
    }

    private void changeIdOperationalLimitsGroup(Database database, ResultSet operationalLimitsGroups,
                                                List<SqlStatement> statements, String name, String applicability) throws SQLException {

        //Delete statement
        if (operationalLimitsGroups.next()) {
            statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "operational_limits_group")
                .setWhereClause(UUID_COL + " = '" + operationalLimitsGroups.getString(UUID_COL) + "'"));

            //Add statement
            statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "operational_limits_group")
                .addColumnValue(UUID_COL, operationalLimitsGroups.getString(UUID_COL))
                .addColumnValue("id", name)
                .addColumnValue("current_limits_id", operationalLimitsGroups.getString("current_limits_id"))
                .addColumnValue("applicability", applicability));
        }
    }

    @Override
    public SqlStatement[] generateStatements(Database database) throws CustomChangeException {
        JdbcConnection connection = (JdbcConnection) database.getConnection();

        List<SqlStatement> statements = new ArrayList<>();
        String linesToProcess = "Select id from line_creation";
        try {
            try (ResultSet lines = connection.createStatement().executeQuery(linesToProcess)) {
                while (lines.next()) {
                    //get operational limits groups1
                    ResultSet lineCreationOpLimitsGroups1 = connection.createStatement()
                        .executeQuery(createQueryLineOpLimitsGroups(lines.getString("id"), "line_creation_operational_limits_groups1"));
                    List<String> oldNames = getOperationalLimitsGroupsNames(connection, lines.getString(ID_COL));
                    ArrayList<String> newNames = new ArrayList<>();

                    while (lineCreationOpLimitsGroups1.next()) {
                        String lineCreationOpLimitsGroup1Id = lineCreationOpLimitsGroups1.getString(OPERATIONAL_LG_ID);
                        String lineCreationOpLimitsGroup1Pos = lineCreationOpLimitsGroups1.getString("pos_operational_limits_groups");

                        ResultSet lineCreationOpLimitsGroups2 = connection.createStatement()
                            .executeQuery(createQueryLineOpLimitsGroupsWithPos("line_creation_operational_limits_groups2", lines.getString("id"), lineCreationOpLimitsGroup1Pos));

                        lineCreationOpLimitsGroups2.next();
                        String lineCreationOpLimitsGroup2Id = lineCreationOpLimitsGroups2.getString(OPERATIONAL_LG_ID);

                        // Compare Both limitsGroups 1 and 2 limits
                        if (compareOperationalLimitsInfos(connection, lineCreationOpLimitsGroup1Id, lineCreationOpLimitsGroup2Id)) {
                            ResultSet operationalLimitsGroups2 = connection.createStatement().executeQuery("select current_limits_id from operational_limits_group where " + UUID_COL + " = '" + lineCreationOpLimitsGroup2Id + "'");

                            // - remove line from operational_limits_group
                            // - remove related permanent limit from current_limits
                            // - remove all other related limits from current_temporary_limits
                            if (operationalLimitsGroups2.next()) {
                                String currentLimitId = operationalLimitsGroups2.getString("current_limits_id");
                                statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "current_limits")
                                    .setWhere(UUID_COL + " = '" + currentLimitId + "'"));
                                statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "current_temporary_limits")
                                    .setWhere(UUID_COL + " = '" + currentLimitId + "'"));
                                statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "operational_limits_group")
                                    .setWhere(UUID_COL + " = '" + lineCreationOpLimitsGroup2Id + "'"));
                            }
                            // if they are equal then add only one limitGroup in new Table with application side = equipment (both)
                            statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "line_creation_operational_limits_groups")
                                .addColumnValue("branch_id", lines.getString("id"))
                                .addColumnValue(OPERATIONAL_LG_ID, lineCreationOpLimitsGroups1.getString(OPERATIONAL_LG_ID))
                                .addColumnValue("pos_operational_limits_groups", lineCreationOpLimitsGroups1.getString("pos_operational_limits_groups"))
                                .addColumnValue("applicability", "EQUIPMENT"));
                        } else {
                            //change their names in operational_limits_group
                            String oldName = getLimitsGroupId(connection, lineCreationOpLimitsGroup1Id);
                            String newName = generateNewName(oldNames, newNames, oldName, "_OR");
                            newNames.add(newName);
                            ResultSet operationalLimitsGroups1 = connection.createStatement().executeQuery("select * from operational_limits_group where " + UUID_COL + " = '" + lineCreationOpLimitsGroup1Id + "'");
                            changeIdOperationalLimitsGroup(database, operationalLimitsGroups1, statements, newName, "SIDE1");

                            statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "line_creation_operational_limits_groups")
                                .addColumnValue("branch_id", lines.getString("id"))
                                .addColumnValue(OPERATIONAL_LG_ID, lineCreationOpLimitsGroups1.getString(OPERATIONAL_LG_ID))
                                .addColumnValue("pos_operational_limits_groups", lineCreationOpLimitsGroups1.getString("pos_operational_limits_groups")));

                            oldName = getLimitsGroupId(connection, lineCreationOpLimitsGroup1Id);
                            newName = generateNewName(oldNames, newNames, oldName, "_EX");
                            newNames.add(newName);
                            ResultSet operationalLimitsGroups2 = connection.createStatement().executeQuery("select * from operational_limits_group where " + UUID_COL + " = '" + lineCreationOpLimitsGroup2Id + "'");
                            changeIdOperationalLimitsGroup(database, operationalLimitsGroups2, statements, newName, "SIDE2");

                            statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "line_creation_operational_limits_groups")
                                .addColumnValue("branch_id", lines.getString("id"))
                                .addColumnValue(OPERATIONAL_LG_ID, lineCreationOpLimitsGroups2.getString(OPERATIONAL_LG_ID))
                                .addColumnValue("pos_operational_limits_groups", lineCreationOpLimitsGroups2.getString("pos_operational_limits_groups")));
                        }
                    }
                }
            }
        } catch (Exception throwables) {
            LOGGER.error(throwables.getMessage());
            return new SqlStatement[0]; // If any exception occurs don't do any migration
        }
        return statements.toArray(new SqlStatement[0]);
    }

    @Override
    public String getConfirmationMessage() {
        return "tables line_creation_operational_limits_group1 and 2, merged successfully into line_creation_operational_limits_group";
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
