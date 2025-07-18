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
import liquibase.statement.core.UpdateStatement;
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
    private static final String OPERATIONAL_LG_ID_COL = "operational_limits_groups_id";
    private static final String CURRENT_LIMITS_ID_COL = "current_limits_id";
    private static final String POS_OP_LG_COL = "pos_operational_limits_groups";
    private static final String OPERATIONAL_LIMITS_GROUPS_TABLE = "operational_limits_group";
    private static final String BRANCH_ID_COL = "branch_id";

    private String createQueryLineOpLimitsGroups(String id, String tableName) {
        return "select * from " + tableName + " where branch_id = '" + id + "'";
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
            String currentLimitsId1 = currentLimits1.getString(CURRENT_LIMITS_ID_COL);
            String currentLimitsId2 = currentLimits2.getString(CURRENT_LIMITS_ID_COL);
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

    private void addOperationalLimitsGroupApplicability(Database database, ResultSet operationalLimitsGroups,
                                                List<SqlStatement> statements, String applicability) throws SQLException {

        if (operationalLimitsGroups.next()) {
            statements.add(new UpdateStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), OPERATIONAL_LIMITS_GROUPS_TABLE)
                .setWhereClause(UUID_COL + " = '" + operationalLimitsGroups.getString(UUID_COL) + "'")
                .addNewColumnValue("applicability", applicability));
        }
    }

    @Override
    public SqlStatement[] generateStatements(Database database) throws CustomChangeException {
        JdbcConnection connection = (JdbcConnection) database.getConnection();

        List<SqlStatement> statements = new ArrayList<>();

        try {
            for (int i = 0; i < 2; i++) {
                // Define tables names ( i= 0 => lines, i=1 => twoWindingsTransformers)
                final String branchCreationTable = i == 0 ? "line_creation" : "two_windings_transformer_creation";
                final String branchCreationOpLimitsGroups1Table = i == 0 ? "line_creation_operational_limits_groups1"
                    : "two_windings_transformer_creation_operational_limits_groups1";
                final String branchCreationOpLimitsGroups2Table = i == 0 ? "line_creation_operational_limits_groups2"
                    : "two_windings_transformer_creation_operational_limits_groups2";
                final String branchCreationOpLimitsGroupsTable = i == 0 ? "line_creation_operational_limits_groups"
                    : "two_windings_transformer_creation_operational_limits_groups";

                String branchesToProcess = "Select id, selected_operational_limits_group_id1, selected_operational_limits_group_id2  from " + branchCreationTable;
                try (ResultSet branches = connection.createStatement().executeQuery(branchesToProcess)) {
                    while (branches.next()) {
                        int position = 0;
                        //get operational limits groups1
                        ResultSet branchCreationOpLimitsGroups1 = connection.createStatement()
                            .executeQuery(createQueryLineOpLimitsGroups(branches.getString(ID_COL), branchCreationOpLimitsGroups1Table));

                        while (branchCreationOpLimitsGroups1.next()) {
                            String branchCreationOpLimitsGroup1Id = branchCreationOpLimitsGroups1.getString(OPERATIONAL_LG_ID_COL);
                            String branchCreationOpLimitsGroup1Pos = branchCreationOpLimitsGroups1.getString(POS_OP_LG_COL);

                            ResultSet branchCreationOpLimitsGroups2 = connection.createStatement()
                                .executeQuery(createQueryLineOpLimitsGroupsWithPos(branchCreationOpLimitsGroups2Table, branches.getString(ID_COL),
                                    branchCreationOpLimitsGroup1Pos));

                            branchCreationOpLimitsGroups2.next();
                            String branchCreationOpLimitsGroup2Id = branchCreationOpLimitsGroups2.getString(OPERATIONAL_LG_ID_COL);

                            // Compare Both limitsGroups 1 and 2 limits
                            ResultSet operationalLimitsGroups1 = connection.createStatement().executeQuery("select * from operational_limits_group where " + UUID_COL + " = '" + branchCreationOpLimitsGroup1Id + "'");

                            if (compareOperationalLimitsInfos(connection, branchCreationOpLimitsGroup1Id, branchCreationOpLimitsGroup2Id)) {
                                ResultSet operationalLimitsGroups2 = connection.createStatement().executeQuery("select current_limits_id from operational_limits_group where " + UUID_COL + " = '" + branchCreationOpLimitsGroup2Id + "'");

                                // - remove line from operational_limits_group
                                // - remove related permanent limit from current_limits
                                // - remove all other related limits from current_temporary_limits
                                if (operationalLimitsGroups2.next()) {
                                    statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), OPERATIONAL_LIMITS_GROUPS_TABLE)
                                        .setWhere(UUID_COL + " = '" + branchCreationOpLimitsGroup2Id + "'"));
                                    String currentLimitId = operationalLimitsGroups2.getString(CURRENT_LIMITS_ID_COL);
                                    statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "current_temporary_limits")
                                        .setWhere(ID_COL + " = '" + currentLimitId + "'"));
                                    statements.add(new DeleteStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), "current_limits")
                                        .setWhere(ID_COL + " = '" + currentLimitId + "'"));
                                }

                                addOperationalLimitsGroupApplicability(database, operationalLimitsGroups1, statements, "EQUIPMENT");

                                // if they are equal then add only one limitGroup in new Table with application side = equipment (both)
                                statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), branchCreationOpLimitsGroupsTable)
                                    .addColumnValue(BRANCH_ID_COL, branches.getString(ID_COL))
                                    .addColumnValue(OPERATIONAL_LG_ID_COL, branchCreationOpLimitsGroup1Id)
                                    .addColumnValue(POS_OP_LG_COL, position++));
                            } else {
                                // change Applicability side 1
                                addOperationalLimitsGroupApplicability(database, operationalLimitsGroups1, statements, "SIDE1");

                                // Add to merged table
                                statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), branchCreationOpLimitsGroupsTable)
                                    .addColumnValue(BRANCH_ID_COL, branches.getString(ID_COL))
                                    .addColumnValue(OPERATIONAL_LG_ID_COL, branchCreationOpLimitsGroup1Id)
                                    .addColumnValue(POS_OP_LG_COL, position++));

                                // Change Applicability side 2
                                ResultSet operationalLimitsGroups2 = connection.createStatement().executeQuery("select * from operational_limits_group where " + UUID_COL + " = '" + branchCreationOpLimitsGroup2Id + "'");
                                addOperationalLimitsGroupApplicability(database, operationalLimitsGroups2, statements, "SIDE2");

                                // Add to merged table
                                statements.add(new InsertStatement(database.getDefaultCatalogName(), database.getDefaultSchemaName(), branchCreationOpLimitsGroupsTable)
                                    .addColumnValue(BRANCH_ID_COL, branches.getString(ID_COL))
                                    .addColumnValue(OPERATIONAL_LG_ID_COL, branchCreationOpLimitsGroup2Id)
                                    .addColumnValue(POS_OP_LG_COL, position++));
                            }
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
