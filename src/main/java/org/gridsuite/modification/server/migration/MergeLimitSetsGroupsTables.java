package org.gridsuite.modification.server.migration;

import liquibase.change.custom.CustomSqlChange;
import liquibase.database.Database;
import liquibase.database.jvm.JdbcConnection;
import liquibase.exception.CustomChangeException;
import liquibase.exception.SetupException;
import liquibase.exception.ValidationErrors;
import liquibase.resource.ResourceAccessor;
import liquibase.statement.SqlStatement;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

public class MergeLimitSetsGroupsTables implements CustomSqlChange {

    private static final Logger LOGGER = LoggerFactory.getLogger(MergeLimitSetsGroupsTables.class);

    private String createQueryLineOpLimitsGroups1(String id) {
        return "select operational_limits_groups_id, pos_operational_limits_groups from line_creation_operational_limits_groups1" +
            " where branch_id = '" + id + "'";
    }

    private String createQueryLineOpLimitsGroups2( String id, String pos) {
        return "select operational_limits_groups_id, pos_operational_limits_groups from line_creation_operational_limits_groups2" +
            " where branch_id = '" + id + "' and pos_operational_limits_groups = '" + pos + "'";
    }

    private boolean compareOperationalLimitsInfos(String operationalLimitsIds1, String operationalLimitsIds2) {
        return false;
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
                    ResultSet operationalLimitsGroups1 = connection.createStatement()
                        .executeQuery(createQueryLineOpLimitsGroups1(lines.getString("id")));

                    while (operationalLimitsGroups1.next()) {
                        String operationalLimitsGroup1Id = operationalLimitsGroups1.getString("operational_limits_groups_id");
                        String operationalLimitsGroup1Pos = operationalLimitsGroups1.getString("pos_operational_limits_groups");

                        ResultSet operationalLimitsGroups2 = connection.createStatement()
                            .executeQuery(createQueryLineOpLimitsGroups2(lines.getString("id"), operationalLimitsGroup1Pos));

                        operationalLimitsGroups2.next();
                        String operationalLimitsGroup2Id = operationalLimitsGroups2.getString("operational_limits_groups");

                        //get id of operationalLimits 2 with

                        // get information on LimitsGroup in operational_limits_group
                        ResultSet limitSGroup1Informations = connection.createStatement()
                            .executeQuery("select id, current_limits_id where uuid = '" + operationalLimitsGroup1Id + "'");

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
