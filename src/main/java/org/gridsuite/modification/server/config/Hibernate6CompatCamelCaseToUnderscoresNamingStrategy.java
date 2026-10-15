package org.gridsuite.modification.server.config;

import org.hibernate.boot.model.naming.Identifier;
import org.hibernate.boot.model.naming.PhysicalNamingStrategy;
import org.hibernate.engine.jdbc.env.spi.JdbcEnvironment;

import java.util.Locale;

// Compatibility copy of Hibernate ORM 6.6's CamelCaseToUnderscoresNamingStrategy.
// In Hibernate 7 this class was changed (breaking change)
// to better convert names with digits, but this means changing the actual database names.
// Keep the previous names to allow to change the database later in a smaller change.
// We should remove this class and the associated yaml config when we do the renames to
// match the new better Hibernate's default naming strategy.
// see https://hibernate.atlassian.net/browse/HHH-17310
// Example before/after:
// hibernate 6:
//  ratedU1  -> ratedu1
//  lineToAttachTo1Id ->   line_to_attach_to1id
// hibernate 7:
//  ratedU1  -> rated_u1
//  lineToAttachTo1Id -> line_to_attach_to1_id
public class Hibernate6CompatCamelCaseToUnderscoresNamingStrategy implements PhysicalNamingStrategy {

    @Override
    public Identifier toPhysicalCatalogName(Identifier logicalName, JdbcEnvironment jdbcEnvironment) {
        return apply(logicalName, jdbcEnvironment);
    }

    @Override
    public Identifier toPhysicalSchemaName(Identifier logicalName, JdbcEnvironment jdbcEnvironment) {
        return apply(logicalName, jdbcEnvironment);
    }

    @Override
    public Identifier toPhysicalTableName(Identifier logicalName, JdbcEnvironment jdbcEnvironment) {
        return apply(logicalName, jdbcEnvironment);
    }

    @Override
    public Identifier toPhysicalSequenceName(Identifier logicalName, JdbcEnvironment jdbcEnvironment) {
        return apply(logicalName, jdbcEnvironment);
    }

    @Override
    public Identifier toPhysicalColumnName(Identifier logicalName, JdbcEnvironment jdbcEnvironment) {
        return apply(logicalName, jdbcEnvironment);
    }

    private Identifier apply(final Identifier name, final JdbcEnvironment jdbcEnvironment) {
        if (name == null) {
            return null;
        }
        StringBuilder builder = new StringBuilder(name.getText().replace('.', '_'));
        for (int i = 1; i < builder.length() - 1; i++) {
            if (isUnderscoreRequired(builder.charAt(i - 1), builder.charAt(i), builder.charAt(i + 1))) {
                builder.insert(i++, '_');
            }
        }
        return getIdentifier(builder.toString(), name.isQuoted(), jdbcEnvironment);
    }

    protected Identifier getIdentifier(String name, final boolean quoted, final JdbcEnvironment jdbcEnvironment) {
        String identifierName = name;
        if (isCaseInsensitive(jdbcEnvironment)) {
            identifierName = name.toLowerCase(Locale.ROOT);
        }
        return new Identifier(identifierName, quoted);
    }

    protected boolean isCaseInsensitive(JdbcEnvironment jdbcEnvironment) {
        return true;
    }

    private boolean isUnderscoreRequired(final char before, final char current, final char after) {
        return Character.isLowerCase(before) && Character.isUpperCase(current) && Character.isLowerCase(after);
    }
}
