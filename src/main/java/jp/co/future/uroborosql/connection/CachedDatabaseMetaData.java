/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.RowIdLifetime;
import java.sql.SQLException;

/**
 * 接続後に変更されない項目をキャッシュするDatabaseMetadataを提供するためのWrapper
 *
 * @author H.Sugimoto
 * @since v0.26.10
 */
public class CachedDatabaseMetaData implements DatabaseMetaData {
	private final DatabaseMetaData originalMetaData;
	private final Connection originalConn;

	/** Cached databaseMajorVersion */
	private Integer databaseMajorVersion = null;

	/** Cached databaseMinorVersion */
	private Integer databaseMinorVersion = null;

	/** Cached databaseProductName */
	private String databaseProductName = null;

	/** Cached databaseProductVersion */
	private String databaseProductVersion = null;

	/** Cached url */
	private String url = null;

	CachedDatabaseMetaData(final DatabaseMetaData originalMetaData, final Connection originalConn) {
		this.originalMetaData = originalMetaData;
		this.originalConn = originalConn;
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T unwrap(final Class<T> iface) throws SQLException {
		if (!isWrapperFor(iface)) {
			throw new SQLException("Cannot unwrap to " + iface.getName());
		}
		if (iface.isInstance(this)) {
			return (T) this;
		}
		return originalMetaData.unwrap(iface);
	}

	@Override
	public boolean isWrapperFor(final Class<?> iface) throws SQLException {
		if (iface.isInstance(this)) {
			return true;
		}
		return originalMetaData.isWrapperFor(iface);
	}

	@Override
	public boolean allProceduresAreCallable() throws SQLException {
		return originalMetaData.allProceduresAreCallable();
	}

	@Override
	public boolean allTablesAreSelectable() throws SQLException {
		return originalMetaData.allTablesAreSelectable();
	}

	@Override
	public String getURL() throws SQLException {
		if (url == null) {
			url = originalMetaData.getURL();
		}
		return url;
	}

	@Override
	public String getUserName() throws SQLException {
		return originalMetaData.getUserName();
	}

	@Override
	public boolean isReadOnly() throws SQLException {
		return originalMetaData.isReadOnly();
	}

	@Override
	public boolean nullsAreSortedHigh() throws SQLException {
		return originalMetaData.nullsAreSortedHigh();
	}

	@Override
	public boolean nullsAreSortedLow() throws SQLException {
		return originalMetaData.nullsAreSortedLow();
	}

	@Override
	public boolean nullsAreSortedAtStart() throws SQLException {
		return originalMetaData.nullsAreSortedAtStart();
	}

	@Override
	public boolean nullsAreSortedAtEnd() throws SQLException {
		return originalMetaData.nullsAreSortedAtEnd();
	}

	@Override
	public String getDatabaseProductName() throws SQLException {
		if (databaseProductName == null) {
			databaseProductName = originalMetaData.getDatabaseProductName();
		}
		return databaseProductName;
	}

	@Override
	public String getDatabaseProductVersion() throws SQLException {
		if (databaseProductVersion == null) {
			databaseProductVersion = originalMetaData.getDatabaseProductVersion();
		}
		return databaseProductVersion;
	}

	@Override
	public String getDriverName() throws SQLException {
		return originalMetaData.getDriverName();
	}

	@Override
	public String getDriverVersion() throws SQLException {
		return originalMetaData.getDriverVersion();
	}

	@Override
	public int getDriverMajorVersion() {
		return originalMetaData.getDriverMajorVersion();
	}

	@Override
	public int getDriverMinorVersion() {
		return originalMetaData.getDriverMinorVersion();
	}

	@Override
	public boolean usesLocalFiles() throws SQLException {
		return originalMetaData.usesLocalFiles();
	}

	@Override
	public boolean usesLocalFilePerTable() throws SQLException {
		return originalMetaData.usesLocalFilePerTable();
	}

	@Override
	public boolean supportsMixedCaseIdentifiers() throws SQLException {
		return originalMetaData.supportsMixedCaseIdentifiers();
	}

	@Override
	public boolean storesUpperCaseIdentifiers() throws SQLException {
		return originalMetaData.storesUpperCaseIdentifiers();
	}

	@Override
	public boolean storesLowerCaseIdentifiers() throws SQLException {
		return originalMetaData.storesLowerCaseIdentifiers();
	}

	@Override
	public boolean storesMixedCaseIdentifiers() throws SQLException {
		return originalMetaData.storesMixedCaseIdentifiers();
	}

	@Override
	public boolean supportsMixedCaseQuotedIdentifiers() throws SQLException {
		return originalMetaData.supportsMixedCaseQuotedIdentifiers();
	}

	@Override
	public boolean storesUpperCaseQuotedIdentifiers() throws SQLException {
		return originalMetaData.storesUpperCaseQuotedIdentifiers();
	}

	@Override
	public boolean storesLowerCaseQuotedIdentifiers() throws SQLException {
		return originalMetaData.storesLowerCaseQuotedIdentifiers();
	}

	@Override
	public boolean storesMixedCaseQuotedIdentifiers() throws SQLException {
		return originalMetaData.storesMixedCaseQuotedIdentifiers();
	}

	@Override
	public String getIdentifierQuoteString() throws SQLException {
		return originalMetaData.getIdentifierQuoteString();
	}

	@Override
	public String getSQLKeywords() throws SQLException {
		return originalMetaData.getSQLKeywords();
	}

	@Override
	public String getNumericFunctions() throws SQLException {
		return originalMetaData.getNumericFunctions();
	}

	@Override
	public String getStringFunctions() throws SQLException {
		return originalMetaData.getStringFunctions();
	}

	@Override
	public String getSystemFunctions() throws SQLException {
		return originalMetaData.getSystemFunctions();
	}

	@Override
	public String getTimeDateFunctions() throws SQLException {
		return originalMetaData.getTimeDateFunctions();
	}

	@Override
	public String getSearchStringEscape() throws SQLException {
		return originalMetaData.getSearchStringEscape();
	}

	@Override
	public String getExtraNameCharacters() throws SQLException {
		return originalMetaData.getExtraNameCharacters();
	}

	@Override
	public boolean supportsAlterTableWithAddColumn() throws SQLException {
		return originalMetaData.supportsAlterTableWithAddColumn();
	}

	@Override
	public boolean supportsAlterTableWithDropColumn() throws SQLException {
		return originalMetaData.supportsAlterTableWithDropColumn();
	}

	@Override
	public boolean supportsColumnAliasing() throws SQLException {
		return originalMetaData.supportsColumnAliasing();
	}

	@Override
	public boolean nullPlusNonNullIsNull() throws SQLException {
		return originalMetaData.nullPlusNonNullIsNull();
	}

	@Override
	public boolean supportsConvert() throws SQLException {
		return originalMetaData.supportsConvert();
	}

	@Override
	public boolean supportsConvert(final int fromType, final int toType) throws SQLException {
		return originalMetaData.supportsConvert(fromType, toType);
	}

	@Override
	public boolean supportsTableCorrelationNames() throws SQLException {
		return originalMetaData.supportsTableCorrelationNames();
	}

	@Override
	public boolean supportsDifferentTableCorrelationNames() throws SQLException {
		return originalMetaData.supportsDifferentTableCorrelationNames();
	}

	@Override
	public boolean supportsExpressionsInOrderBy() throws SQLException {
		return originalMetaData.supportsExpressionsInOrderBy();
	}

	@Override
	public boolean supportsOrderByUnrelated() throws SQLException {
		return originalMetaData.supportsOrderByUnrelated();
	}

	@Override
	public boolean supportsGroupBy() throws SQLException {
		return originalMetaData.supportsGroupBy();
	}

	@Override
	public boolean supportsGroupByUnrelated() throws SQLException {
		return originalMetaData.supportsGroupByUnrelated();
	}

	@Override
	public boolean supportsGroupByBeyondSelect() throws SQLException {
		return originalMetaData.supportsGroupByBeyondSelect();
	}

	@Override
	public boolean supportsLikeEscapeClause() throws SQLException {
		return originalMetaData.supportsLikeEscapeClause();
	}

	@Override
	public boolean supportsMultipleResultSets() throws SQLException {
		return originalMetaData.supportsMultipleResultSets();
	}

	@Override
	public boolean supportsMultipleTransactions() throws SQLException {
		return originalMetaData.supportsMultipleTransactions();
	}

	@Override
	public boolean supportsNonNullableColumns() throws SQLException {
		return originalMetaData.supportsNonNullableColumns();
	}

	@Override
	public boolean supportsMinimumSQLGrammar() throws SQLException {
		return originalMetaData.supportsMinimumSQLGrammar();
	}

	@Override
	public boolean supportsCoreSQLGrammar() throws SQLException {
		return originalMetaData.supportsCoreSQLGrammar();
	}

	@Override
	public boolean supportsExtendedSQLGrammar() throws SQLException {
		return originalMetaData.supportsExtendedSQLGrammar();
	}

	@Override
	public boolean supportsANSI92EntryLevelSQL() throws SQLException {
		return originalMetaData.supportsANSI92EntryLevelSQL();
	}

	@Override
	public boolean supportsANSI92IntermediateSQL() throws SQLException {
		return originalMetaData.supportsANSI92IntermediateSQL();
	}

	@Override
	public boolean supportsANSI92FullSQL() throws SQLException {
		return originalMetaData.supportsANSI92FullSQL();
	}

	@Override
	public boolean supportsIntegrityEnhancementFacility() throws SQLException {
		return originalMetaData.supportsIntegrityEnhancementFacility();
	}

	@Override
	public boolean supportsOuterJoins() throws SQLException {
		return originalMetaData.supportsOuterJoins();
	}

	@Override
	public boolean supportsFullOuterJoins() throws SQLException {
		return originalMetaData.supportsFullOuterJoins();
	}

	@Override
	public boolean supportsLimitedOuterJoins() throws SQLException {
		return originalMetaData.supportsLimitedOuterJoins();
	}

	@Override
	public String getSchemaTerm() throws SQLException {
		return originalMetaData.getSchemaTerm();
	}

	@Override
	public String getProcedureTerm() throws SQLException {
		return originalMetaData.getProcedureTerm();
	}

	@Override
	public String getCatalogTerm() throws SQLException {
		return originalMetaData.getCatalogTerm();
	}

	@Override
	public boolean isCatalogAtStart() throws SQLException {
		return originalMetaData.isCatalogAtStart();
	}

	@Override
	public String getCatalogSeparator() throws SQLException {
		return originalMetaData.getCatalogSeparator();
	}

	@Override
	public boolean supportsSchemasInDataManipulation() throws SQLException {
		return originalMetaData.supportsSchemasInDataManipulation();
	}

	@Override
	public boolean supportsSchemasInProcedureCalls() throws SQLException {
		return originalMetaData.supportsSchemasInProcedureCalls();
	}

	@Override
	public boolean supportsSchemasInTableDefinitions() throws SQLException {
		return originalMetaData.supportsSchemasInTableDefinitions();
	}

	@Override
	public boolean supportsSchemasInIndexDefinitions() throws SQLException {
		return originalMetaData.supportsSchemasInIndexDefinitions();
	}

	@Override
	public boolean supportsSchemasInPrivilegeDefinitions() throws SQLException {
		return originalMetaData.supportsSchemasInPrivilegeDefinitions();
	}

	@Override
	public boolean supportsCatalogsInDataManipulation() throws SQLException {
		return originalMetaData.supportsCatalogsInDataManipulation();
	}

	@Override
	public boolean supportsCatalogsInProcedureCalls() throws SQLException {
		return originalMetaData.supportsCatalogsInProcedureCalls();
	}

	@Override
	public boolean supportsCatalogsInTableDefinitions() throws SQLException {
		return originalMetaData.supportsCatalogsInTableDefinitions();
	}

	@Override
	public boolean supportsCatalogsInIndexDefinitions() throws SQLException {
		return originalMetaData.supportsCatalogsInIndexDefinitions();
	}

	@Override
	public boolean supportsCatalogsInPrivilegeDefinitions() throws SQLException {
		return originalMetaData.supportsCatalogsInPrivilegeDefinitions();
	}

	@Override
	public boolean supportsPositionedDelete() throws SQLException {
		return originalMetaData.supportsPositionedDelete();
	}

	@Override
	public boolean supportsPositionedUpdate() throws SQLException {
		return originalMetaData.supportsPositionedUpdate();
	}

	@Override
	public boolean supportsSelectForUpdate() throws SQLException {
		return originalMetaData.supportsSelectForUpdate();
	}

	@Override
	public boolean supportsStoredProcedures() throws SQLException {
		return originalMetaData.supportsStoredProcedures();
	}

	@Override
	public boolean supportsSubqueriesInComparisons() throws SQLException {
		return originalMetaData.supportsSubqueriesInComparisons();
	}

	@Override
	public boolean supportsSubqueriesInExists() throws SQLException {
		return originalMetaData.supportsSubqueriesInExists();
	}

	@Override
	public boolean supportsSubqueriesInIns() throws SQLException {
		return originalMetaData.supportsSubqueriesInIns();
	}

	@Override
	public boolean supportsSubqueriesInQuantifieds() throws SQLException {
		return originalMetaData.supportsSubqueriesInQuantifieds();
	}

	@Override
	public boolean supportsCorrelatedSubqueries() throws SQLException {
		return originalMetaData.supportsCorrelatedSubqueries();
	}

	@Override
	public boolean supportsUnion() throws SQLException {
		return originalMetaData.supportsUnion();
	}

	@Override
	public boolean supportsUnionAll() throws SQLException {
		return originalMetaData.supportsUnionAll();
	}

	@Override
	public boolean supportsOpenCursorsAcrossCommit() throws SQLException {
		return originalMetaData.supportsOpenCursorsAcrossCommit();
	}

	@Override
	public boolean supportsOpenCursorsAcrossRollback() throws SQLException {
		return originalMetaData.supportsOpenCursorsAcrossRollback();
	}

	@Override
	public boolean supportsOpenStatementsAcrossCommit() throws SQLException {
		return originalMetaData.supportsOpenStatementsAcrossCommit();
	}

	@Override
	public boolean supportsOpenStatementsAcrossRollback() throws SQLException {
		return originalMetaData.supportsOpenStatementsAcrossRollback();
	}

	@Override
	public int getMaxBinaryLiteralLength() throws SQLException {
		return originalMetaData.getMaxBinaryLiteralLength();
	}

	@Override
	public int getMaxCharLiteralLength() throws SQLException {
		return originalMetaData.getMaxCharLiteralLength();
	}

	@Override
	public int getMaxColumnNameLength() throws SQLException {
		return originalMetaData.getMaxColumnNameLength();
	}

	@Override
	public int getMaxColumnsInGroupBy() throws SQLException {
		return originalMetaData.getMaxColumnsInGroupBy();
	}

	@Override
	public int getMaxColumnsInIndex() throws SQLException {
		return originalMetaData.getMaxColumnsInIndex();
	}

	@Override
	public int getMaxColumnsInOrderBy() throws SQLException {
		return originalMetaData.getMaxColumnsInOrderBy();
	}

	@Override
	public int getMaxColumnsInSelect() throws SQLException {
		return originalMetaData.getMaxColumnsInSelect();
	}

	@Override
	public int getMaxColumnsInTable() throws SQLException {
		return originalMetaData.getMaxColumnsInTable();
	}

	@Override
	public int getMaxConnections() throws SQLException {
		return originalMetaData.getMaxConnections();
	}

	@Override
	public int getMaxCursorNameLength() throws SQLException {
		return originalMetaData.getMaxCursorNameLength();
	}

	@Override
	public int getMaxIndexLength() throws SQLException {
		return originalMetaData.getMaxIndexLength();
	}

	@Override
	public int getMaxSchemaNameLength() throws SQLException {
		return originalMetaData.getMaxSchemaNameLength();
	}

	@Override
	public int getMaxProcedureNameLength() throws SQLException {
		return originalMetaData.getMaxProcedureNameLength();
	}

	@Override
	public int getMaxCatalogNameLength() throws SQLException {
		return originalMetaData.getMaxCatalogNameLength();
	}

	@Override
	public int getMaxRowSize() throws SQLException {
		return originalMetaData.getMaxRowSize();
	}

	@Override
	public boolean doesMaxRowSizeIncludeBlobs() throws SQLException {
		return originalMetaData.doesMaxRowSizeIncludeBlobs();
	}

	@Override
	public int getMaxStatementLength() throws SQLException {
		return originalMetaData.getMaxStatementLength();
	}

	@Override
	public int getMaxStatements() throws SQLException {
		return originalMetaData.getMaxStatements();
	}

	@Override
	public int getMaxTableNameLength() throws SQLException {
		return originalMetaData.getMaxTableNameLength();
	}

	@Override
	public int getMaxTablesInSelect() throws SQLException {
		return originalMetaData.getMaxTablesInSelect();
	}

	@Override
	public int getMaxUserNameLength() throws SQLException {
		return originalMetaData.getMaxUserNameLength();
	}

	@Override
	public int getDefaultTransactionIsolation() throws SQLException {
		return originalMetaData.getDefaultTransactionIsolation();
	}

	@Override
	public boolean supportsTransactions() throws SQLException {
		return originalMetaData.supportsTransactions();
	}

	@Override
	public boolean supportsTransactionIsolationLevel(final int level) throws SQLException {
		return originalMetaData.supportsTransactionIsolationLevel(level);
	}

	@Override
	public boolean supportsDataDefinitionAndDataManipulationTransactions() throws SQLException {
		return originalMetaData.supportsDataDefinitionAndDataManipulationTransactions();
	}

	@Override
	public boolean supportsDataManipulationTransactionsOnly() throws SQLException {
		return originalMetaData.supportsDataManipulationTransactionsOnly();
	}

	@Override
	public boolean dataDefinitionCausesTransactionCommit() throws SQLException {
		return originalMetaData.dataDefinitionCausesTransactionCommit();
	}

	@Override
	public boolean dataDefinitionIgnoredInTransactions() throws SQLException {
		return originalMetaData.dataDefinitionIgnoredInTransactions();
	}

	@Override
	public ResultSet getProcedures(final String catalog, final String schemaPattern, final String procedureNamePattern)
			throws SQLException {
		return originalMetaData.getProcedures(catalog, schemaPattern, procedureNamePattern);
	}

	@Override
	public ResultSet getProcedureColumns(final String catalog, final String schemaPattern,
			final String procedureNamePattern, final String columnNamePattern) throws SQLException {
		return originalMetaData.getProcedureColumns(catalog, schemaPattern, procedureNamePattern, columnNamePattern);
	}

	@Override
	public ResultSet getTables(final String catalog, final String schemaPattern, final String tableNamePattern,
			final String[] types) throws SQLException {
		return originalMetaData.getTables(catalog, schemaPattern, tableNamePattern, types);
	}

	@Override
	public ResultSet getSchemas() throws SQLException {
		return originalMetaData.getSchemas();
	}

	@Override
	public ResultSet getCatalogs() throws SQLException {
		return originalMetaData.getCatalogs();
	}

	@Override
	public ResultSet getTableTypes() throws SQLException {
		return originalMetaData.getTableTypes();
	}

	@Override
	public ResultSet getColumns(final String catalog, final String schemaPattern, final String tableNamePattern,
			final String columnNamePattern) throws SQLException {
		return originalMetaData.getColumns(catalog, schemaPattern, tableNamePattern, columnNamePattern);
	}

	@Override
	public ResultSet getColumnPrivileges(final String catalog, final String schema, final String table,
			final String columnNamePattern) throws SQLException {
		return originalMetaData.getColumnPrivileges(catalog, schema, table, columnNamePattern);
	}

	@Override
	public ResultSet getTablePrivileges(final String catalog, final String schemaPattern, final String tableNamePattern)
			throws SQLException {
		return originalMetaData.getTablePrivileges(catalog, schemaPattern, tableNamePattern);
	}

	@Override
	public ResultSet getBestRowIdentifier(final String catalog, final String schema, final String table,
			final int scope, final boolean nullable) throws SQLException {
		return originalMetaData.getBestRowIdentifier(catalog, schema, table, scope, nullable);
	}

	@Override
	public ResultSet getVersionColumns(final String catalog, final String schema, final String table)
			throws SQLException {
		return originalMetaData.getVersionColumns(catalog, schema, table);
	}

	@Override
	public ResultSet getPrimaryKeys(final String catalog, final String schema, final String table) throws SQLException {
		return originalMetaData.getPrimaryKeys(catalog, schema, table);
	}

	@Override
	public ResultSet getImportedKeys(final String catalog, final String schema, final String table)
			throws SQLException {
		return originalMetaData.getImportedKeys(catalog, schema, table);
	}

	@Override
	public ResultSet getExportedKeys(final String catalog, final String schema, final String table)
			throws SQLException {
		return originalMetaData.getExportedKeys(catalog, schema, table);
	}

	@Override
	public ResultSet getCrossReference(final String parentCatalog, final String parentSchema, final String parentTable,
			final String foreignCatalog, final String foreignSchema, final String foreignTable) throws SQLException {
		return originalMetaData.getCrossReference(parentCatalog, parentSchema, parentTable, foreignCatalog,
				foreignSchema, foreignTable);
	}

	@Override
	public ResultSet getTypeInfo() throws SQLException {
		return originalMetaData.getTypeInfo();
	}

	@Override
	public ResultSet getIndexInfo(final String catalog, final String schema, final String table, final boolean unique,
			final boolean approximate) throws SQLException {
		return originalMetaData.getIndexInfo(catalog, schema, table, unique, approximate);
	}

	@Override
	public boolean supportsResultSetType(final int type) throws SQLException {
		return originalMetaData.supportsResultSetType(type);
	}

	@Override
	public boolean supportsResultSetConcurrency(final int type, final int concurrency) throws SQLException {
		return originalMetaData.supportsResultSetConcurrency(type, concurrency);
	}

	@Override
	public boolean ownUpdatesAreVisible(final int type) throws SQLException {
		return originalMetaData.ownUpdatesAreVisible(type);
	}

	@Override
	public boolean ownDeletesAreVisible(final int type) throws SQLException {
		return originalMetaData.ownDeletesAreVisible(type);
	}

	@Override
	public boolean ownInsertsAreVisible(final int type) throws SQLException {
		return originalMetaData.ownInsertsAreVisible(type);
	}

	@Override
	public boolean othersUpdatesAreVisible(final int type) throws SQLException {
		return originalMetaData.othersUpdatesAreVisible(type);
	}

	@Override
	public boolean othersDeletesAreVisible(final int type) throws SQLException {
		return originalMetaData.othersDeletesAreVisible(type);
	}

	@Override
	public boolean othersInsertsAreVisible(final int type) throws SQLException {
		return originalMetaData.othersInsertsAreVisible(type);
	}

	@Override
	public boolean updatesAreDetected(final int type) throws SQLException {
		return originalMetaData.updatesAreDetected(type);
	}

	@Override
	public boolean deletesAreDetected(final int type) throws SQLException {
		return originalMetaData.deletesAreDetected(type);
	}

	@Override
	public boolean insertsAreDetected(final int type) throws SQLException {
		return originalMetaData.insertsAreDetected(type);
	}

	@Override
	public boolean supportsBatchUpdates() throws SQLException {
		return originalMetaData.supportsBatchUpdates();
	}

	@Override
	public ResultSet getUDTs(final String catalog, final String schemaPattern, final String typeNamePattern,
			final int[] types) throws SQLException {
		return originalMetaData.getUDTs(catalog, schemaPattern, typeNamePattern, types);
	}

	@Override
	public Connection getConnection() throws SQLException {
		return originalConn;
	}

	@Override
	public boolean supportsSavepoints() throws SQLException {
		return originalMetaData.supportsSavepoints();
	}

	@Override
	public boolean supportsNamedParameters() throws SQLException {
		return originalMetaData.supportsNamedParameters();
	}

	@Override
	public boolean supportsMultipleOpenResults() throws SQLException {
		return originalMetaData.supportsMultipleOpenResults();
	}

	@Override
	public boolean supportsGetGeneratedKeys() throws SQLException {
		return originalMetaData.supportsGetGeneratedKeys();
	}

	@Override
	public ResultSet getSuperTypes(final String catalog, final String schemaPattern, final String typeNamePattern)
			throws SQLException {
		return originalMetaData.getSuperTypes(catalog, schemaPattern, typeNamePattern);
	}

	@Override
	public ResultSet getSuperTables(final String catalog, final String schemaPattern, final String tableNamePattern)
			throws SQLException {
		return originalMetaData.getSuperTables(catalog, schemaPattern, tableNamePattern);
	}

	@Override
	public ResultSet getAttributes(final String catalog, final String schemaPattern, final String typeNamePattern,
			final String attributeNamePattern) throws SQLException {
		return originalMetaData.getAttributes(catalog, schemaPattern, typeNamePattern, attributeNamePattern);
	}

	@Override
	public boolean supportsResultSetHoldability(final int holdability) throws SQLException {
		return originalMetaData.supportsResultSetHoldability(holdability);
	}

	@Override
	public int getResultSetHoldability() throws SQLException {
		return originalMetaData.getResultSetHoldability();
	}

	@Override
	public int getDatabaseMajorVersion() throws SQLException {
		if (databaseMajorVersion == null) {
			databaseMajorVersion = originalMetaData.getDatabaseMajorVersion();
		}
		return databaseMajorVersion;
	}

	@Override
	public int getDatabaseMinorVersion() throws SQLException {
		if (databaseMinorVersion == null) {
			databaseMinorVersion = originalMetaData.getDatabaseMinorVersion();
		}
		return databaseMinorVersion;
	}

	@Override
	public int getJDBCMajorVersion() throws SQLException {
		return originalMetaData.getJDBCMajorVersion();
	}

	@Override
	public int getJDBCMinorVersion() throws SQLException {
		return originalMetaData.getJDBCMinorVersion();
	}

	@Override
	public int getSQLStateType() throws SQLException {
		return originalMetaData.getSQLStateType();
	}

	@Override
	public boolean locatorsUpdateCopy() throws SQLException {
		return originalMetaData.locatorsUpdateCopy();
	}

	@Override
	public boolean supportsStatementPooling() throws SQLException {
		return originalMetaData.supportsStatementPooling();
	}

	@Override
	public RowIdLifetime getRowIdLifetime() throws SQLException {
		return originalMetaData.getRowIdLifetime();
	}

	@Override
	public ResultSet getSchemas(final String catalog, final String schemaPattern) throws SQLException {
		return originalMetaData.getSchemas(catalog, schemaPattern);
	}

	@Override
	public boolean supportsStoredFunctionsUsingCallSyntax() throws SQLException {
		return originalMetaData.supportsStoredFunctionsUsingCallSyntax();
	}

	@Override
	public boolean autoCommitFailureClosesAllResultSets() throws SQLException {
		return originalMetaData.autoCommitFailureClosesAllResultSets();
	}

	@Override
	public ResultSet getClientInfoProperties() throws SQLException {
		return originalMetaData.getClientInfoProperties();
	}

	@Override
	public ResultSet getFunctions(final String catalog, final String schemaPattern, final String functionNamePattern)
			throws SQLException {
		return originalMetaData.getFunctions(catalog, schemaPattern, functionNamePattern);
	}

	@Override
	public ResultSet getFunctionColumns(final String catalog, final String schemaPattern,
			final String functionNamePattern, final String columnNamePattern) throws SQLException {
		return originalMetaData.getFunctionColumns(catalog, schemaPattern, functionNamePattern, columnNamePattern);
	}

	@Override
	public ResultSet getPseudoColumns(final String catalog, final String schemaPattern, final String tableNamePattern,
			final String columnNamePattern) throws SQLException {
		return originalMetaData.getPseudoColumns(catalog, schemaPattern, tableNamePattern, columnNamePattern);
	}

	@Override
	public boolean generatedKeyAlwaysReturned() throws SQLException {
		return originalMetaData.generatedKeyAlwaysReturned();
	}

	@Override
	public long getMaxLogicalLobSize() throws SQLException {
		return originalMetaData.getMaxLogicalLobSize();
	}

	@Override
	public boolean supportsRefCursors() throws SQLException {
		return originalMetaData.supportsRefCursors();
	}

}
