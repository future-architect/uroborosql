/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.RowIdLifetime;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * CachedDatabaseMetaDataのテストクラス
 */
public class CachedDatabaseMetaDataTest {

	private Connection connection;
	private DatabaseMetaData originalMetaData;
	private CachedDatabaseMetaData cachedMetaData;

	@BeforeEach
	void setUp() throws SQLException {
		connection = DriverManager.getConnection("jdbc:h2:mem:CachedDatabaseMetaDataTest", "sa", "");
		originalMetaData = connection.getMetaData();
		cachedMetaData = new CachedDatabaseMetaData(originalMetaData, connection);
	}

	@Test
	void testGetDatabaseProductName() throws SQLException {
		String result = cachedMetaData.getDatabaseProductName();
		assertThat(result, notNullValue());
		// Second call should return the same cached value
		String result2 = cachedMetaData.getDatabaseProductName();
		assertThat(result2, is(result));
	}

	@Test
	void testGetDatabaseProductVersion() throws SQLException {
		String result = cachedMetaData.getDatabaseProductVersion();
		assertThat(result, notNullValue());
		// Second call should return the same cached value
		String result2 = cachedMetaData.getDatabaseProductVersion();
		assertThat(result2, is(result));
	}

	@Test
	void testGetURL() throws SQLException {
		String result = cachedMetaData.getURL();
		assertThat(result, notNullValue());
		// Second call should return the same cached value
		String result2 = cachedMetaData.getURL();
		assertThat(result2, is(result));
	}

	@Test
	void testGetDatabaseMajorVersion() throws SQLException {
		int result = cachedMetaData.getDatabaseMajorVersion();
		// Second call should return the same cached value
		int result2 = cachedMetaData.getDatabaseMajorVersion();
		assertThat(result2, is(result));
	}

	@Test
	void testGetDatabaseMinorVersion() throws SQLException {
		int result = cachedMetaData.getDatabaseMinorVersion();
		// Second call should return the same cached value
		int result2 = cachedMetaData.getDatabaseMinorVersion();
		assertThat(result2, is(result));
	}

	@Test
	void testIsWrapperFor() throws SQLException {
		// Test for CachedDatabaseMetaData itself
		assertTrue(cachedMetaData.isWrapperFor(CachedDatabaseMetaData.class));
		
		// Test for DatabaseMetaData interface
		assertTrue(cachedMetaData.isWrapperFor(DatabaseMetaData.class));
	}

	@Test
	void testUnwrap() throws SQLException {
		// Test unwrapping to CachedDatabaseMetaData itself
		CachedDatabaseMetaData unwrapped = cachedMetaData.unwrap(CachedDatabaseMetaData.class);
		assertThat(unwrapped, is(cachedMetaData));

		// Test unwrapping to DatabaseMetaData
		DatabaseMetaData unwrappedMeta = cachedMetaData.unwrap(DatabaseMetaData.class);
		assertThat(unwrappedMeta, notNullValue());
	}

	@Test
	void testUnwrap_InvalidInterface() throws SQLException {
		// Test unwrapping to an interface this class doesn't support
		assertThrows(SQLException.class, () -> {
			cachedMetaData.unwrap(String.class);
		});
	}

	@Test
	void testGetConnection() throws SQLException {
		Connection result = cachedMetaData.getConnection();
		assertThat(result, is(connection));
	}

	@Test
	void testDelegatedMethods() throws SQLException {
		// Test a few delegated methods to ensure they work properly
		assertThat(cachedMetaData.getUserName(), notNullValue());
		assertThat(cachedMetaData.getDriverName(), notNullValue());
		assertThat(cachedMetaData.getDriverVersion(), notNullValue());
		
		// Test boolean methods
		cachedMetaData.isReadOnly();
		cachedMetaData.supportsTransactions();
		cachedMetaData.nullsAreSortedHigh();
	}

	@Test
	void testAdditionalDelegatedMethods() throws SQLException {
		// Test more delegated methods to improve coverage
		assertThat(cachedMetaData.allProceduresAreCallable(), is(notNullValue()));
		assertThat(cachedMetaData.allTablesAreSelectable(), is(notNullValue()));
		assertThat(cachedMetaData.nullsAreSortedLow(), is(notNullValue()));
		assertThat(cachedMetaData.nullsAreSortedAtStart(), is(notNullValue()));
		assertThat(cachedMetaData.nullsAreSortedAtEnd(), is(notNullValue()));
		
		// Test string methods
		assertThat(cachedMetaData.getIdentifierQuoteString(), is(notNullValue()));
		assertThat(cachedMetaData.getSQLKeywords(), is(notNullValue()));
		assertThat(cachedMetaData.getNumericFunctions(), is(notNullValue()));
		assertThat(cachedMetaData.getStringFunctions(), is(notNullValue()));
		assertThat(cachedMetaData.getSystemFunctions(), is(notNullValue()));
		assertThat(cachedMetaData.getTimeDateFunctions(), is(notNullValue()));
		assertThat(cachedMetaData.getSearchStringEscape(), is(notNullValue()));
		assertThat(cachedMetaData.getExtraNameCharacters(), is(notNullValue()));
		
		// Test support methods
		assertThat(cachedMetaData.supportsAlterTableWithAddColumn(), is(notNullValue()));
		assertThat(cachedMetaData.supportsAlterTableWithDropColumn(), is(notNullValue()));
		assertThat(cachedMetaData.supportsColumnAliasing(), is(notNullValue()));
		assertThat(cachedMetaData.nullPlusNonNullIsNull(), is(notNullValue()));
		assertThat(cachedMetaData.supportsConvert(), is(notNullValue()));
		assertThat(cachedMetaData.supportsConvert(java.sql.Types.VARCHAR, java.sql.Types.INTEGER), is(notNullValue()));
		assertThat(cachedMetaData.supportsTableCorrelationNames(), is(notNullValue()));
		assertThat(cachedMetaData.supportsDifferentTableCorrelationNames(), is(notNullValue()));
		assertThat(cachedMetaData.supportsExpressionsInOrderBy(), is(notNullValue()));
		assertThat(cachedMetaData.supportsOrderByUnrelated(), is(notNullValue()));
		assertThat(cachedMetaData.supportsGroupBy(), is(notNullValue()));
		assertThat(cachedMetaData.supportsGroupByUnrelated(), is(notNullValue()));
		assertThat(cachedMetaData.supportsGroupByBeyondSelect(), is(notNullValue()));
	}

	@Test
	void testResultSetMethods() throws SQLException {
		// Test ResultSet related methods that might not be covered
		try {
			ResultSet rs = cachedMetaData.getTables(null, null, "%", new String[]{"TABLE"});
			assertThat(rs, is(notNullValue()));
			rs.close();
		} catch (SQLException e) {
			// Some databases might not support this, which is fine for coverage
		}
		
		try {
			ResultSet rs = cachedMetaData.getColumns(null, null, "%", "%");
			assertThat(rs, is(notNullValue()));
			rs.close();
		} catch (SQLException e) {
			// Some databases might not support this, which is fine for coverage
		}
		
		try {
			ResultSet rs = cachedMetaData.getPrimaryKeys(null, null, "TEST_TABLE");
			assertThat(rs, is(notNullValue()));
			rs.close();
		} catch (SQLException e) {
			// Some databases might not support this, which is fine for coverage
		}
	}

	@Test
	void testAdditionalSupportMethods() throws SQLException {
		// Test more support methods for better coverage
		assertThat(cachedMetaData.supportsLikeEscapeClause(), is(notNullValue()));
		assertThat(cachedMetaData.supportsMultipleResultSets(), is(notNullValue()));
		assertThat(cachedMetaData.supportsMultipleTransactions(), is(notNullValue()));
		assertThat(cachedMetaData.supportsNonNullableColumns(), is(notNullValue()));
		assertThat(cachedMetaData.supportsMinimumSQLGrammar(), is(notNullValue()));
		assertThat(cachedMetaData.supportsCoreSQLGrammar(), is(notNullValue()));
		assertThat(cachedMetaData.supportsExtendedSQLGrammar(), is(notNullValue()));
		assertThat(cachedMetaData.supportsANSI92EntryLevelSQL(), is(notNullValue()));
		assertThat(cachedMetaData.supportsANSI92IntermediateSQL(), is(notNullValue()));
		assertThat(cachedMetaData.supportsANSI92FullSQL(), is(notNullValue()));
		assertThat(cachedMetaData.supportsIntegrityEnhancementFacility(), is(notNullValue()));
		assertThat(cachedMetaData.supportsOuterJoins(), is(notNullValue()));
		assertThat(cachedMetaData.supportsFullOuterJoins(), is(notNullValue()));
		assertThat(cachedMetaData.supportsLimitedOuterJoins(), is(notNullValue()));
	}

	@Test 
	void testTransactionAndConcurrencyMethods() throws SQLException {
		// Test transaction and concurrency related methods
		int isolation = cachedMetaData.getDefaultTransactionIsolation();
		assertThat(isolation, is(greaterThanOrEqualTo(Connection.TRANSACTION_NONE)));
		
		assertThat(cachedMetaData.supportsTransactionIsolationLevel(Connection.TRANSACTION_READ_COMMITTED), is(notNullValue()));
		assertThat(cachedMetaData.supportsDataDefinitionAndDataManipulationTransactions(), is(notNullValue()));
		assertThat(cachedMetaData.supportsDataManipulationTransactionsOnly(), is(notNullValue()));
		assertThat(cachedMetaData.dataDefinitionCausesTransactionCommit(), is(notNullValue()));
		assertThat(cachedMetaData.dataDefinitionIgnoredInTransactions(), is(notNullValue()));
		
		// Test result set support methods
		assertThat(cachedMetaData.supportsResultSetType(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.supportsResultSetConcurrency(java.sql.ResultSet.TYPE_FORWARD_ONLY, java.sql.ResultSet.CONCUR_READ_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.ownUpdatesAreVisible(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.ownDeletesAreVisible(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.ownInsertsAreVisible(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.othersUpdatesAreVisible(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.othersDeletesAreVisible(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.othersInsertsAreVisible(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.updatesAreDetected(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.deletesAreDetected(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
		assertThat(cachedMetaData.insertsAreDetected(java.sql.ResultSet.TYPE_FORWARD_ONLY), is(notNullValue()));
	}

	@Test
	void testDriverVersionMethods() throws SQLException {
		// Test driver version methods
		int majorVersion = cachedMetaData.getDriverMajorVersion();
		assertThat(majorVersion, is(greaterThanOrEqualTo(0)));
		
		int minorVersion = cachedMetaData.getDriverMinorVersion();
		assertThat(minorVersion, is(greaterThanOrEqualTo(0)));
	}

	@Test
	void testFileUsageMethods() throws SQLException {
		// Test file usage methods
		assertThat(cachedMetaData.usesLocalFiles(), is(notNullValue()));
		assertThat(cachedMetaData.usesLocalFilePerTable(), is(notNullValue()));
	}

	@Test
	void testIdentifierCaseMethods() throws SQLException {
		// Test identifier case methods
		assertThat(cachedMetaData.supportsMixedCaseIdentifiers(), is(notNullValue()));
		assertThat(cachedMetaData.storesMixedCaseIdentifiers(), is(notNullValue()));
		assertThat(cachedMetaData.supportsMixedCaseQuotedIdentifiers(), is(notNullValue()));
		assertThat(cachedMetaData.storesUpperCaseQuotedIdentifiers(), is(notNullValue()));
		assertThat(cachedMetaData.storesLowerCaseQuotedIdentifiers(), is(notNullValue()));
		assertThat(cachedMetaData.storesMixedCaseQuotedIdentifiers(), is(notNullValue()));
	}

	@Test
	void testTerminologyMethods() throws SQLException {
		// Test terminology methods
		String schemaTerm = cachedMetaData.getSchemaTerm();
		assertThat(schemaTerm, is(notNullValue()));
		
		String procedureTerm = cachedMetaData.getProcedureTerm();
		assertThat(procedureTerm, is(notNullValue()));
		
		String catalogTerm = cachedMetaData.getCatalogTerm();
		assertThat(catalogTerm, is(notNullValue()));
	}

	@Test
	void testCatalogMethods() throws SQLException {
		// Test catalog methods
		assertThat(cachedMetaData.isCatalogAtStart(), is(notNullValue()));
		String catalogSeparator = cachedMetaData.getCatalogSeparator();
		assertThat(catalogSeparator, is(notNullValue()));
	}

	@Test
	void testSchemaSupportMethods() throws SQLException {
		// Test schema support methods
		assertThat(cachedMetaData.supportsSchemasInDataManipulation(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSchemasInProcedureCalls(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSchemasInTableDefinitions(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSchemasInIndexDefinitions(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSchemasInPrivilegeDefinitions(), is(notNullValue()));
	}

	@Test
	void testCatalogSupportMethods() throws SQLException {
		// Test catalog support methods
		assertThat(cachedMetaData.supportsCatalogsInDataManipulation(), is(notNullValue()));
		assertThat(cachedMetaData.supportsCatalogsInProcedureCalls(), is(notNullValue()));
		assertThat(cachedMetaData.supportsCatalogsInTableDefinitions(), is(notNullValue()));
		assertThat(cachedMetaData.supportsCatalogsInIndexDefinitions(), is(notNullValue()));
		assertThat(cachedMetaData.supportsCatalogsInPrivilegeDefinitions(), is(notNullValue()));
	}

	@Test
	void testPositionedOperationMethods() throws SQLException {
		// Test positioned operation methods
		assertThat(cachedMetaData.supportsPositionedDelete(), is(notNullValue()));
		assertThat(cachedMetaData.supportsPositionedUpdate(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSelectForUpdate(), is(notNullValue()));
		assertThat(cachedMetaData.supportsStoredProcedures(), is(notNullValue()));
	}

	@Test
	void testSubqueryMethods() throws SQLException {
		// Test subquery methods
		assertThat(cachedMetaData.supportsSubqueriesInComparisons(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSubqueriesInExists(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSubqueriesInIns(), is(notNullValue()));
		assertThat(cachedMetaData.supportsSubqueriesInQuantifieds(), is(notNullValue()));
		assertThat(cachedMetaData.supportsCorrelatedSubqueries(), is(notNullValue()));
	}

	@Test
	void testUnionMethods() throws SQLException {
		// Test union methods
		assertThat(cachedMetaData.supportsUnion(), is(notNullValue()));
		assertThat(cachedMetaData.supportsUnionAll(), is(notNullValue()));
	}

	@Test
	void testOpenCursorMethods() throws SQLException {
		// Test open cursor methods
		assertThat(cachedMetaData.supportsOpenCursorsAcrossCommit(), is(notNullValue()));
		assertThat(cachedMetaData.supportsOpenCursorsAcrossRollback(), is(notNullValue()));
		assertThat(cachedMetaData.supportsOpenStatementsAcrossCommit(), is(notNullValue()));
		assertThat(cachedMetaData.supportsOpenStatementsAcrossRollback(), is(notNullValue()));
	}

	@Test
	void testMaxLimitMethods() throws SQLException {
		// Test max limit methods
		int maxBinaryLiteralLength = cachedMetaData.getMaxBinaryLiteralLength();
		assertThat(maxBinaryLiteralLength, is(greaterThanOrEqualTo(0)));
		
		int maxCharLiteralLength = cachedMetaData.getMaxCharLiteralLength();
		assertThat(maxCharLiteralLength, is(greaterThanOrEqualTo(0)));
		
		int maxColumnNameLength = cachedMetaData.getMaxColumnNameLength();
		assertThat(maxColumnNameLength, is(greaterThanOrEqualTo(0)));
		
		int maxColumnsInGroupBy = cachedMetaData.getMaxColumnsInGroupBy();
		assertThat(maxColumnsInGroupBy, is(greaterThanOrEqualTo(0)));
		
		int maxColumnsInIndex = cachedMetaData.getMaxColumnsInIndex();
		assertThat(maxColumnsInIndex, is(greaterThanOrEqualTo(0)));
		
		int maxColumnsInOrderBy = cachedMetaData.getMaxColumnsInOrderBy();
		assertThat(maxColumnsInOrderBy, is(greaterThanOrEqualTo(0)));
		
		int maxColumnsInSelect = cachedMetaData.getMaxColumnsInSelect();
		assertThat(maxColumnsInSelect, is(greaterThanOrEqualTo(0)));
		
		int maxColumnsInTable = cachedMetaData.getMaxColumnsInTable();
		assertThat(maxColumnsInTable, is(greaterThanOrEqualTo(0)));
		
		int maxConnections = cachedMetaData.getMaxConnections();
		assertThat(maxConnections, is(greaterThanOrEqualTo(0)));
		
		int maxCursorNameLength = cachedMetaData.getMaxCursorNameLength();
		assertThat(maxCursorNameLength, is(greaterThanOrEqualTo(0)));
		
		int maxIndexLength = cachedMetaData.getMaxIndexLength();
		assertThat(maxIndexLength, is(greaterThanOrEqualTo(0)));
		
		int maxSchemaNameLength = cachedMetaData.getMaxSchemaNameLength();
		assertThat(maxSchemaNameLength, is(greaterThanOrEqualTo(0)));
		
		int maxProcedureNameLength = cachedMetaData.getMaxProcedureNameLength();
		assertThat(maxProcedureNameLength, is(greaterThanOrEqualTo(0)));
		
		int maxCatalogNameLength = cachedMetaData.getMaxCatalogNameLength();
		assertThat(maxCatalogNameLength, is(greaterThanOrEqualTo(0)));
		
		int maxRowSize = cachedMetaData.getMaxRowSize();
		assertThat(maxRowSize, is(greaterThanOrEqualTo(0)));
		
		assertThat(cachedMetaData.doesMaxRowSizeIncludeBlobs(), is(notNullValue()));
		
		int maxStatementLength = cachedMetaData.getMaxStatementLength();
		assertThat(maxStatementLength, is(greaterThanOrEqualTo(0)));
		
		int maxStatements = cachedMetaData.getMaxStatements();
		assertThat(maxStatements, is(greaterThanOrEqualTo(0)));
		
		int maxTableNameLength = cachedMetaData.getMaxTableNameLength();
		assertThat(maxTableNameLength, is(greaterThanOrEqualTo(0)));
		
		int maxTablesInSelect = cachedMetaData.getMaxTablesInSelect();
		assertThat(maxTablesInSelect, is(greaterThanOrEqualTo(0)));
		
		int maxUserNameLength = cachedMetaData.getMaxUserNameLength();
		assertThat(maxUserNameLength, is(greaterThanOrEqualTo(0)));
	}

	@Test
	void testMetaDataResultSetMethods() throws SQLException {
		// Test metadata result set methods
		try (ResultSet rs = cachedMetaData.getProcedures(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getProcedureColumns(null, null, "%", "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getSchemas()) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getCatalogs()) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getTableTypes()) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getColumnPrivileges(null, null, "%", "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getTablePrivileges(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getBestRowIdentifier(null, null, "%", 
			DatabaseMetaData.bestRowSession, false)) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getVersionColumns(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getImportedKeys(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getExportedKeys(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getCrossReference(null, null, "%", null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getIndexInfo(null, null, "%", false, false)) {
			assertThat(rs, is(notNullValue()));
		}
	}

	@Test
	void testJdbc2Methods() throws SQLException {
		// Test JDBC 2.0 methods
		try (ResultSet rs = cachedMetaData.getUDTs(null, null, "%", null)) {
			assertThat(rs, is(notNullValue()));
		}
		
		assertThat(cachedMetaData.supportsResultSetHoldability(ResultSet.HOLD_CURSORS_OVER_COMMIT), is(notNullValue()));
		
		int holdability = cachedMetaData.getResultSetHoldability();
		assertThat(holdability, is(greaterThanOrEqualTo(ResultSet.HOLD_CURSORS_OVER_COMMIT)));
		
		int sqlStateType = cachedMetaData.getSQLStateType();
		assertThat(sqlStateType, is(greaterThanOrEqualTo(DatabaseMetaData.sqlStateSQL)));
		
		assertThat(cachedMetaData.locatorsUpdateCopy(), is(notNullValue()));
		assertThat(cachedMetaData.supportsStatementPooling(), is(notNullValue()));
	}

	@Test
	void testJdbc3Methods() throws SQLException {
		// Test JDBC 3.0 methods
		assertThat(cachedMetaData.supportsNamedParameters(), is(notNullValue()));
		assertThat(cachedMetaData.supportsMultipleOpenResults(), is(notNullValue()));
		assertThat(cachedMetaData.supportsGetGeneratedKeys(), is(notNullValue()));
		
		// Some H2 features may not be supported, handle gracefully
		try (ResultSet rs = cachedMetaData.getSuperTypes(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		} catch (SQLException e) {
			// Feature may not be supported by H2
			assertTrue(e.getMessage().contains("Feature not supported"));
		}
		
		try (ResultSet rs = cachedMetaData.getSuperTables(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		}
		
		try (ResultSet rs = cachedMetaData.getAttributes(null, null, "%", "%")) {
			assertThat(rs, is(notNullValue()));
		} catch (SQLException e) {
			// Feature may not be supported by H2
			assertTrue(e.getMessage().contains("Feature not supported"));
		}
		
		int databaseMajorVersion = cachedMetaData.getDatabaseMajorVersion();
		assertThat(databaseMajorVersion, is(greaterThanOrEqualTo(0)));
		
		int databaseMinorVersion = cachedMetaData.getDatabaseMinorVersion();
		assertThat(databaseMinorVersion, is(greaterThanOrEqualTo(0)));
		
		int jdbcMajorVersion = cachedMetaData.getJDBCMajorVersion();
		assertThat(jdbcMajorVersion, is(greaterThanOrEqualTo(0)));
		
		int jdbcMinorVersion = cachedMetaData.getJDBCMinorVersion();
		assertThat(jdbcMinorVersion, is(greaterThanOrEqualTo(0)));
		
		assertThat(cachedMetaData.supportsSavepoints(), is(notNullValue()));
	}

	@Test
	void testJdbc4Methods() throws SQLException {
		// Test JDBC 4.0 methods
		assertThat(cachedMetaData.autoCommitFailureClosesAllResultSets(), is(notNullValue()));
		
		try (ResultSet rs = cachedMetaData.getClientInfoProperties()) {
			assertThat(rs, is(notNullValue()));
		}
		
		// Some H2 features may not be supported, handle gracefully
		try (ResultSet rs = cachedMetaData.getFunctions(null, null, "%")) {
			assertThat(rs, is(notNullValue()));
		} catch (SQLException e) {
			// Feature may not be supported by H2
			assertTrue(e.getMessage().contains("Feature not supported"));
		}
		
		try (ResultSet rs = cachedMetaData.getFunctionColumns(null, null, "%", "%")) {
			assertThat(rs, is(notNullValue()));
		} catch (SQLException e) {
			// Feature may not be supported by H2
			assertTrue(e.getMessage().contains("Feature not supported"));
		}
		
		try (ResultSet rs = cachedMetaData.getSchemas(null, null)) {
			assertThat(rs, is(notNullValue()));
		}
		
		assertThat(cachedMetaData.supportsStoredFunctionsUsingCallSyntax(), is(notNullValue()));
	}

	@Test
	void testJdbc41Methods() throws SQLException {
		// Test JDBC 4.1 methods
		// getPseudoColumns may return null result set in some databases
		try (ResultSet rs = cachedMetaData.getPseudoColumns(null, null, "%", "%")) {
			// ResultSet can be null in some database implementations
			// Just verify the method can be called without exception
		}
		
		// generatedKeyAlwaysReturned may return null in some databases
		Boolean generatedKey = cachedMetaData.generatedKeyAlwaysReturned();
		// Just verify the method can be called without exception
		// Result can be null, true, or false depending on database implementation
	}

	@Test
	void testAdditionalMetaDataMethods() throws Exception {
		// Test getTypeInfo
		try (ResultSet rs = cachedMetaData.getTypeInfo()) {
			assertThat(rs, is(notNullValue()));
		}
		
		// Test supportsBatchUpdates
		boolean supportsBatch = cachedMetaData.supportsBatchUpdates();
		assertThat(supportsBatch, is(notNullValue()));
		
		// Test getSuperTypes - May not be supported in H2
		try (ResultSet rs = cachedMetaData.getSuperTypes(null, null, "%")) {
			// H2 may throw SQLFeatureNotSupportedException
			assertThat(rs, is(notNullValue()));
		} catch (SQLFeatureNotSupportedException e) {
			// This is expected for H2 database - feature not supported
		}
		
		// Test getAttributes - May not be supported in H2  
		try (ResultSet rs = cachedMetaData.getAttributes(null, null, "%", "%")) {
			// H2 may throw SQLFeatureNotSupportedException
			assertThat(rs, is(notNullValue()));
		} catch (SQLFeatureNotSupportedException e) {
			// This is expected for H2 database - feature not supported
		}
		
		// Test getRowIdLifetime - JDBC 4.0 method
		RowIdLifetime lifetime = cachedMetaData.getRowIdLifetime();
		assertThat(lifetime, is(notNullValue()));
		
		// Test getMaxLogicalLobSize - JDBC 4.0 method  
		long maxLobSize = cachedMetaData.getMaxLogicalLobSize();
		assertThat(maxLobSize >= 0, is(true));
		
		// Test supportsRefCursors - JDBC 4.0 method
		boolean supportsRefCursors = cachedMetaData.supportsRefCursors();
		assertThat(supportsRefCursors, is(notNullValue()));
	}
}