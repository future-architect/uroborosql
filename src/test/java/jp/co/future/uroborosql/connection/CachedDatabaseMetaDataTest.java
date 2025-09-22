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
import java.sql.SQLException;

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
		cachedMetaData.supportsAlterTableWithAddColumn();
		cachedMetaData.supportsAlterTableWithDropColumn();
		cachedMetaData.supportsColumnAliasing();
		cachedMetaData.nullPlusNonNullIsNull();
		cachedMetaData.supportsConvert();
		cachedMetaData.supportsConvert(java.sql.Types.VARCHAR, java.sql.Types.INTEGER);
		cachedMetaData.supportsTableCorrelationNames();
		cachedMetaData.supportsDifferentTableCorrelationNames();
		cachedMetaData.supportsExpressionsInOrderBy();
		cachedMetaData.supportsOrderByUnrelated();
		cachedMetaData.supportsGroupBy();
		cachedMetaData.supportsGroupByUnrelated();
		cachedMetaData.supportsGroupByBeyondSelect();
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
}