/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
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
}