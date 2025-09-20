/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.Connection;
import java.sql.SQLException;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;

/**
 * Test class for SchemaFixedConnectionWrapper to improve method coverage
 *
 * @author Generated Test
 */
public class SchemaFixedConnectionWrapperAdditionalTest extends AbstractDbTest {

	/**
	 * Test wrapper functionality
	 */
	@Test
	public void testWrapperMethods() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// Test unwrap method
		assertThat(wrapper.unwrap(Connection.class), is(notNullValue()));
		assertThat(wrapper.unwrap(SchemaFixedConnectionWrapper.class), is(wrapper));
		
		// Test isWrapperFor method
		assertThat(wrapper.isWrapperFor(Connection.class), is(true));
		assertThat(wrapper.isWrapperFor(SchemaFixedConnectionWrapper.class), is(true));
		assertThat(wrapper.isWrapperFor(String.class), is(false));
		
		// Test unwrap with unsupported interface
		assertThrows(SQLException.class, () -> {
			wrapper.unwrap(String.class);
		});
	}

	/**
	 * Test statement creation methods
	 */
	@Test
	public void testStatementCreation() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// Test createStatement
		var statement = wrapper.createStatement();
		assertThat(statement, is(notNullValue()));
		statement.close();
		
		// Test prepareStatement
		var preparedStatement = wrapper.prepareStatement("SELECT 1");
		assertThat(preparedStatement, is(notNullValue()));
		preparedStatement.close();
		
		// Test prepareCall
		var callableStatement = wrapper.prepareCall("SELECT 1");
		assertThat(callableStatement, is(notNullValue()));
		callableStatement.close();
	}

	/**
	 * Test connection properties and metadata
	 */
	@Test
	public void testConnectionProperties() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// Test nativeSQL
		var nativeSQL = wrapper.nativeSQL("SELECT * FROM test");
		assertThat(nativeSQL, is(notNullValue()));
		
		// Test auto commit
		wrapper.setAutoCommit(true);
		assertThat(wrapper.getAutoCommit(), is(true));
		
		// Test metadata (should return cached metadata with fixed schema)
		var metadata = wrapper.getMetaData();
		assertThat(metadata, is(notNullValue()));
		
		// Test getting metadata again (should use cached version)
		var metadata2 = wrapper.getMetaData();
		assertThat(metadata2, is(notNullValue()));
	}

	/**
	 * Test transaction methods
	 */
	@Test
	public void testTransactionMethods() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		wrapper.setAutoCommit(false);
		
		// Test commit
		wrapper.commit();
		
		// Test rollback
		wrapper.rollback();
		
		// Test savepoint operations
		var savepoint = wrapper.setSavepoint();
		assertThat(savepoint, is(notNullValue()));
		
		var namedSavepoint = wrapper.setSavepoint("test_savepoint");
		assertThat(namedSavepoint, is(notNullValue()));
		
		wrapper.rollback(savepoint);
		wrapper.releaseSavepoint(namedSavepoint);
	}

	/**
	 * Test client info and warnings
	 */
	@Test
	public void testClientInfoAndWarnings() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// Test client info (may not be supported by H2)
		try {
			wrapper.setClientInfo("ApplicationName", "TestApp");
			var clientInfo = wrapper.getClientInfo("ApplicationName");
			// Note: clientInfo might be null depending on database implementation
		} catch (java.sql.SQLClientInfoException e) {
			// H2 doesn't support all client info operations - this is expected
		}
		
		var allClientInfo = wrapper.getClientInfo();
		assertThat(allClientInfo, is(notNullValue()));
		
		// Test warnings
		wrapper.clearWarnings();
		var warnings = wrapper.getWarnings();
		// Note: warnings might be null if no warnings exist
	}

	/**
	 * Test additional connection properties
	 */
	@Test
	public void testAdditionalProperties() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// Test catalog operations
		var catalog = wrapper.getCatalog();
		wrapper.setCatalog(catalog);
		
		// Test schema operations (should be controlled by wrapper)
		var schema = wrapper.getSchema();
		// Note: schema might be null in some databases
		try {
			wrapper.setSchema("test_schema");
		} catch (UnsupportedOperationException e) {
			// Expected - schema is fixed in this wrapper
		}
		
		// Test transaction isolation
		var isolation = wrapper.getTransactionIsolation();
		wrapper.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
		
		// Test type map
		var typeMap = wrapper.getTypeMap();
		// Note: typeMap might be null in some databases
		
		// Test holdability
		var holdability = wrapper.getHoldability();
		wrapper.setHoldability(holdability);
		
		// Test read only
		wrapper.setReadOnly(false);
		assertThat(wrapper.isReadOnly(), is(false));
		
		// Test validity
		assertThat(wrapper.isValid(1), is(true));
		
		// Test closed status
		assertThat(wrapper.isClosed(), is(false));
	}

	/**
	 * Test advanced statement creation methods
	 */
	@Test
	public void testAdvancedStatementCreation() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// Test createStatement with parameters
		var statement1 = wrapper.createStatement(
			java.sql.ResultSet.TYPE_FORWARD_ONLY, 
			java.sql.ResultSet.CONCUR_READ_ONLY
		);
		assertThat(statement1, is(notNullValue()));
		statement1.close();
		
		var statement2 = wrapper.createStatement(
			java.sql.ResultSet.TYPE_FORWARD_ONLY, 
			java.sql.ResultSet.CONCUR_READ_ONLY,
			java.sql.ResultSet.HOLD_CURSORS_OVER_COMMIT
		);
		assertThat(statement2, is(notNullValue()));
		statement2.close();
		
		// Test prepareStatement with parameters
		var prepStatement1 = wrapper.prepareStatement(
			"SELECT 1",
			java.sql.ResultSet.TYPE_FORWARD_ONLY, 
			java.sql.ResultSet.CONCUR_READ_ONLY
		);
		assertThat(prepStatement1, is(notNullValue()));
		prepStatement1.close();
		
		// Test prepareCall with parameters
		var callStatement1 = wrapper.prepareCall(
			"SELECT 1",
			java.sql.ResultSet.TYPE_FORWARD_ONLY, 
			java.sql.ResultSet.CONCUR_READ_ONLY
		);
		assertThat(callStatement1, is(notNullValue()));
		callStatement1.close();
	}

	/**
	 * Test data type creation methods
	 */
	@Test
	public void testDataTypeCreation() throws SQLException {
		var connection = config.getConnectionSupplier().getConnection();
		var wrapper = new SchemaFixedConnectionWrapper(connection, "TEST_SCHEMA");
		
		// These methods may not be supported by all databases, so we test them conditionally
		try {
			var blob = wrapper.createBlob();
			if (blob != null) {
				blob.free();
			}
		} catch (SQLException e) {
			// Some databases don't support these operations
		}
		
		try {
			var clob = wrapper.createClob();
			if (clob != null) {
				clob.free();
			}
		} catch (SQLException e) {
			// Some databases don't support these operations
		}
	}
}