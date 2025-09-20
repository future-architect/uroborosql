/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.NClob;
import java.sql.SQLException;
import java.sql.SQLXML;
import java.sql.Struct;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * JdbcParameterFactoryのテストクラス
 */
public class JdbcParameterFactoryTest {

	private Connection connection;

	@BeforeEach
	void setUp() throws SQLException {
		connection = DriverManager.getConnection("jdbc:h2:mem:JdbcParameterFactoryTest", "sa", "");
	}

	@AfterEach
	void tearDown() throws SQLException {
		if (connection != null && !connection.isClosed()) {
			connection.close();
		}
	}

	@Test
	void testCreateArrayOf() throws SQLException {
		String[] elements = {"test1", "test2", "test3"};
		Array array = JdbcParameterFactory.createArrayOf(connection, "VARCHAR", elements);
		
		assertThat(array, notNullValue());
		Object[] resultArray = (Object[]) array.getArray();
		assertThat(resultArray.length, is(3));
		assertThat(resultArray[0], is("test1"));
		assertThat(resultArray[1], is("test2"));
		assertThat(resultArray[2], is("test3"));
	}

	@Test
	void testCreateBlob() throws SQLException {
		Blob blob = JdbcParameterFactory.createBlob(connection);
		
		assertThat(blob, notNullValue());
		assertThat(blob.length(), is(0L));
	}

	@Test
	void testCreateClob() throws SQLException {
		Clob clob = JdbcParameterFactory.createClob(connection);
		
		assertThat(clob, notNullValue());
		assertThat(clob.length(), is(0L));
	}

	@Test
	void testCreateNClob() throws SQLException {
		NClob nclob = JdbcParameterFactory.createNClob(connection);
		
		assertThat(nclob, notNullValue());
		assertThat(nclob.length(), is(0L));
	}

	@Test
	void testCreateSQLXML() throws SQLException {
		SQLXML sqlxml = JdbcParameterFactory.createSQLXML(connection);
		
		assertThat(sqlxml, notNullValue());
	}

	@Test
	void testCreateStruct() throws SQLException {
		// H2 doesn't support custom structs, but we can test that the method works
		// This will likely throw an exception from H2, which should be wrapped in UroborosqlRuntimeException
		Object[] attributes = {"test", 123};
		
		assertThrows(UroborosqlRuntimeException.class, () -> {
			JdbcParameterFactory.createStruct(connection, "TESTTYPE", attributes);
		});
	}

	@Test
	void testCreateArrayOfWithNullElements() throws SQLException {
		String[] elements = {"test1", null, "test3"};
		Array array = JdbcParameterFactory.createArrayOf(connection, "VARCHAR", elements);
		
		assertThat(array, notNullValue());
		Object[] resultArray = (Object[]) array.getArray();
		assertThat(resultArray.length, is(3));
		assertThat(resultArray[0], is("test1"));
		assertThat(resultArray[1], is((Object) null));
		assertThat(resultArray[2], is("test3"));
	}

	@Test
	void testCreateArrayOfWithEmptyArray() throws SQLException {
		String[] elements = {};
		Array array = JdbcParameterFactory.createArrayOf(connection, "VARCHAR", elements);
		
		assertThat(array, notNullValue());
		Object[] resultArray = (Object[]) array.getArray();
		assertThat(resultArray.length, is(0));
	}

	@Test
	void testCreateArrayOfWithIntegerElements() throws SQLException {
		Integer[] elements = {1, 2, 3, null, 5};
		Array array = JdbcParameterFactory.createArrayOf(connection, "INTEGER", elements);
		
		assertThat(array, notNullValue());
		Object[] resultArray = (Object[]) array.getArray();
		assertThat(resultArray.length, is(5));
		assertThat(resultArray[0], is(1));
		assertThat(resultArray[3], is((Object) null));
		assertThat(resultArray[4], is(5));
	}
}