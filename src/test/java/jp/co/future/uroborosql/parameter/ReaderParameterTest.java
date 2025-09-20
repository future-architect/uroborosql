/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.Reader;
import java.io.StringReader;
import java.sql.JDBCType;

import org.junit.jupiter.api.Test;

/**
 * ReaderParameterのテストクラス
 */
public class ReaderParameterTest {

	@Test
	void testConstructorWithoutLength() {
		String parameterName = "testParam";
		Reader reader = new StringReader("test data");
		
		ReaderParameter parameter = new ReaderParameter(parameterName, reader);
		
		assertThat(parameter.getParameterName(), is(parameterName));
		assertThat(parameter.getSqlType(), is(JDBCType.CLOB));
		assertThat(parameter.getValue(), is("[CLOB]"));
	}

	@Test
	void testConstructorWithLength() {
		String parameterName = "testParam";
		Reader reader = new StringReader("test data");
		int length = 100;
		
		ReaderParameter parameter = new ReaderParameter(parameterName, reader, length);
		
		assertThat(parameter.getParameterName(), is(parameterName));
		assertThat(parameter.getSqlType(), is(JDBCType.CLOB));
		assertThat(parameter.getValue(), is("[CLOB]"));
	}

	@Test
	void testToString() {
		String parameterName = "testParam";
		Reader reader = new StringReader("test data");
		ReaderParameter parameter = new ReaderParameter(parameterName, reader);
		
		String result = parameter.toString();
		
		assertThat(result, notNullValue());
		assertThat(result, containsString("Parameter name[testParam]"));
		assertThat(result, containsString("Value["));
		assertThat(result, containsString("SQL type[CLOB]"));
	}

	@Test
	void testToStringWithLength() {
		String parameterName = "testParam";
		Reader reader = new StringReader("test data with length");
		int length = 200;
		ReaderParameter parameter = new ReaderParameter(parameterName, reader, length);
		
		String result = parameter.toString();
		
		assertThat(result, notNullValue());
		assertThat(result, containsString("Parameter name[testParam]"));
		assertThat(result, containsString("SQL type[CLOB]"));
	}

	@Test
	void testConstructorWithZeroLength() {
		String parameterName = "testParam";
		Reader reader = new StringReader("");
		int length = 0;
		ReaderParameter parameter = new ReaderParameter(parameterName, reader, length);
		
		assertThat(parameter.getParameterName(), is(parameterName));
		assertThat(parameter.getSqlType(), is(JDBCType.CLOB));
		assertThat(parameter.getValue(), is("[CLOB]"));
	}

	@Test
	void testConstructorWithNegativeLength() {
		String parameterName = "testParam";
		Reader reader = new StringReader("test data");
		int length = -5;
		ReaderParameter parameter = new ReaderParameter(parameterName, reader, length);
		
		assertThat(parameter.getParameterName(), is(parameterName));
		assertThat(parameter.getSqlType(), is(JDBCType.CLOB));
		assertThat(parameter.getValue(), is("[CLOB]"));
	}

	@Test
	void testConstructorWithDifferentParameterNames() {
		// Test with various parameter names
		ReaderParameter param1 = new ReaderParameter("param1", new StringReader("data1"));
		ReaderParameter param2 = new ReaderParameter("param_underscore", new StringReader("data2"));
		ReaderParameter param3 = new ReaderParameter("paramWithCamelCase", new StringReader("data3"));
		
		assertThat(param1.getParameterName(), is("param1"));
		assertThat(param2.getParameterName(), is("param_underscore"));
		assertThat(param3.getParameterName(), is("paramWithCamelCase"));
	}
}