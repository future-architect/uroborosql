/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

/**
 * SqlParserRuntimeExceptionのテストクラス
 */
public class SqlParserRuntimeExceptionTest {

	@Test
	void testDefaultConstructor() {
		SqlParserRuntimeException exception = new SqlParserRuntimeException();
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), nullValue());
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testMessageConstructor() {
		String message = "SQL parse error";
		SqlParserRuntimeException exception = new SqlParserRuntimeException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testCauseConstructor() {
		RuntimeException cause = new RuntimeException("Root cause");
		SqlParserRuntimeException exception = new SqlParserRuntimeException(cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testMessageAndCauseConstructor() {
		String message = "SQL parse error";
		RuntimeException cause = new RuntimeException("Root cause");
		SqlParserRuntimeException exception = new SqlParserRuntimeException(message, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testThrowException() {
		String message = "SQL parsing failed";
		assertThrows(SqlParserRuntimeException.class, () -> {
			throw new SqlParserRuntimeException(message);
		});
	}
}