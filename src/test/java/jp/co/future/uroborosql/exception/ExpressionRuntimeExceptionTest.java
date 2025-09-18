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
 * ExpressionRuntimeExceptionのテストクラス
 */
public class ExpressionRuntimeExceptionTest {

	@Test
	void testDefaultConstructor() {
		ExpressionRuntimeException exception = new ExpressionRuntimeException();
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), nullValue());
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testMessageConstructor() {
		String message = "Test expression error message";
		ExpressionRuntimeException exception = new ExpressionRuntimeException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testCauseConstructor() {
		RuntimeException cause = new RuntimeException("Root cause");
		ExpressionRuntimeException exception = new ExpressionRuntimeException(cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testMessageAndCauseConstructor() {
		String message = "Test expression error message";
		RuntimeException cause = new RuntimeException("Root cause");
		ExpressionRuntimeException exception = new ExpressionRuntimeException(message, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testThrowException() {
		String message = "Expression evaluation failed";
		assertThrows(ExpressionRuntimeException.class, () -> {
			throw new ExpressionRuntimeException(message);
		});
	}
}