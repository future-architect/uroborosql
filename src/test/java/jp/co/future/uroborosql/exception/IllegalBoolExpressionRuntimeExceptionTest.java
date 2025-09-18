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
 * IllegalBoolExpressionRuntimeExceptionのテストクラス
 */
public class IllegalBoolExpressionRuntimeExceptionTest {

	@Test
	void testMessageConstructor() {
		String message = "Invalid boolean expression";
		IllegalBoolExpressionRuntimeException exception = new IllegalBoolExpressionRuntimeException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is("A boolean type value could not be obtained.[" + message + "]"));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testThrowException() {
		String message = "Invalid boolean expression";
		assertThrows(IllegalBoolExpressionRuntimeException.class, () -> {
			throw new IllegalBoolExpressionRuntimeException(message);
		});
	}
}