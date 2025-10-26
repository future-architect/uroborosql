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
 * UroborosqlTransactionExceptionのテストクラス
 */
public class UroborosqlTransactionExceptionTest {

	@Test
	void testDefaultConstructor() {
		UroborosqlTransactionException exception = new UroborosqlTransactionException();
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), nullValue());
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testMessageConstructor() {
		String message = "Transaction error";
		UroborosqlTransactionException exception = new UroborosqlTransactionException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testCauseConstructor() {
		RuntimeException cause = new RuntimeException("Root cause");
		UroborosqlTransactionException exception = new UroborosqlTransactionException(cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testMessageAndCauseConstructor() {
		String message = "Transaction error";
		RuntimeException cause = new RuntimeException("Root cause");
		UroborosqlTransactionException exception = new UroborosqlTransactionException(message, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testThrowException() {
		String message = "Transaction failed to commit";
		assertThrows(UroborosqlTransactionException.class, () -> {
			throw new UroborosqlTransactionException(message);
		});
	}
}