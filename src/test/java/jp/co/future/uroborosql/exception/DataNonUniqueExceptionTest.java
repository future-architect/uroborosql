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
 * DataNonUniqueExceptionのテストクラス
 */
public class DataNonUniqueExceptionTest {

	@Test
	void testDefaultConstructor() {
		DataNonUniqueException exception = new DataNonUniqueException();
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), nullValue());
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testMessageConstructor() {
		String message = "Data is not unique";
		DataNonUniqueException exception = new DataNonUniqueException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testCauseConstructor() {
		RuntimeException cause = new RuntimeException("Root cause");
		DataNonUniqueException exception = new DataNonUniqueException(cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testMessageAndCauseConstructor() {
		String message = "Data is not unique";
		RuntimeException cause = new RuntimeException("Root cause");
		DataNonUniqueException exception = new DataNonUniqueException(message, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testThrowException() {
		String message = "Multiple records found when expecting one";
		assertThrows(DataNonUniqueException.class, () -> {
			throw new DataNonUniqueException(message);
		});
	}
}