/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;

/**
 * OptimisticLockExceptionのテストクラス
 */
public class OptimisticLockExceptionTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:OptimisticLockExceptionTest", "sa", "").build();
	}

	@Test
	void testMessageConstructor() {
		String message = "Optimistic lock error";
		OptimisticLockException exception = new OptimisticLockException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testCauseConstructor() {
		RuntimeException cause = new RuntimeException("Root cause");
		OptimisticLockException exception = new OptimisticLockException(cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testMessageAndCauseConstructor() {
		String message = "Optimistic lock error";
		RuntimeException cause = new RuntimeException("Root cause");
		OptimisticLockException exception = new OptimisticLockException(message, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testContextConstructor() {
		ExecutionContext context = config.context()
				.setSql("UPDATE test SET value = ? WHERE id = ? AND version = ?")
				.param("value", "updated")
				.param("id", 1)
				.param("version", 1);
		context.addSqlPart("UPDATE test SET value = ? WHERE id = ? AND version = ?");

		OptimisticLockException exception = new OptimisticLockException(context);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), containsString("An error occurred due to optimistic locking"));
		assertThat(exception.getMessage(), containsString("UPDATE test SET value = ? WHERE id = ? AND version = ?"));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testThrowException() {
		String message = "Optimistic lock failed";
		assertThrows(OptimisticLockException.class, () -> {
			throw new OptimisticLockException(message);
		});
	}
}