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
 * PessimisticLockExceptionのテストクラス
 */
public class PessimisticLockExceptionTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpClass() {
		config = UroboroSQL.builder("jdbc:h2:mem:PessimisticLockExceptionTest", "sa", "").build();
	}

	@Test
	void testMessageConstructor() {
		String message = "Pessimistic lock error";
		PessimisticLockException exception = new PessimisticLockException(message);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testCauseConstructor() {
		RuntimeException cause = new RuntimeException("Root cause");
		PessimisticLockException exception = new PessimisticLockException(cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testMessageAndCauseConstructor() {
		String message = "Pessimistic lock error";
		RuntimeException cause = new RuntimeException("Root cause");
		PessimisticLockException exception = new PessimisticLockException(message, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is(message));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testContextConstructor() {
		ExecutionContext context = config.context()
				.setSql("SELECT * FROM test WHERE id = ?")
				.param("id", 1);
		context.addSqlPart("SELECT * FROM test WHERE id = ?");

		PessimisticLockException exception = new PessimisticLockException(context);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), containsString("An error occurred due to pessimistic locking"));
		assertThat(exception.getMessage(), containsString("SELECT * FROM test WHERE id = ?"));
		assertThat(exception.getCause(), nullValue());
	}

	@Test
	void testContextAndCauseConstructor() {
		ExecutionContext context = config.context()
				.setSql("UPDATE test SET value = ? WHERE id = ?")
				.param("value", "updated")
				.param("id", 1);
		context.addSqlPart("UPDATE test SET value = ? WHERE id = ?");

		RuntimeException cause = new RuntimeException("Lock timeout");
		PessimisticLockException exception = new PessimisticLockException(context, cause);
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), containsString("An error occurred due to pessimistic locking"));
		assertThat(exception.getMessage(), containsString("UPDATE test SET value = ? WHERE id = ?"));
		assertThat(exception.getCause(), is(cause));
	}

	@Test
	void testThrowException() {
		String message = "Pessimistic lock failed";
		assertThrows(PessimisticLockException.class, () -> {
			throw new PessimisticLockException(message);
		});
	}
}