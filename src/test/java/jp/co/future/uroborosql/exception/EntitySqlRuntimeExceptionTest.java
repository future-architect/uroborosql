/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.exception;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.SQLException;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.enums.SqlKind;

/**
 * EntitySqlRuntimeExceptionのテストクラス
 */
public class EntitySqlRuntimeExceptionTest {

	@Test
	void testConstructorWithProcKindAndCause() {
		SQLException cause = new SQLException("SQL error");
		SqlKind procKind = SqlKind.INSERT;
		
		EntitySqlRuntimeException exception = new EntitySqlRuntimeException(procKind, cause);
		
		assertThat(exception, notNullValue());
		assertThat(exception.getCause(), is(cause));
		assertThat(exception.getProcKind(), is(procKind));
		assertThat(exception.getMessage(), is(cause.toString()));
	}

	@Test
	void testGetProcKind() {
		SQLException cause = new SQLException("SQL error");
		SqlKind procKind = SqlKind.UPDATE;
		
		EntitySqlRuntimeException exception = new EntitySqlRuntimeException(procKind, cause);
		
		assertThat(exception.getProcKind(), is(procKind));
	}

	@Test
	void testWithDifferentSqlKinds() {
		SQLException cause = new SQLException("SQL error");
		
		// Test with SELECT
		EntitySqlRuntimeException selectException = new EntitySqlRuntimeException(SqlKind.SELECT, cause);
		assertThat(selectException.getProcKind(), is(SqlKind.SELECT));
		
		// Test with DELETE
		EntitySqlRuntimeException deleteException = new EntitySqlRuntimeException(SqlKind.DELETE, cause);
		assertThat(deleteException.getProcKind(), is(SqlKind.DELETE));
		
		// Test with MERGE
		EntitySqlRuntimeException mergeException = new EntitySqlRuntimeException(SqlKind.MERGE, cause);
		assertThat(mergeException.getProcKind(), is(SqlKind.MERGE));
	}

	@Test
	void testThrowException() {
		SQLException cause = new SQLException("Database connection failed");
		SqlKind procKind = SqlKind.INSERT;
		
		assertThrows(EntitySqlRuntimeException.class, () -> {
			throw new EntitySqlRuntimeException(procKind, cause);
		});
	}
}