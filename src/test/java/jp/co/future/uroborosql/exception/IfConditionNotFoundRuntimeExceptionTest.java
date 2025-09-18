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

import org.junit.jupiter.api.Test;

/**
 * IfConditionNotFoundRuntimeExceptionのテストクラス
 */
public class IfConditionNotFoundRuntimeExceptionTest {

	@Test
	void testDefaultConstructor() {
		IfConditionNotFoundRuntimeException exception = new IfConditionNotFoundRuntimeException();
		assertThat(exception, notNullValue());
		assertThat(exception.getMessage(), is("IF condition is not specified."));
	}

	@Test
	void testThrowException() {
		assertThrows(IfConditionNotFoundRuntimeException.class, () -> {
			throw new IfConditionNotFoundRuntimeException();
		});
	}
}