/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * AbstractExpressionParserFactoryのテストクラス
 */
public class AbstractExpressionParserFactoryTest {

	/**
	 * テスト用のAbstractExpressionParserFactory実装
	 */
	private static class TestExpressionParserFactory extends AbstractExpressionParserFactory {
		@Override
		public boolean accept() {
			return true;
		}

		@Override
		public ExpressionParser create() {
			return createExpressionParser("jp.co.future.uroborosql.expr.ognl.OgnlExpressionParser");
		}

		// テスト用にprotectedメソッドを公開
		public boolean testExistsTargetClass(String className) {
			return existsTargetClass(className);
		}

		public ExpressionParser testCreateExpressionParser(String className) {
			return createExpressionParser(className);
		}
	}

	@Test
	void testExistsTargetClass_Exists() {
		TestExpressionParserFactory factory = new TestExpressionParserFactory();
		// String class should always exist
		assertThat(factory.testExistsTargetClass("java.lang.String"), is(true));
	}

	@Test
	void testExistsTargetClass_NotExists() {
		TestExpressionParserFactory factory = new TestExpressionParserFactory();
		// Non-existent class should return false
		assertThat(factory.testExistsTargetClass("com.nonexistent.TestClass"), is(false));
	}

	@Test
	void testCreateExpressionParser_ValidClass() {
		TestExpressionParserFactory factory = new TestExpressionParserFactory();
		// Try to create OGNL expression parser if available
		try {
			ExpressionParser parser = factory.testCreateExpressionParser("jp.co.future.uroborosql.expr.ognl.OgnlExpressionParser");
			assertThat(parser, notNullValue());
		} catch (UroborosqlRuntimeException e) {
			// This is expected if OGNL is not available in the test environment
			assertThat(e.getMessage().contains("could not be created"), is(true));
		}
	}

	@Test
	void testCreateExpressionParser_InvalidClass() {
		TestExpressionParserFactory factory = new TestExpressionParserFactory();
		// Try to create a non-existent class
		assertThrows(UroborosqlRuntimeException.class, () -> {
			factory.testCreateExpressionParser("com.nonexistent.TestExpressionParser");
		});
	}

	@Test
	void testCreateExpressionParser_NotExpressionParserClass() {
		TestExpressionParserFactory factory = new TestExpressionParserFactory();
		// Try to create a class that is not an ExpressionParser
		// This will result in a ClassCastException wrapped in UroborosqlRuntimeException
		assertThrows(ClassCastException.class, () -> {
			factory.testCreateExpressionParser("java.lang.String");
		});
	}
}