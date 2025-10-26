/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.log.support;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.slf4j.Logger;

/**
 * ReplLoggingSupportのテストクラス
 */
public class ReplLoggingSupportTest {

	/**
	 * テスト用のReplLoggingSupport実装
	 */
	private static class TestReplLoggingSupport implements ReplLoggingSupport {
		// Test implementation
	}

	@Test
	void testReplLoggerAvailable() {
		TestReplLoggingSupport support = new TestReplLoggingSupport();
		Logger logger = support.REPL_LOG;
		
		assertThat(logger, notNullValue());
		assertThat(logger.getName(), is("jp.co.future.uroborosql.repl"));
	}

	@Test
	void testStaticLoggerAccess() {
		Logger logger = ReplLoggingSupport.REPL_LOG;
		
		assertThat(logger, notNullValue());
		assertThat(logger.getName(), is("jp.co.future.uroborosql.repl"));
	}

	@Test
	void testLoggerIsEnabled() {
		Logger logger = ReplLoggingSupport.REPL_LOG;
		
		// Test that logger methods can be called without exception
		logger.isDebugEnabled();
		logger.isInfoEnabled();
		logger.isWarnEnabled();
		logger.isErrorEnabled();
	}

	@Test
	void testLoggerCanLog() {
		Logger logger = ReplLoggingSupport.REPL_LOG;
		
		// Test that logging methods can be called without exception
		logger.debug("Debug message for test");
		logger.info("Info message for test");
		logger.warn("Warn message for test");
		logger.error("Error message for test");
	}
}