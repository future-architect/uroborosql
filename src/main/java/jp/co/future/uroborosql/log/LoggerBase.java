/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.log;

import org.slf4j.Logger;
import org.slf4j.MDC;
import org.slf4j.spi.LoggingEventBuilder;
import org.slf4j.spi.NOPLoggingEventBuilder;

/**
 * ログ出力する際の共通親インタフェース
 */
public interface LoggerBase {
	/** すべてのログ出力を抑止するためのMDCキー */
	String SUPPRESS_LOG_OUTPUT = "UroboroSQL_SuppressLogOutput";

	/** パラメータログ出力を抑止するためのMDCキー */
	String SUPPRESS_PARAMETER_LOG_OUTPUT = "UroboroSQL_SuppressParameterLogOutput";

	/**
	 * Uroborosqlが出力するログを抑止する.
	 */
	default void suppressLogging() {
		MDC.put(SUPPRESS_LOG_OUTPUT, SUPPRESS_LOG_OUTPUT);
	}

	/**
	 * Uroborosqlが出力するログが抑止されているかを返す.
	 * @return ログが抑止されている場合<code>true</code>
	 */
	default boolean isSuppressLogging() {
		return MDC.get(SUPPRESS_LOG_OUTPUT) != null;
	}

	/**
	 * Uroborosqlが出力するログが抑止されている場合、抑止を終了する.
	 */
	default void releaseLogging() {
		MDC.remove(SUPPRESS_LOG_OUTPUT);
	}

	/**
	 * Uroborosqlが出力するパラメータログを抑止する.
	 */
	default void suppressParameterLogging() {
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, SUPPRESS_PARAMETER_LOG_OUTPUT);
	}

	/**
	 * Uroborosqlが出力するパラメータログが抑止されているかを返す.
	 * @return パラメータログが抑止されている場合<code>true</code>
	 */
	default boolean isSuppressParameterLogging() {
		return MDC.get(SUPPRESS_PARAMETER_LOG_OUTPUT) != null;
	}

	/**
	 * Uroborosqlが出力するパラメータログが抑止されている場合、抑止を終了する.
	 */
	default void releaseParameterLogging() {
		MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
	}

	default LoggingEventBuilder atError(final Logger logger) {
		if (!isSuppressLogging()) {
			return logger.atError();
		} else {
			return NOPLoggingEventBuilder.singleton();
		}
	}

	default LoggingEventBuilder atWarn(final Logger logger) {
		if (!isSuppressLogging()) {
			return logger.atWarn();
		} else {
			return NOPLoggingEventBuilder.singleton();
		}
	}

	default LoggingEventBuilder atInfo(final Logger logger) {
		if (!isSuppressLogging()) {
			return logger.atInfo();
		} else {
			return NOPLoggingEventBuilder.singleton();
		}
	}

	default LoggingEventBuilder atDebug(final Logger logger) {
		if (!isSuppressLogging()) {
			return logger.atDebug();
		} else {
			return NOPLoggingEventBuilder.singleton();
		}
	}

	default LoggingEventBuilder atTrace(final Logger logger) {
		if (!isSuppressLogging()) {
			return logger.atTrace();
		} else {
			return NOPLoggingEventBuilder.singleton();
		}
	}
}
