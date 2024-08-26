/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.log;

import org.slf4j.MDC;

public interface LoggerBase {
	/** すべてのログ出力を抑止するためのMDCキー */
	String SUPPRESS_LOG_OUTPUT = "SuppressLogOutput";

	/** パラメータログ出力を抑止するためのMDCキー */
	String SUPPRESS_PARAMETER_LOG_OUTPUT = "SuppressParameterLogOutput";

	default void suppressLogging() {
		MDC.put(SUPPRESS_LOG_OUTPUT, SUPPRESS_LOG_OUTPUT);
	}

	default boolean isSuppressLogging() {
		return MDC.get(SUPPRESS_LOG_OUTPUT) != null;
	}

	default void releaseLogging() {
		MDC.remove(SUPPRESS_LOG_OUTPUT);
	}

	default void suppressParameterLogging() {
		MDC.put(SUPPRESS_PARAMETER_LOG_OUTPUT, SUPPRESS_PARAMETER_LOG_OUTPUT);
	}

	default boolean isSuppressParameterLogging() {
		return MDC.get(SUPPRESS_PARAMETER_LOG_OUTPUT) != null;
	}

	default void releaseParameterLogging() {
		MDC.remove(SUPPRESS_PARAMETER_LOG_OUTPUT);
	}
}
