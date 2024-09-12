/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.log.support;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public interface EventLoggingSupport extends LoggerBase {
	static Logger getEventLogger(final String eventName) {
		return LoggerFactory.getLogger("jp.co.future.uroborosql.event." + eventName);
	}
}
