/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.log;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public interface SqlLogger extends LoggerBase {
	/** SQLロガー */
	Logger SQL_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.sql");
}
