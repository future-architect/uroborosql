/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * MariaDB(ver5）用のDialect
 *
 * @author H.Sugimoto
 */
public class MariaDb5Dialect extends MariaDbDialect {
	/**
	 * コンストラクタ
	 */
	public MariaDb5Dialect() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.MariaDbDialect#isTargetVersion(int)
	 */
	@Override
	protected boolean isTargetVersion(final int majorVersion) {
		return majorVersion == 5;
	}

}
