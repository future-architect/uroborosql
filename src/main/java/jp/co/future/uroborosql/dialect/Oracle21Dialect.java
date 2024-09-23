/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * Oracle21用のDialect
 *
 * @author H.Sugimoto
 */
public class Oracle21Dialect extends Oracle12Dialect {
	/**
	 * コンストラクタ
	 */
	public Oracle21Dialect() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.OracleDialect#isTargetVersion(int)
	 */
	@Override
	protected boolean isTargetVersion(final int majorVersion) {
		return majorVersion == 21;
	}
}
