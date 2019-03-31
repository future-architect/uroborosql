/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * Oracle10（以前のバージョンも含む）用のDialect
 *
 * @author H.Sugimoto
 */
public class Oracle10Dialect extends OracleDialect {
	/**
	 * コンストラクタ
	 */
	public Oracle10Dialect() {
		super('\\', new char[] { '%', '_' });
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.OracleDialect#isTargetVersion(int)
	 */
	@Override
	protected boolean isTargetVersion(final int majorVersion) {
		return majorVersion < 11;
	}

}
