/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * MariaDB(ver10とそれ以降）用のDialect
 *
 * @author H.Sugimoto
 */
public class MariaDb10Dialect extends MariaDbDialect {
	/**
	 * コンストラクタ
	 */
	public MariaDb10Dialect() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.MariaDbDialect#isTargetVersion(int)
	 */
	@Override
	protected boolean isTargetVersion(final int majorVersion) {
		return majorVersion >= 10;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.MariaDbDialect#supportsSequence()
	 */
	@Override
	public boolean supportsSequence() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getSequenceNextValSql(java.lang.String)
	 */
	@Override
	public String getSequenceNextValSql(final String sequenceName) {
		return "nextval(" + sequenceName + ")";
	}

}
