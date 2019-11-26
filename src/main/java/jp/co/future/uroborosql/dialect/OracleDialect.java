/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import jp.co.future.uroborosql.connection.ConnectionSupplier;

/**
 * Oracle共通の抽象Dialect
 *
 * @author H.Sugimoto
 */
public abstract class OracleDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 * @param escapeChar like検索時のエスケープ文字
	 * @param wildcards like検索時のワイルドカード文字配列
	 */
	protected OracleDialect(final char escapeChar, final char[] wildcards) {
		super(escapeChar, wildcards);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "Oracle";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsNullValuesOrdering()
	 */
	@Override
	public boolean supportsNullValuesOrdering() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsIdentity()
	 */
	@Override
	public boolean supportsIdentity() {
		return false;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getSequenceNextValSql(java.lang.String)
	 */
	@Override
	public String getSequenceNextValSql(final String sequenceName) {
		return sequenceName + ".nextval";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#accept(jp.co.future.uroborosql.connection.ConnectionSupplier)
	 */
	@Override
	public boolean accept(final ConnectionSupplier supplier) {
		if (supplier == null) {
			return false;
		}

		String[] parts = supplier.getDatabaseName().split("-", 2);
		String databaseName = parts[0];

		if (!databaseName.startsWith(getDatabaseName())) {
			return false;
		}

		String databaseVersion = parts[1];

		try {
			int majorVersion = Integer.parseInt(databaseVersion.substring(0, databaseVersion.indexOf(".")));
			return isTargetVersion(majorVersion);
		} catch (NumberFormatException e) {
			return false;
		}
	}

	/**
	 * 対象のOracleバージョンかどうかを判定する
	 *
	 * @param majorVersion コネクションから取得したメジャーバージョン
	 * @return 対象のバージョンの場合<code>true</code>
	 */
	protected abstract boolean isTargetVersion(int majorVersion);

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getModLiteral(java.lang.String, java.lang.String)
	 */
	@Override
	public String getModLiteral(final String dividend, final String divisor) {
		return "MOD(" + dividend + ", " + divisor + ")";
	}
}
