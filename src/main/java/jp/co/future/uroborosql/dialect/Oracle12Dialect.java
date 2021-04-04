/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * Oracle12（以降のバージョンも含む）用のDialect
 *
 * @author H.Sugimoto
 */
public class Oracle12Dialect extends OracleDialect {
	/**
	 * コンストラクタ
	 */
	public Oracle12Dialect() {
		super('\\', new char[] { '%', '_', '％', '＿' });
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsLimitClause()
	 */
	@Override
	public boolean supportsLimitClause() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.OracleDialect#supportsIdentity()
	 */
	@Override
	public boolean supportsIdentity() {
		return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.AbstractDialect#getLimitClause(long, long)
	 */
	@Override
	public String getLimitClause(final long limit, final long offset) {
		var builder = new StringBuilder();
		if (offset > 0) {
			builder.append("OFFSET ").append(offset).append(" ROWS");
			if (limit > 0) {
				builder.append(" ");
			}
		}
		if (limit > 0) {
			builder.append("FETCH FIRST ").append(limit).append(" ROWS ONLY");
		}
		if (builder.length() > 0) {
			builder.append(System.lineSeparator());
		}
		return builder.toString();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.OracleDialect#isTargetVersion(int)
	 */
	@Override
	protected boolean isTargetVersion(final int majorVersion) {
		return majorVersion >= 12;
	}
}
