/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * MySQL用のDialect
 *
 * @author H.Sugimoto
 */
public class MySqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public MySqlDialect() {
		super();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseName()
	 */
	@Override
	public String getDatabaseName() {
		return "MySQL";
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#supportsBulkInsert()
	 */
	@Override
	public boolean supportsBulkInsert() {
		return true;
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
}
