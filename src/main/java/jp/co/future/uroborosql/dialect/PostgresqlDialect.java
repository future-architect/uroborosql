/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

/**
 * Postgresql用のDialect
 *
 * @author H.Sugimoto
 */
public class PostgresqlDialect extends AbstractDialect {
	/**
	 * コンストラクタ
	 */
	public PostgresqlDialect() {
		super();
	}

	@Override
	public boolean isRollbackToSavepointBeforeRetry() {
		return true;
	}

	@Override
	public String getDatabaseName() {
		return "PostgreSQL";
	}

}
