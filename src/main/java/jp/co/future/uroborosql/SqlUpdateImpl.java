/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.SqlUpdate;

/**
 * SqlUpdate実装
 *
 * @author H.Sugimoto
 */
final class SqlUpdateImpl extends AbstractSqlFluent<SqlUpdate> implements SqlUpdate {
	/** バッチ処理を行うかどうか */
	private boolean batch = false;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 */
	SqlUpdateImpl(final SqlAgent agent, final ExecutionContext context) {
		super(agent, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#count()
	 */
	@Override
	public int count() {
		if (batch) {
			throw new IllegalStateException(
					"Since the parameter has already been set with addBatch() method, please call batch() method.");
		}
		try {
			return agent().update(context());
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		}
	}
}
