/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.expr;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.log.support.ParserLoggingSupport;

/**
 * 評価式パーサの共通親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractExpressionParser implements ExpressionParser, ParserLoggingSupport {

	/** SqlConfig */
	private SqlConfig sqlConfig;

	/**
	 * コンストラクタ
	 */
	public AbstractExpressionParser() {
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#setSqlConfig(jp.co.future.uroborosql.config.SqlConfig)
	 */
	@Override
	public void setSqlConfig(final SqlConfig sqlConfig) {
		this.sqlConfig = sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfigAware#getSqlConfig()
	 */
	@Override
	public SqlConfig getSqlConfig() {
		return sqlConfig;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.expr.ExpressionParser#initialize()
	 */
	@Override
	public void initialize() {
	}
}