/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlEntityDelete;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.TableMetadata;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityDeleteImpl<E> extends AbstractExtractionCondition<SqlEntityDelete<E>>
		implements SqlEntityDelete<E> {
	private final EntityHandler<?> entityHandler;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context ExecutionContext
	 */
	SqlEntityDeleteImpl(final SqlAgent agent, final EntityHandler<?> entityHandler, final TableMetadata tableMetadata,
			final ExecutionContext context) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
	}

	@Override
	public int count() {
		try {
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString());
			return this.entityHandler.doDelete(agent(), context(), null);
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

}
