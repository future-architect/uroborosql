/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.enums.SqlKind;
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
	/** エンティティハンドラー */
	private final EntityHandler<?> entityHandler;
	/** エンティティタイプ */
	private final Class<?> entityType;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context SqlContext
	 * @param entityType エンティティタイプ
	 */
	SqlEntityDeleteImpl(final SqlAgent agent, final EntityHandler<?> entityHandler, final TableMetadata tableMetadata,
			final SqlContext context, final Class<?> entityType) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
		this.entityType = entityType;
	}

	@Override
	public int count() {
		try {
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString())
					.setSqlKind(SqlKind.ENTITY_DELETE);
			int count = this.entityHandler.doDelete(agent(), context(), null);
			agent().getQueryCache(this.entityType).ifPresent(cache -> cache.clear());
			return count;
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_DELETE, e);
		}
	}

}
