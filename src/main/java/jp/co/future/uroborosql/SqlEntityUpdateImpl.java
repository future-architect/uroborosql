/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.sql.SQLType;
import java.util.function.Supplier;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlEntityUpdate;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.TableMetadata;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityUpdateImpl<E> extends AbstractExtractionCondition<SqlEntityUpdate<E>>
		implements SqlEntityUpdate<E> {
	private final EntityHandler<?> entityHandler;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context SqlContext
	 */
	SqlEntityUpdateImpl(final SqlAgent agent, final EntityHandler<?> entityHandler,
			final TableMetadata tableMetadata, final SqlContext context) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
	}

	@Override
	public int count() {
		try {
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString());
			return this.entityHandler.doUpdate(agent(), context(), null);
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.UPDATE, e);
		}
	}

	@Override
	public <V> SqlEntityUpdate<E> set(final String paramName, final V value) {
		param(paramName, value);
		return this;
	}

	@Override
	public <V> SqlEntityUpdate<E> set(final String paramName, final Supplier<V> supplier) {
		param(paramName, supplier);
		return this;
	}

	@Override
	public <V> SqlEntityUpdate<E> set(final String paramName, final V value, final int sqlType) {
		param(paramName, value, sqlType);
		return this;
	}

	@Override
	public <V> SqlEntityUpdate<E> set(final String paramName, final V value, final SQLType sqlType) {
		param(paramName, value, sqlType);
		return this;
	}
}
