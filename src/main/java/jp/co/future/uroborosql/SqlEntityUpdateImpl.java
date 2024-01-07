/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.sql.SQLType;
import java.util.function.Function;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.enums.SqlKind;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlEntityUpdate;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityUpdateImpl<E> extends AbstractExtractionCondition<SqlEntityUpdate<E>>
		implements SqlEntityUpdate<E> {
	/** エンティティハンドラー */
	private final EntityHandler<?> entityHandler;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context ExecutionContext
	 */
	SqlEntityUpdateImpl(final SqlAgent agent, final EntityHandler<?> entityHandler,
			final TableMetadata tableMetadata, final ExecutionContext context) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#count()
	 */
	@Override
	public int count() {
		try {
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString());
			return this.entityHandler.doUpdate(agent(), context(), null);
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.UPDATE, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.lang.Object)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final V value) {
		param(CaseFormat.CAMEL_CASE.convert(col), value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.util.function.Function)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final Function<ExecutionContext, V> function) {
		param(CaseFormat.CAMEL_CASE.convert(col), function);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final V value, final int sqlType) {
		param(CaseFormat.CAMEL_CASE.convert(col), value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final V value, final SQLType sqlType) {
		param(CaseFormat.CAMEL_CASE.convert(col), value, sqlType);
		return this;
	}
}
