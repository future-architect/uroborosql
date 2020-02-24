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
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlEntityUpdate実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityUpdateImpl<E> extends AbstractExtractionCondition<SqlEntityUpdate<E>>
		implements SqlEntityUpdate<E> {
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
	SqlEntityUpdateImpl(final SqlAgent agent, final EntityHandler<?> entityHandler,
			final TableMetadata tableMetadata, final SqlContext context, final Class<?> entityType) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
		this.entityType = entityType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#count()
	 */
	@Override
	public int count() {
		try {
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString())
					.setSqlKind(SqlKind.ENTITY_UPDATE);
			int count = this.entityHandler.doUpdate(agent(), context(), null);
			agent().getQueryCache(this.entityType).ifPresent(cache -> cache.clear());
			return count;
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(SqlKind.ENTITY_UPDATE, e);
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
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.util.function.Supplier)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final Supplier<V> supplier) {
		param(CaseFormat.CAMEL_CASE.convert(col), supplier);
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
