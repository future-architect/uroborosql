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

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.event.AfterEntityUpdateEvent;
import jp.co.future.uroborosql.event.BeforeEntityUpdateEvent;
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
	/** エンティティタイプ */
	private final Class<? extends E> entityType;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context ExecutionContext
	 * @param entityType EntityType
	 */
	SqlEntityUpdateImpl(final SqlAgent agent, final EntityHandler<?> entityHandler,
			final TableMetadata tableMetadata, final ExecutionContext context, final Class<? extends E> entityType) {
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
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString());

			// EntityUpdate実行前イベント発行
			var eventListenerHolder = agent().getSqlConfig().getEventListenerHolder();
			if (eventListenerHolder.hasBeforeEntityUpdateListener()) {
				var eventObj = new BeforeEntityUpdateEvent(context(), null, this.entityType);
				for (var listener : eventListenerHolder.getBeforeEntityUpdateListeners()) {
					listener.accept(eventObj);
				}
			}

			var count = this.entityHandler.doUpdate(agent(), context(), null);

			// EntityUpdate実行後イベント発行
			if (eventListenerHolder.hasAfterEntityUpdateListener()) {
				var eventObj = new AfterEntityUpdateEvent(context(), null, entityType, count);
				for (var listener : eventListenerHolder.getAfterEntityUpdateListeners()) {
					listener.accept(eventObj);
				}
				count = eventObj.getCount();
			}
			return count;
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.lang.Object)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final V value) {
		context().param(CaseFormat.CAMEL_CASE.convert(col), value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.util.function.Supplier)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final Supplier<V> supplier) {
		context().param(CaseFormat.CAMEL_CASE.convert(col), supplier);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final V value, final int sqlType) {
		context().param(CaseFormat.CAMEL_CASE.convert(col), value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityUpdate#set(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public <V> SqlEntityUpdate<E> set(final String col, final V value, final SQLType sqlType) {
		context().param(CaseFormat.CAMEL_CASE.convert(col), value, sqlType);
		return this;
	}
}
