/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.event.AfterEntityDeleteEvent;
import jp.co.future.uroborosql.event.BeforeEntityDeleteEvent;
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
	SqlEntityDeleteImpl(final SqlAgent agent, final EntityHandler<?> entityHandler, final TableMetadata tableMetadata,
			final ExecutionContext context, final Class<? extends E> entityType) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
		this.entityType = entityType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityDelete#sqlId(java.lang.String)
	 */
	@Override
	public SqlEntityDelete<E> sqlId(final String sqlId) {
		context().setSqlId(sqlId);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityDelete#retry(int)
	 */
	@Override
	public SqlEntityDelete<E> retry(final int count) {
		return retry(count, 0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityDelete#retry(int, int)
	 */
	@Override
	public SqlEntityDelete<E> retry(final int count, final int waitTime) {
		context().setMaxRetryCount(count).setRetryWaitTime(waitTime);
		return this;
	}

	@Override
	public int count() {
		try {
			context().setSql(new StringBuilder(context().getSql()).append(getWhereClause()).toString());

			// EntityDelete実行前イベント発行
			var eventListenerHolder = agent().getSqlConfig().getEventListenerHolder();
			if (eventListenerHolder.hasBeforeEntityDeleteListener()) {
				var eventObj = new BeforeEntityDeleteEvent(context(), null, this.entityType);
				for (var listener : eventListenerHolder.getBeforeEntityDeleteListeners()) {
					listener.accept(eventObj);
				}
			}

			var count = this.entityHandler.doDelete(agent(), context(), null);

			// EntityDelete実行後イベント発行
			if (eventListenerHolder.hasAfterEntityDeleteListener()) {
				var eventObj = new AfterEntityDeleteEvent(context(), null, this.entityType, count);
				for (var listener : eventListenerHolder.getAfterEntityDeleteListeners()) {
					listener.accept(eventObj);
				}
				count = eventObj.getCount();
			}
			return count;
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

}
