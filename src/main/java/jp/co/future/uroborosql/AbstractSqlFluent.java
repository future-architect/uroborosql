/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLType;
import java.util.Map;
import java.util.function.Supplier;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.utils.BeanAccessor;

abstract class AbstractSqlFluent<T extends SqlFluent<T>> implements SqlFluent<T> {
	/** SqlAgent. */
	private final SqlAgent agent;

	/** ExecutionContext. */
	private final ExecutionContext context;

	protected AbstractSqlFluent(final SqlAgent agent, final ExecutionContext context) {
		this.agent = agent;
		this.context = context;
	}

	/**
	 * SqlAgentの取得
	 *
	 * @return SqlAgent
	 */
	protected SqlAgent agent() {
		return agent;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#context()
	 */
	@Override
	public ExecutionContext context() {
		return context;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#hasParam(java.lang.String)
	 */
	@Override
	public boolean hasParam(final String paramName) {
		return context().hasParam(paramName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T param(final String paramName, final V value) {
		context().param(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.util.function.Supplier)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T param(final String paramName, final Supplier<V> supplier) {
		context().param(paramName, supplier);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T param(final String paramName, final V value, final SQLType sqlType) {
		context().param(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T param(final String paramName, final V value, final int sqlType) {
		context().param(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfNotEmpty(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramIfNotEmpty(final String paramName, final V value) {
		context().paramIfNotEmpty(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfNotEmpty(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramIfNotEmpty(final String paramName, final V value, final SQLType sqlType) {
		context().paramIfNotEmpty(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfNotEmpty(java.lang.String, java.lang.Object, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramIfNotEmpty(final String paramName, final V value, final int sqlType) {
		context().paramIfNotEmpty(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramIfAbsent(final String paramName, final V value) {
		context().paramIfAbsent(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramIfAbsent(final String paramName, final V value, final SQLType sqlType) {
		context().paramIfAbsent(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramIfAbsent(final String paramName, final V value, final int sqlType) {
		context().paramIfAbsent(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(java.util.Map)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T paramMap(final Map<String, Object> paramMap) {
		if (paramMap != null && !paramMap.isEmpty()) {
			paramMap.forEach(this::param);
		}
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramBean(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T paramBean(final V bean) {
		if (bean != null) {
			BeanAccessor.fields(bean.getClass()).stream()
					.forEach(f -> param(f.getName(), BeanAccessor.value(f, bean)));
		}
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int)
	 */
	@Override
	public T retry(final int count) {
		return retry(count, 0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T retry(final int count, final int waitTime) {
		context().setMaxRetryCount(count).setRetryWaitTime(waitTime);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#sqlId(String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T sqlId(final String sqlId) {
		context().setSqlId(sqlId);
		return (T) this;
	}
}