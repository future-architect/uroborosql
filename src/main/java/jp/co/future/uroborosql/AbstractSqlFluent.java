/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLType;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Supplier;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.utils.BeanAccessor;

abstract class AbstractSqlFluent<T extends SqlFluent<T>> implements SqlFluent<T> {
	protected final SqlAgent agent;
	protected final SqlContext context;

	protected AbstractSqlFluent(final SqlAgent agent, final SqlContext context) {
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
	public SqlContext context() {
		return context;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramList(java.lang.String, java.lang.Object[])
	 */
	@SuppressWarnings("unchecked")
	@Override
	@Deprecated
	public <V> T paramList(final String paramName, final V... value) {
		context().param(paramName, Arrays.asList(value));
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramList(java.lang.String, java.util.function.Supplier)
	 */
	@SuppressWarnings("unchecked")
	@Override
	@Deprecated
	public <V> T paramList(final String paramName, final Supplier<Iterable<V>> supplier) {
		context().param(paramName, supplier);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramListIfAbsent(java.lang.String, java.lang.Object[])
	 */
	@SuppressWarnings("unchecked")
	@Override
	@Deprecated
	public <V> T paramListIfAbsent(final String paramName, final V... value) {
		context().paramIfAbsent(paramName, Arrays.asList(value));
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
		if (paramMap != null) {
			paramMap.forEach((k, v) -> param(k, v));
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
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#hasParam(java.lang.String)
	 */
	@Override
	public boolean hasParam(final String paramName) {
		return context().hasParam(paramName);
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
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(String, Supplier)
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
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParam(java.lang.String, java.io.Reader, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T clobParam(final String paramName, final Reader value, final int len) {
		context().clobParam(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParamIfAbsent(java.lang.String, java.io.Reader, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T clobParamIfAbsent(final String paramName, final Reader value, final int len) {
		context().clobParamIfAbsent(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParam(java.lang.String, java.io.Reader)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T clobParam(final String paramName, final Reader value) {
		context().clobParam(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#clobParamIfAbsent(java.lang.String, java.io.Reader)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T clobParamIfAbsent(final String paramName, final Reader value) {
		context().clobParamIfAbsent(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParam(java.lang.String, java.io.InputStream, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T blobParam(final String paramName, final InputStream value, final int len) {
		context().blobParam(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParamIfAbsent(java.lang.String, java.io.InputStream, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T blobParamIfAbsent(final String paramName, final InputStream value, final int len) {
		context().blobParamIfAbsent(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParam(java.lang.String, java.io.InputStream)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T blobParam(final String paramName, final InputStream value) {
		context().blobParam(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#blobParamIfAbsent(java.lang.String, java.io.InputStream)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T blobParamIfAbsent(final String paramName, final InputStream value) {
		context().blobParamIfAbsent(paramName, value);
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