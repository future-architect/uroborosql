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
import java.util.Map;

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
	public T paramList(final String paramName, final Object... value) {
		context().paramList(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramListIfAbsent(java.lang.String, java.lang.Object[])
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T paramListIfAbsent(final String paramName, final Object... value) {
		context().paramListIfAbsent(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(java.util.Map)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T paramMap(final Map<String, ?> paramMap) {
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
	public T paramBean(final Object bean) {
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
	public T param(final String paramName, final Object value, final int sqlType) {
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
	public T paramIfAbsent(final String paramName, final Object value, final int sqlType) {
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
	public T param(final String paramName, final Object value, final SQLType sqlType) {
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
	public T paramIfAbsent(final String paramName, final Object value, final SQLType sqlType) {
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
	public T param(final String paramName, final Object value) {
		context().param(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramIfAbsent(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T paramIfAbsent(final String paramName, final Object value) {
		context().paramIfAbsent(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T outParam(final String paramName, final int sqlType) {
		context().outParam(paramName, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, java.sql.SQLType)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T outParam(final String paramName, final SQLType sqlType) {
		context().outParam(paramName, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T inOutParam(final String paramName, final Object value, final int sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParamIfAbsent(java.lang.String, java.lang.Object, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T inOutParamIfAbsent(final String paramName, final Object value, final int sqlType) {
		context().inOutParamIfAbsent(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T inOutParam(final String paramName, final Object value, final SQLType sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParamIfAbsent(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T inOutParamIfAbsent(final String paramName, final Object value, final SQLType sqlType) {
		context().inOutParamIfAbsent(paramName, value, sqlType);
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
}