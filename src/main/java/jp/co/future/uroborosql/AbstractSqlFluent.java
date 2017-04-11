package jp.co.future.uroborosql;

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLType;
import java.util.Map;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.parameter.Parameter;

abstract class AbstractSqlFluent<T extends SqlFluent<T>> implements SqlFluent<T> {
	protected final SqlContext context;

	protected AbstractSqlFluent(final SqlContext context) {
		this.context = context;
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
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T param(final Parameter param) {
		context().param(param);
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
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T characterStreamParam(final String paramName, final Reader value, final int len) {
		context().characterStreamParam(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T characterStreamParam(final String paramName, final Reader value) {
		context().characterStreamParam(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T binaryStreamParam(final String paramName, final InputStream value, final int len) {
		context().binaryStreamParam(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T binaryStreamParam(final String paramName, final InputStream value) {
		context().binaryStreamParam(paramName, value);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T asciiStreamParam(final String paramName, final InputStream value, final int len) {
		context().asciiStreamParam(paramName, value, len);
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T asciiStreamParam(final String paramName, final InputStream value) {
		context().asciiStreamParam(paramName, value);
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