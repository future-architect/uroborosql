package jp.co.future.uroborosql;

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.Map;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * Procedure実装
 *
 * @author H.Sugimoto
 */
final class ProcedureImpl implements Procedure {
	private final SqlAgent agent;
	private final SqlContext context;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	ProcedureImpl(final SqlAgent agent, final SqlContext context) {
		this.agent = agent;
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
	@Override
	public Procedure paramList(final String paramName, final Object... value) {
		context().paramList(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(Map<String, Object>)
	 */
	@Override
	public Procedure paramMap(final Map<String, Object> paramMap) {
		if (paramMap != null) {
			paramMap.forEach((k, v) -> param(k, v));
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public Procedure param(final String paramName, final Object value, final int sqlType) {
		context().param(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public Procedure param(final String paramName, final Object value, final SQLType sqlType) {
		context().param(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object)
	 */
	@Override
	public Procedure param(final String paramName, final Object value) {
		context().param(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@Override
	public Procedure param(final Parameter param) {
		context().param(param);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, int)
	 */
	@Override
	public Procedure outParam(final String paramName, final int sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, java.sql.SQLType)
	 */
	@Override
	public Procedure outParam(final String paramName, final SQLType sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public Procedure inOutParam(final String paramName, final Object value, final int sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object,
	 *      java.sql.SQLType)
	 */
	@Override
	public Procedure inOutParam(final String paramName, final Object value, final SQLType sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader, int)
	 */
	@Override
	public Procedure characterStreamParam(final String paramName, final Reader value, final int len) {
		context().characterStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader)
	 */
	@Override
	public Procedure characterStreamParam(final String paramName, final Reader value) {
		context().characterStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public Procedure binaryStreamParam(final String paramName, final InputStream value, final int len) {
		context().binaryStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public Procedure binaryStreamParam(final String paramName, final InputStream value) {
		context().binaryStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public Procedure asciiStreamParam(final String paramName, final InputStream value, final int len) {
		context().asciiStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public Procedure asciiStreamParam(final String paramName, final InputStream value) {
		context().asciiStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.Procedure#call()
	 */
	@Override
	public Map<String, Object> call() throws SQLException {
		return agent.procedure(context());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int)
	 */
	@Override
	public Procedure retry(final int count) {
		return retry(count, 0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int, int)
	 */
	@Override
	public Procedure retry(final int count, final int waitTime) {
		context().setMaxRetryCount(count).setRetryWaitTime(waitTime);
		return this;
	}
}
