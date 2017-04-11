package jp.co.future.uroborosql;

import java.io.InputStream;
import java.io.Reader;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.Map;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * SqlUpdate実装
 *
 * @author H.Sugimoto
 */
final class SqlUpdateImpl implements SqlUpdate {
	private final SqlAgent agent;
	private final SqlContext context;
	/** バッチ処理を行うかどうか */
	boolean batch = false;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	SqlUpdateImpl(final SqlAgent agent, final SqlContext context) {
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
	public SqlUpdate paramList(final String paramName, final Object... value) {
		context().paramList(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(Map<String, Object>)
	 */
	@Override
	public SqlUpdate paramMap(final Map<String, Object> paramMap) {
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
	public SqlUpdate param(final String paramName, final Object value, final int sqlType) {
		context().param(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public SqlUpdate param(final String paramName, final Object value, final SQLType sqlType) {
		context().param(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlUpdate param(final String paramName, final Object value) {
		context().param(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@Override
	public SqlUpdate param(final Parameter param) {
		context().param(param);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, int)
	 */
	@Override
	public SqlUpdate outParam(final String paramName, final int sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, java.sql.SQLType)
	 */
	@Override
	public SqlUpdate outParam(final String paramName, final SQLType sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public SqlUpdate inOutParam(final String paramName, final Object value, final int sqlType) {
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
	public SqlUpdate inOutParam(final String paramName, final Object value, final SQLType sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader, int)
	 */
	@Override
	public SqlUpdate characterStreamParam(final String paramName, final Reader value, final int len) {
		context().characterStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader)
	 */
	@Override
	public SqlUpdate characterStreamParam(final String paramName, final Reader value) {
		context().characterStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public SqlUpdate binaryStreamParam(final String paramName, final InputStream value, final int len) {
		context().binaryStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public SqlUpdate binaryStreamParam(final String paramName, final InputStream value) {
		context().binaryStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public SqlUpdate asciiStreamParam(final String paramName, final InputStream value, final int len) {
		context().asciiStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public SqlUpdate asciiStreamParam(final String paramName, final InputStream value) {
		context().asciiStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#addBatch()
	 */
	@Override
	public SqlUpdate addBatch() {
		context().addBatch();
		batch = true;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#count()
	 */
	@Override
	public int count() throws SQLException {
		if (batch) {
			throw new IllegalStateException("すでにaddBatch()でパラメータが設定されているため、batch()を呼び出してください");
		}
		return agent.update(context());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlUpdate#batch()
	 */
	@Override
	public int[] batch() throws SQLException {
		if (!batch) {
			addBatch();
		}
		return agent.batch(context());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int)
	 */
	@Override
	public SqlUpdate retry(final int count) {
		return retry(count, 0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int, int)
	 */
	@Override
	public SqlUpdate retry(final int count, final int waitTime) {
		context().setMaxRetryCount(count).setRetryWaitTime(waitTime);
		return this;
	}

}
