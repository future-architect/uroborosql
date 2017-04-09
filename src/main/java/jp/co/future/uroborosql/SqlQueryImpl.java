package jp.co.future.uroborosql;

import java.io.InputStream;
import java.io.Reader;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLType;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlQuery実装
 *
 * @author H.Sugimoto
 */
final class SqlQueryImpl implements SqlQuery {
	private final SqlAgent agent;
	private final SqlContext context;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	SqlQueryImpl(final SqlAgent agent, final SqlContext context) {
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
	public SqlQuery paramList(final String paramName, final Object... value) {
		context().paramList(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#paramMap(Map<String, Object>)
	 */
	@Override
	public SqlQuery paramMap(final Map<String, Object> paramMap) {
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
	public SqlQuery param(final String paramName, final Object value, final int sqlType) {
		context().param(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object, java.sql.SQLType)
	 */
	@Override
	public SqlQuery param(final String paramName, final Object value, final SQLType sqlType) {
		context().param(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlQuery param(final String paramName, final Object value) {
		context().param(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#param(jp.co.future.uroborosql.parameter.Parameter)
	 */
	@Override
	public SqlQuery param(final Parameter param) {
		context().param(param);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, int)
	 */
	@Override
	public SqlQuery outParam(final String paramName, final int sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#outParam(java.lang.String, java.sql.SQLType)
	 */
	@Override
	public SqlQuery outParam(final String paramName, final SQLType sqlType) {
		context().outParam(paramName, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#inOutParam(java.lang.String, java.lang.Object, int)
	 */
	@Override
	public SqlQuery inOutParam(final String paramName, final Object value, final int sqlType) {
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
	public SqlQuery inOutParam(final String paramName, final Object value, final SQLType sqlType) {
		context().inOutParam(paramName, value, sqlType);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader, int)
	 */
	@Override
	public SqlQuery characterStreamParam(final String paramName, final Reader value, final int len) {
		context().characterStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#characterStreamParam(java.lang.String, java.io.Reader)
	 */
	@Override
	public SqlQuery characterStreamParam(final String paramName, final Reader value) {
		context().characterStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public SqlQuery binaryStreamParam(final String paramName, final InputStream value, final int len) {
		context().binaryStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#binaryStreamParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public SqlQuery binaryStreamParam(final String paramName, final InputStream value) {
		context().binaryStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream, int)
	 */
	@Override
	public SqlQuery asciiStreamParam(final String paramName, final InputStream value, final int len) {
		context().asciiStreamParam(paramName, value, len);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#asciiStreamParam(java.lang.String, java.io.InputStream)
	 */
	@Override
	public SqlQuery asciiStreamParam(final String paramName, final InputStream value) {
		context().asciiStreamParam(paramName, value);
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream()
	 */
	@Override
	public Stream<Map<String, Object>> stream() throws SQLException {
		return stream(new MapResultSetConverter());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream(jp.co.future.uroborosql.converter.ResultSetConverter)
	 */
	@Override
	public <T> Stream<T> stream(final ResultSetConverter<T> converter) throws SQLException {
		return agent.query(context(), converter);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#resultSet()
	 */
	@Override
	public ResultSet resultSet() throws SQLException {
		return agent.query(context());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first()
	 */
	@Override
	public Map<String, Object> first() throws DataNotFoundException, SQLException {
		return first(CaseFormat.SnakeCase);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public Map<String, Object> first(final CaseFormat caseFormat) throws DataNotFoundException, SQLException {
		Optional<Map<String, Object>> first = stream(new MapResultSetConverter(caseFormat)).findFirst();
		return first.orElseThrow(() -> new DataNotFoundException("query result is empty."));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public List<Map<String, Object>> collect(final CaseFormat caseFormat) throws SQLException {
		return agent.query(context(), caseFormat);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect()
	 */
	@Override
	public List<Map<String, Object>> collect() throws SQLException {
		return collect(CaseFormat.SnakeCase);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int)
	 */
	@Override
	public SqlQuery retry(final int count) {
		return retry(count, 0);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlFluent#retry(int, int)
	 */
	@Override
	public SqlQuery retry(final int count, final int waitTime) {
		context().setMaxRetryCount(count).setRetryWaitTime(waitTime);
		return this;
	}

}
