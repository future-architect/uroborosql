package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlQuery実装
 *
 * @author H.Sugimoto
 */
final class SqlQueryImpl extends AbstractSqlFluent<SqlQuery> implements SqlQuery {
	private final SqlAgent agent;

	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context SqlContext
	 */
	SqlQueryImpl(final SqlAgent agent, final SqlContext context) {
		super(context);
		this.agent = agent;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream()
	 */
	@Override
	public Stream<Map<String, Object>> stream() {
		return stream(new MapResultSetConverter());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream(jp.co.future.uroborosql.converter.ResultSetConverter)
	 */
	@Override
	public <T> Stream<T> stream(final ResultSetConverter<T> converter) {
		try {
			return agent.query(context(), converter);
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#resultSet()
	 */
	@Override
	public ResultSet resultSet() {
		try {
			return agent.query(context());
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first()
	 */
	@Override
	public Map<String, Object> first() {
		return first(CaseFormat.UPPER_SNAKE_CASE);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public Map<String, Object> first(final CaseFormat caseFormat) {
		Optional<Map<String, Object>> first = stream(new MapResultSetConverter(caseFormat)).findFirst();
		return first.orElseThrow(() -> new DataNotFoundException("query result is empty."));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public List<Map<String, Object>> collect(final CaseFormat caseFormat) {
		try {
			return agent.query(context(), caseFormat);
		} catch (SQLException e) {
			throw new UroborosqlSQLException(e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect()
	 */
	@Override
	public List<Map<String, Object>> collect() {
		return collect(CaseFormat.UPPER_SNAKE_CASE);
	}
}
