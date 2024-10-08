/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.converter.EntityResultSetConverter;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.converter.ScalarResultSetConverter;
import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlQuery実装
 *
 * @author H.Sugimoto
 */
final class SqlQueryImpl extends AbstractSqlFluent<SqlQuery> implements SqlQuery {
	/**
	 * コンストラクタ
	 *
	 * @param agent SqlAgent
	 * @param context ExecutionContext
	 */
	SqlQueryImpl(final SqlAgent agent, final ExecutionContext context) {
		super(agent, context);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first()
	 */
	@Override
	public Map<String, Object> first() {
		return first(agent().getMapKeyCaseFormat());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public Map<String, Object> first(final CaseFormat caseFormat) {
		return findFirst(caseFormat).orElseThrow(() -> new DataNotFoundException("query result is empty."));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#first(Class)
	 */
	@Override
	public <T> T first(final Class<T> type) {
		return findFirst(type).orElseThrow(() -> new DataNotFoundException("query result is empty."));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#findFirst()
	 */
	@Override
	public Optional<Map<String, Object>> findFirst() {
		return findFirst(agent().getMapKeyCaseFormat());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#findFirst(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public Optional<Map<String, Object>> findFirst(final CaseFormat caseFormat) {
		try (var stream = stream(caseFormat)) {
			return stream.findFirst();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#findFirst(java.lang.Class)
	 */
	@Override
	public <T> Optional<T> findFirst(final Class<T> type) {
		try (var stream = stream(type)) {
			return stream.findFirst();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#one()
	 */
	@Override
	public Map<String, Object> one() {
		return one(agent().getMapKeyCaseFormat());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#one(CaseFormat)
	 */
	@Override
	public Map<String, Object> one(final CaseFormat caseFormat) {
		return findOne(caseFormat).orElseThrow(() -> new DataNotFoundException("query result is empty."));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#one(Class)
	 */
	@Override
	public <T> T one(final Class<T> type) {
		return findOne(type).orElseThrow(() -> new DataNotFoundException("query result is empty."));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#findOne()
	 */
	@Override
	public Optional<Map<String, Object>> findOne() {
		return findOne(agent().getMapKeyCaseFormat());
	}

	@Override
	public Optional<Map<String, Object>> findOne(final CaseFormat caseFormat) {
		try (var stream = stream(caseFormat)) {
			var resultList = stream.limit(2).collect(Collectors.toList());
			if (resultList.size() > 1) {
				throw new DataNonUniqueException("two or more query results.");
			}
			return resultList.stream().findFirst();
		}
	}

	@Override
	public <T> Optional<T> findOne(final Class<T> type) {
		try (var stream = stream(type)) {
			var resultList = stream.limit(2).collect(Collectors.toList());
			if (resultList.size() > 1) {
				throw new DataNonUniqueException("two or more query results.");
			}
			return resultList.stream().findFirst();
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
			return agent().query(context());
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect()
	 */
	@Override
	public List<Map<String, Object>> collect() {
		return collect(agent().getMapKeyCaseFormat());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public List<Map<String, Object>> collect(final CaseFormat caseFormat) {
		try {
			return agent().query(context(), caseFormat);
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#collect(java.lang.Class)
	 */
	@Override
	public <T> List<T> collect(final Class<T> type) {
		try (var stream = stream(type)) {
			return stream.collect(Collectors.toList());
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream()
	 */
	@Override
	public Stream<Map<String, Object>> stream() {
		return stream(agent().getMapKeyCaseFormat());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream(jp.co.future.uroborosql.converter.ResultSetConverter)
	 */
	@Override
	public <T> Stream<T> stream(final ResultSetConverter<T> converter) {
		try {
			return agent().query(context(), converter);
		} catch (SQLException ex) {
			throw new UroborosqlSQLException(ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream(jp.co.future.uroborosql.utils.CaseFormat)
	 */
	@Override
	public Stream<Map<String, Object>> stream(final CaseFormat caseFormat) {
		return stream(new MapResultSetConverter(agent().getSqlConfig(), caseFormat));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#stream(java.lang.Class)
	 */
	@Override
	public <T> Stream<T> stream(final Class<T> type) {
		if (type == null) {
			throw new IllegalArgumentException("Argument 'type' is required.");
		}
		var manager = new PropertyMapperManager(agent().getSqlConfig().getClock());
		if (ScalarResultSetConverter.accept(type)) {
			return stream(new ScalarResultSetConverter<>(null, type, manager));
		} else {
			return stream(new EntityResultSetConverter<>(context().getSchema(), type, manager));
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#select(java.lang.Class)
	 */
	@Override
	public <T> Stream<T> select(final Class<T> type) {
		return select(null, type);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlQuery#select(java.lang.String, java.lang.Class)
	 */
	@Override
	public <T> Stream<T> select(final String col, final Class<T> type) {
		if (type == null) {
			throw new IllegalArgumentException("Argument 'type' is required.");
		}
		if (!ScalarResultSetConverter.accept(type)) {
			throw new IllegalArgumentException(type.getName() + " is not supported.");
		}
		return stream(new ScalarResultSetConverter<>(col, type,
				new PropertyMapperManager(agent().getSqlConfig().getClock())));
	}

}
