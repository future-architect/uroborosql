/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException.EntityProcKind;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.mapping.TableMetadata.Column;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityQueryImpl<E> extends AbstractExtractionCondition<SqlEntityQuery<E>> implements SqlEntityQuery<E> {
	private final EntityHandler<?> entityHandler;
	private final Class<? extends E> entityType;
	private final List<SortOrder> sortOrders;
	private long limit;
	private long offset;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context SqlContext
	 * @param entityType エンティティタイプ
	 */
	SqlEntityQueryImpl(final SqlAgent agent, final EntityHandler<?> entityHandler, final TableMetadata tableMetadata,
			final SqlContext context, final Class<? extends E> entityType) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
		this.entityType = entityType;
		this.sortOrders = new ArrayList<>();
		this.limit = -1;
		this.offset = -1;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#collect()
	 */
	@Override
	public List<E> collect() {
		try (Stream<E> stream = stream()) {
			return stream.collect(Collectors.toList());
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#first()
	 */
	@Override
	public Optional<E> first() {
		try (Stream<E> stream = stream()) {
			return stream.findFirst();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#stream()
	 */
	@Override
	public Stream<E> stream() {
		try {
			StringBuilder sql = new StringBuilder(context().getSql()).append(getWhereClause())
					.append(getOrderByClause());
			Dialect dialect = agent().getSqlConfig().getDialect();
			if (dialect.supportsLimitClause()) {
				sql.append(dialect.getLimitClause(this.limit, this.offset));
			}

			context().setSql(sql.toString());
			return this.entityHandler.doSelect(agent(), context(), this.entityType);
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}
	}

	/**
	 * 引数で指定したカラム名に一致するMappingColumnを探す。見つからなかった場合は例外をスローする
	 *
	 * @param col 検索対象のカラム名（キャメルケース）
	 * @return colに対するMappingColumn
	 * @throws UroborosqlRuntimeException colに該当するMappingColumnが見つからなかった場合
	 */
	private MappingColumn findMappingColumn(final String col) {
		return Arrays.stream(MappingUtils.getMappingColumns(entityType))
				.filter(c -> col.equalsIgnoreCase(c.getCamelName())).findFirst()
				.orElseThrow(() -> new UroborosqlRuntimeException("No such column found. col=" + col));
	}

	/**
	 * 集計関数で集計する元となるSQL文字列を生成する.<br>
	 * 集計する場合はソートする必要がないので order by が除かれている
	 *
	 * @return 集計関数で集計する元となるSQL文字列
	 */
	private StringBuilder aggregationSourceSql() {
		StringBuilder sql = new StringBuilder(context().getSql()).append(getWhereClause());
		Dialect dialect = agent().getSqlConfig().getDialect();
		if (dialect.supportsLimitClause()) {
			sql.append(dialect.getLimitClause(this.limit, this.offset));
		}
		return sql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#count()
	 */
	@Override
	public long count() {
		return count(null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#count(java.lang.String)
	 */
	@Override
	public long count(final String col) {
		String expr = col != null ? findMappingColumn(col).getName() : "*";
		StringBuilder sql = new StringBuilder("select count(").append(expr).append(") from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try (ResultSet rs = agent().query(context())) {
			rs.next();
			return rs.getLong(1);
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#sum(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T> T sum(final String col) {
		MappingColumn mappingColumn = findMappingColumn(col);
		Class<?> rawType = mappingColumn.getJavaType().getRawType();
		if (!(short.class.equals(rawType) ||
				int.class.equals(rawType) ||
				long.class.equals(rawType) ||
				float.class.equals(rawType) ||
				double.class.equals(rawType) ||
				Number.class.isAssignableFrom(mappingColumn.getJavaType().getRawType()))) {
			throw new UroborosqlRuntimeException("Column is not of type Number. col=" + col);
		}
		StringBuilder sql = new StringBuilder("select sum(t_.").append(mappingColumn.getName()).append(") as ")
				.append(mappingColumn.getName()).append(" from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try {
			return (T) mappingColumn
					.getValue(this.entityHandler.doSelect(agent(), context(), this.entityType).findFirst().get());
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#min(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T> T min(final String col) {
		MappingColumn mappingColumn = findMappingColumn(col);
		StringBuilder sql = new StringBuilder("select min(t_.").append(mappingColumn.getName()).append(") as ")
				.append(mappingColumn.getName()).append(" from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try {
			return (T) mappingColumn
					.getValue(this.entityHandler.doSelect(agent(), context(), this.entityType).findFirst().get());
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#max(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T> T max(final String col) {
		MappingColumn mappingColumn = findMappingColumn(col);
		StringBuilder sql = new StringBuilder("select max(t_.").append(mappingColumn.getName()).append(") as ")
				.append(mappingColumn.getName()).append(" from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try {
			return (T) mappingColumn
					.getValue(this.entityHandler.doSelect(agent(), context(), this.entityType).findFirst().get());
		} catch (SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}
	}

	/**
	 * ORDER BY句を生成する
	 *
	 * @return ORDER BY句の文字列
	 */
	@SuppressWarnings("unchecked")
	private String getOrderByClause() {
		boolean firstFlag = true;
		List<TableMetadata.Column> keys;
		Map<TableMetadata.Column, SortOrder> existsSortOrders = new HashMap<>();

		if (this.sortOrders.isEmpty()) {
			// ソート条件の指定がない場合は主キーでソートする
			keys = (List<TableMetadata.Column>) this.tableMetadata.getKeyColumns();
			for (Column key : keys) {
				existsSortOrders.put(key, new SortOrder(key.getCamelColumnName(), Order.ASCENDING));
			}
		} else {
			// ソート条件の指定がある場合は指定されたカラムでソートする
			keys = new ArrayList<>();
			for (SortOrder sortOrder : sortOrders) {
				String snakeCol = CaseFormat.UPPER_SNAKE_CASE.convert(sortOrder.getCol());
				for (TableMetadata.Column metaCol : this.tableMetadata.getColumns()) {
					if (snakeCol.equalsIgnoreCase(metaCol.getColumnName())) {
						keys.add(metaCol);
						existsSortOrders.put(metaCol, sortOrder);
						break;
					}
				}
			}
		}

		if (!keys.isEmpty()) {
			StringBuilder sql = new StringBuilder();
			Dialect dialect = agent().getSqlConfig().getDialect();
			sql.append("ORDER BY").append(System.lineSeparator());
			firstFlag = true;
			for (final TableMetadata.Column key : keys) {
				SortOrder sortOrder = existsSortOrders.get(key);
				sql.append("\t");
				if (firstFlag) {
					sql.append("  ");
					firstFlag = false;
				} else {
					sql.append(", ");
				}
				sql.append(key.getColumnIdentifier()).append(" ").append(sortOrder.getOrder().toString());
				if (dialect.supportsNullValuesOrdering()) {
					sql.append(" ").append(sortOrder.getNulls().toString());
				}
				sql.append(System.lineSeparator());
			}
			return sql.toString();
		} else {
			return "";
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#asc(java.lang.String[])
	 */
	@Override
	public SqlEntityQuery<E> asc(final String... cols) {
		for (String col : cols) {
			this.sortOrders.add(new SortOrder(col, Order.ASCENDING));
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#asc(java.lang.String, jp.co.future.uroborosql.fluent.SqlEntityQuery.Nulls)
	 */
	@Override
	public SqlEntityQuery<E> asc(final String col, final Nulls nulls) {
		this.sortOrders.add(new SortOrder(col, Order.ASCENDING, nulls));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#desc(java.lang.String[])
	 */
	@Override
	public SqlEntityQuery<E> desc(final String... cols) {
		for (String col : cols) {
			this.sortOrders.add(new SortOrder(col, Order.DESCENDING));
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	s	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#desc(java.lang.String, jp.co.future.uroborosql.fluent.SqlEntityQuery.Nulls)
	 */
	@Override
	public SqlEntityQuery<E> desc(final String col, final Nulls nulls) {
		this.sortOrders.add(new SortOrder(col, Order.DESCENDING, nulls));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#limit(long)
	 */
	@Override
	public SqlEntityQuery<E> limit(final long limit) {
		if (!agent().getSqlConfig().getDialect().supportsLimitClause()) {
			throw new UroborosqlRuntimeException("Unsupported limit clause.");
		}
		this.limit = limit;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#offset(long)
	 */
	@Override
	public SqlEntityQuery<E> offset(final long offset) {
		if (!agent().getSqlConfig().getDialect().supportsLimitClause()) {
			throw new UroborosqlRuntimeException("Unsupported offset clause.");
		}
		this.offset = offset;
		return this;
	}

	/**
	 * Sort Order
	 */
	private static class SortOrder {
		private final String col;
		private final Order order;
		private final Nulls nulls;

		/**
		 * Constructor
		 *
		 * @param col sort column name (camelCase)
		 * @param order {@link Order}
		 */
		SortOrder(final String col, final Order order) {
			this(col, order, Nulls.LAST);
		}

		/**
		 * Constructor
		 *
		 * @param col sort column name (camelCase)
		 * @param order {@link Order}
		 * @param nulls {@link Nulls}
		 */
		SortOrder(final String col, final Order order, final Nulls nulls) {
			if (col == null) {
				throw new UroborosqlRuntimeException("argment col is required.");
			}
			this.col = col;
			this.order = order != null ? order : Order.ASCENDING;
			this.nulls = nulls != null ? nulls : Nulls.LAST;
		}

		final String getCol() {
			return col;
		}

		final Order getOrder() {
			return order;
		}

		final Nulls getNulls() {
			return nulls;
		}
	}
}
