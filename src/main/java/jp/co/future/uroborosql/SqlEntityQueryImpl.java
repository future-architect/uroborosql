/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.event.AfterEntityQueryEvent;
import jp.co.future.uroborosql.event.BeforeEntityQueryEvent;
import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.utils.BeanAccessor;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityQueryImpl<E> extends AbstractExtractionCondition<SqlEntityQuery<E>> implements SqlEntityQuery<E> {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.log");

	private final EntityHandler<?> entityHandler;
	private final Class<? extends E> entityType;
	private final List<SortOrder> sortOrders;
	private final List<String> optimizerHints;
	private final Dialect dialect;
	private long limit;
	private long offset;
	private ForUpdateType forUpdateType;
	private int waitSeconds;

	private final List<String> includeColumns;
	private final List<String> excludeColumns;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param entityHandler EntityHandler
	 * @param tableMetadata TableMetadata
	 * @param context ExecutionContext
	 * @param entityType エンティティタイプ
	 */
	SqlEntityQueryImpl(final SqlAgent agent, final EntityHandler<?> entityHandler, final TableMetadata tableMetadata,
			final ExecutionContext context, final Class<? extends E> entityType) {
		super(agent, tableMetadata, context);
		this.entityHandler = entityHandler;
		this.entityType = entityType;
		this.sortOrders = new ArrayList<>();
		this.optimizerHints = new ArrayList<>();
		this.dialect = agent.getSqlConfig().getDialect();
		this.limit = -1;
		this.offset = -1;
		this.forUpdateType = null;
		this.waitSeconds = -1;
		this.includeColumns = new ArrayList<>();
		this.excludeColumns = new ArrayList<>();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#collect()
	 */
	@Override
	public List<E> collect() {
		try (var stream = stream()) {
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
		try (var stream = stream()) {
			return stream.findFirst();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#one()
	 */
	@Override
	public Optional<E> one() {
		try (var stream = stream()) {
			var entities = stream.limit(2).collect(Collectors.toList());
			if (entities.size() > 1) {
				throw new DataNonUniqueException("two or more query results.");
			}
			return entities.stream().findFirst();
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#stream()
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Stream<E> stream() {
		try {
			var selectClause = context().getSql();
			if (!includeColumns.isEmpty() || !excludeColumns.isEmpty()) {
				// 除外対象カラムを取得する
				List<? extends TableMetadata.Column> excludeCols = List.of();
				if (!includeColumns.isEmpty()) {
					excludeCols = tableMetadata().getColumns().stream()
							.filter(col -> !includeColumns.contains(col.getCamelColumnName()))
							.collect(Collectors.toList());
					if (excludeCols.size() == tableMetadata().getColumns().size()) {
						// includeColumnsに含まれるカラムが1つもselect句に含まれない場合は実行時例外とする
						throw new UroborosqlRuntimeException("None of the includeColumns matches the column name.");
					}
				} else if (!excludeColumns.isEmpty()) {
					excludeCols = tableMetadata().getColumns().stream()
							.filter(col -> excludeColumns.contains(col.getCamelColumnName()))
							.collect(Collectors.toList());
				}
				if (!excludeCols.isEmpty()) {
					// 除外対象カラムをselect句から除外する置換処理を行う
					selectClause = selectClause.replaceAll(excludeCols.stream()
							.map(TableMetadata.Column::getColumnIdentifier)
							.collect(Collectors.joining("|", "\\s*,*\\s+(", ").+")), "");
					// SELECT句の直後にカンマがくる場合はそのカンマを除外する
					selectClause = selectClause.replaceFirst("(SELECT.+\\s*)(,)", "$1 ");
				}
			}
			var sql = new StringBuilder(selectClause).append(getWhereClause())
					.append(getOrderByClause());
			if (dialect.supportsLimitClause()) {
				sql.append(dialect.getLimitClause(this.limit, this.offset));
			}
			if (this.forUpdateType != null) {
				sql = dialect.addForUpdateClause(sql, this.forUpdateType, this.waitSeconds);
			}
			if (!this.optimizerHints.isEmpty()) {
				sql = dialect.addOptimizerHints(sql, this.optimizerHints);
			}
			context().setSql(sql.toString());

			// EntityQuery実行前イベント発行
			var eventListenerHolder = agent().getSqlConfig().getEventListenerHolder();
			if (eventListenerHolder.hasBeforeEntityQueryListener()) {
				var eventObj = new BeforeEntityQueryEvent(context(), null, this.entityType);
				for (var listener : eventListenerHolder.getBeforeEntityQueryListeners()) {
					listener.accept(eventObj);
				}
			}

			Stream<E> results = this.entityHandler.doSelect(agent(), context(), this.entityType);

			// EntityQuery実行後イベント発行
			if (eventListenerHolder.hasAfterEntityQueryListener()) {
				var eventObj = new AfterEntityQueryEvent(context(), null, this.entityType, results);
				for (var listener : eventListenerHolder.getAfterEntityQueryListeners()) {
					listener.accept(eventObj);
				}
				return (Stream<E>) eventObj.getResults();
			} else {
				return results;
			}
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#select(java.lang.String, java.lang.Class)
	 */
	@Override
	public <C> Stream<C> select(final String col, final Class<C> type) {
		var fieldName = CaseFormat.CAMEL_CASE.convert(col);
		var field = BeanAccessor.fields(this.entityType).stream()
				.filter(f -> f.getName().equalsIgnoreCase(fieldName))
				.findFirst()
				.orElseThrow(() -> new UroborosqlRuntimeException(
						"field:" + fieldName + " not found in " + this.entityType.getSimpleName() + "."));
		includeColumns(fieldName);

		return stream().map(e -> type.cast(BeanAccessor.value(field, e)));
	}

	/**
	 * 集計関数で集計する元となるSQL文字列を生成する.<br>
	 * 集計する場合はソートする必要がないので order by が除かれている
	 *
	 * @return 集計関数で集計する元となるSQL文字列
	 */
	private StringBuilder aggregationSourceSql() {
		var sql = new StringBuilder(context().getSql()).append(getWhereClause());
		if (dialect.supportsLimitClause()) {
			sql.append(dialect.getLimitClause(this.limit, this.offset));
		}
		if (this.forUpdateType != null) {
			sql = dialect.addForUpdateClause(sql, this.forUpdateType, this.waitSeconds);
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
		var expr = col != null
				? tableMetadata().getColumn(CaseFormat.CAMEL_CASE.convert(col)).getColumnIdentifier()
				: "*";
		var sql = new StringBuilder("select count(").append(expr).append(") from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try (var rs = agent().query(context())) {
			rs.next();
			return rs.getLong(1);
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
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
		var camelColumnName = CaseFormat.CAMEL_CASE.convert(col);
		var mappingColumn = MappingUtils.getMappingColumn(context().getSchema(), this.entityType, camelColumnName);
		if (!mappingColumn.isNumber()) {
			throw new UroborosqlRuntimeException("Column is not of type Number. col=" + camelColumnName);
		}
		var column = tableMetadata().getColumn(camelColumnName);
		var sql = new StringBuilder("select sum(t_.").append(column.getColumnIdentifier()).append(") as ")
				.append(column.getColumnIdentifier()).append(" from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try {
			return (T) mappingColumn
					.getValue(this.entityHandler.doSelect(agent(), context(), this.entityType).findFirst().get());
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
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
		var camelColumnName = CaseFormat.CAMEL_CASE.convert(col);
		var mappingColumn = MappingUtils.getMappingColumn(context().getSchema(), this.entityType, camelColumnName);
		var column = tableMetadata().getColumn(camelColumnName);
		var sql = new StringBuilder("select min(t_.").append(column.getColumnIdentifier()).append(") as ")
				.append(column.getColumnIdentifier()).append(" from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try {
			return (T) mappingColumn
					.getValue(this.entityHandler.doSelect(agent(), context(), this.entityType).findFirst().get());
		} catch (SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
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
		var camelColumnName = CaseFormat.CAMEL_CASE.convert(col);
		var mappingColumn = MappingUtils.getMappingColumn(context().getSchema(), this.entityType, camelColumnName);
		var column = tableMetadata().getColumn(camelColumnName);
		var sql = new StringBuilder("select max(t_.").append(column.getColumnIdentifier()).append(") as ")
				.append(column.getColumnIdentifier()).append(" from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try {
			return (T) mappingColumn
					.getValue(this.entityHandler.doSelect(agent(), context(), this.entityType).findFirst().get());
		} catch (SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#exists(java.lang.Runnable)
	 */
	@Override
	public void exists(final Runnable runnable) {
		var sql = new StringBuilder("select 1 from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try (var rs = agent().query(context())) {
			if (rs.next()) {
				runnable.run();
			}
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notExists(java.lang.Runnable)
	 */
	@Override
	public void notExists(final Runnable runnable) {
		var sql = new StringBuilder("select 1 from (")
				.append(System.lineSeparator())
				.append(aggregationSourceSql())
				.append(System.lineSeparator())
				.append(") t_");
		context().setSql(sql.toString());
		try (var rs = agent().query(context())) {
			if (!rs.next()) {
				runnable.run();
			}
		} catch (final SQLException ex) {
			throw new EntitySqlRuntimeException(context().getSqlKind(), ex);
		}
	}

	/**
	 * ORDER BY句を生成する
	 *
	 * @return ORDER BY句の文字列
	 */
	@SuppressWarnings("unchecked")
	private String getOrderByClause() {
		var firstFlag = true;
		List<TableMetadata.Column> keys;
		var existsSortOrders = new HashMap<TableMetadata.Column, SortOrder>();

		if (this.sortOrders.isEmpty()) {
			// ソート条件の指定がない場合は主キーでソートする
			keys = (List<TableMetadata.Column>) tableMetadata().getKeyColumns();
			for (var key : keys) {
				existsSortOrders.put(key, new SortOrder(key.getCamelColumnName(), Order.ASCENDING));
			}
		} else {
			// ソート条件の指定がある場合は指定されたカラムでソートする
			keys = new ArrayList<>();
			for (var sortOrder : sortOrders) {
				for (var metaCol : tableMetadata().getColumns()) {
					if (sortOrder.getCol().equals(metaCol.getCamelColumnName())) {
						keys.add(metaCol);
						existsSortOrders.put(metaCol, sortOrder);
						break;
					}
				}
			}
		}

		if (!keys.isEmpty()) {
			var sql = new StringBuilder();
			sql.append("ORDER BY").append(System.lineSeparator());
			firstFlag = true;
			for (final TableMetadata.Column key : keys) {
				var sortOrder = existsSortOrders.get(key);
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
		for (var col : cols) {
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
		for (var col : cols) {
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
		if (!dialect.supportsLimitClause()) {
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
		if (!dialect.supportsLimitClause()) {
			throw new UroborosqlRuntimeException("Unsupported offset clause.");
		}
		this.offset = offset;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#forUpdate()
	 */
	@Override
	public SqlEntityQuery<E> forUpdate() {
		if (dialect.supportsForUpdate()) {
			this.forUpdateType = ForUpdateType.NORMAL;
			return this;
		} else {
			throw new UroborosqlRuntimeException("Unsupported for update clause.");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#forUpdateNoWait()
	 */
	@Override
	public SqlEntityQuery<E> forUpdateNoWait() {
		if (dialect.supportsForUpdateNoWait()) {
			this.forUpdateType = ForUpdateType.NOWAIT;
			return this;
		} else if (!agent().getSqlConfig().getSqlAgentProvider().isStrictForUpdateType()
				&& dialect.supportsForUpdate()) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("'FOR UPDATE NOWAIT' is not supported. Set 'FOR UPDATE' instead.");
			}
			this.forUpdateType = ForUpdateType.NORMAL;
			return this;
		} else {
			throw new UroborosqlRuntimeException("Unsupported for update nowait clause.");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#forUpdateWait()
	 */
	@Override
	public SqlEntityQuery<E> forUpdateWait() {
		if (dialect.supportsForUpdateWait()) {
			return forUpdateWait(agent().getSqlConfig().getSqlAgentProvider().getDefaultForUpdateWaitSeconds());
		} else if (!agent().getSqlConfig().getSqlAgentProvider().isStrictForUpdateType()
				&& dialect.supportsForUpdate()) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("'FOR UPDATE WAIT' is not supported. Set 'FOR UPDATE' instead.");
			}
			this.forUpdateType = ForUpdateType.NORMAL;
			return this;
		} else {
			throw new UroborosqlRuntimeException("Unsupported for update wait clause.");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#forUpdateWait(int)
	 */
	@Override
	public SqlEntityQuery<E> forUpdateWait(final int waitSeconds) {
		if (dialect.supportsForUpdateWait()) {
			this.forUpdateType = ForUpdateType.WAIT;
			this.waitSeconds = waitSeconds;
			return this;
		} else if (!agent().getSqlConfig().getSqlAgentProvider().isStrictForUpdateType()
				&& dialect.supportsForUpdate()) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("'FOR UPDATE WAIT' is not supported. Set 'FOR UPDATE' instead.");
			}
			this.forUpdateType = ForUpdateType.NORMAL;
			return this;
		} else {
			throw new UroborosqlRuntimeException("Unsupported for update wait clause.");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#hint(java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> hint(final String hint) {
		if (dialect.supportsOptimizerHints()) {
			this.optimizerHints.add(hint);
		} else {
			if (LOG.isWarnEnabled()) {
				LOG.warn("Optimizer Hints is not supported.");
			}
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#includeColumns(java.lang.String[])
	 */
	@Override
	public SqlEntityQuery<E> includeColumns(final String... cols) {
		if (cols != null && cols.length != 0) {
			for (var col : cols) {
				includeColumns.add(CaseFormat.CAMEL_CASE.convert(col));
			}
		}
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#excludeColumns(java.lang.String[])
	 */
	@Override
	public SqlEntityQuery<E> excludeColumns(final String... cols) {
		if (cols != null && cols.length != 0) {
			for (var col : cols) {
				excludeColumns.add(CaseFormat.CAMEL_CASE.convert(col));
			}
		}
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
				throw new UroborosqlRuntimeException("argument col is required.");
			}
			this.col = CaseFormat.CAMEL_CASE.convert(col);
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
