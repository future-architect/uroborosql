/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
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
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.mapping.TableMetadata.Column;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlEntityQuery実装
 *
 * @param <E> Entity型
 * @author ota
 */
final class SqlEntityQueryImpl<E> extends AbstractSqlFluent<SqlEntityQuery<E>> implements SqlEntityQuery<E> {
	private final EntityHandler<?> entityHandler;
	private final TableMetadata tableMetadata;
	private final Class<? extends E> entityType;
	private CharSequence rawString;
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
			final SqlContext context,
			final Class<? extends E> entityType) {
		super(agent, context);
		this.entityHandler = entityHandler;
		this.tableMetadata = tableMetadata;
		this.entityType = entityType;
		this.rawString = null;
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
			StringBuilder sql = new StringBuilder(context().getSql()).append(whereAndOrderClause());
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
	 * SELECT文のWHERE句とORDER BY句を生成する
	 *
	 * @return WHERE句とORDER BY句の文字列
	 */
	@SuppressWarnings("unchecked")
	private String whereAndOrderClause() {
		final List<? extends TableMetadata.Column> columns = this.tableMetadata.getColumns();

		StringBuilder where = new StringBuilder();
		for (final TableMetadata.Column col : columns) {
			final String camelColName = col.getCamelColumnName();

			Parameter param = context().getParam(camelColName);
			if (param != null) {
				if (param.getValue() instanceof Operator) {
					Operator ope = (Operator) param.getValue();
					where.append("\t").append("AND ").append(col.getColumnIdentifier())
					.append(ope.toConditionString()).append(System.lineSeparator());
				} else {
					where.append("\t").append("AND ").append(col.getColumnIdentifier())
					.append(" = ").append("/*").append(camelColName).append("*/''")
					.append(System.lineSeparator());
				}
			}
		}
		if (rawString != null) {
			where.append(rawString).append(System.lineSeparator());
		}

		StringBuilder sql = new StringBuilder();
		if (where.length() > 0) {
			sql.append("WHERE").append(System.lineSeparator()).append(where.toString());
		}

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
				for (TableMetadata.Column metaCol : columns) {
					if (snakeCol.equalsIgnoreCase(metaCol.getColumnName())) {
						keys.add(metaCol);
						existsSortOrders.put(metaCol, sortOrder);
						break;
					}
				}
			}
		}

		if (!keys.isEmpty()) {
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
		}

		return sql.toString();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#equal(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> equal(final String col, final Object value) {
		context().param(col, new Equal(col, value));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notEqual(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> notEqual(final String col, final Object value) {
		context().param(col, new NotEqual(col, value));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#greaterThan(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> greaterThan(final String col, final Object value) {
		context().param(col, new GreaterThan(col, value));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#lessThan(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> lessThan(final String col, final Object value) {
		context().param(col, new LessThan(col, value));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#greaterEqual(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> greaterEqual(final String col, final Object value) {
		context().param(col, new GreaterEqual(col, value));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#lessEqual(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> lessEqual(final String col, final Object value) {
		context().param(col, new LessEqual(col, value));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#in(java.lang.String, java.lang.Object[])
	 */
	@Override
	public SqlEntityQuery<E> in(final String col, final Object... values) {
		context().param(col, new In(col, values));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#in(java.lang.String, java.lang.Iterable)
	 */
	@Override
	public SqlEntityQuery<E> in(final String col, final Iterable<?> valueList) {
		context().param(col, new In(col, valueList));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notIn(java.lang.String, java.lang.Object[])
	 */
	@Override
	public SqlEntityQuery<E> notIn(final String col, final Object... values) {
		context().param(col, new NotIn(col, values));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notIn(java.lang.String, java.lang.Iterable)
	 */
	@Override
	public SqlEntityQuery<E> notIn(final String col, final Iterable<?> valueList) {
		context().param(col, new NotIn(col, valueList));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#like(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> like(final String col, final CharSequence searchValue) {
		context().param(col, new Like(col, searchValue));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#startsWith(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> startsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new Like(col, escaped, true));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#endsWith(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> endsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new Like(col, true, escaped));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#contains(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> contains(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new Like(col, true, escaped, true));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notLike(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> notLike(final String col, final CharSequence searchValue) {
		context().param(col, new NotLike(col, searchValue));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notStartsWith(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> notStartsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new NotLike(col, escaped, true));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notEndsWith(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> notEndsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new NotLike(col, true, escaped));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notContains(java.lang.String, java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> notContains(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new NotLike(col, true, escaped, true));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#between(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> between(final String col, final Object fromValue, final Object toValue) {
		context().param(col, new Between(col, fromValue, toValue));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#isNull(java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> isNull(final String col) {
		context().param(col, new IsNull(col));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#isNotNull(java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> isNotNull(final String col) {
		context().param(col, new IsNotNull(col));
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#where(java.lang.CharSequence)
	 */
	@Override
	public SqlEntityQuery<E> where(final CharSequence rawString) {
		this.rawString = rawString;
		return this;
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
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#desc(java.lang.String, jp.co.future.uroborosql.fluent.SqlEntityQuery.Nulls)
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
	 * 条件指定用のラップクラス
	 */
	public static abstract class Operator {
		protected final String col;

		/**
		 * Constructor
		 *
		 * @param col バインドしたカラム名
		 */
		public Operator(final String col) {
			this.col = col;
		}

		/**
		 * バインドしたカラム名の取得
		 *
		 * @return カラム名
		 */
		public String getCol() {
			return col;
		}

		/**
		 * オペレータを取得する
		 *
		 * @return オペレータ
		 */
		public abstract String getOperator();

		/**
		 * 評価式に変換する
		 *
		 * @return 評価式
		 */
		public String toConditionString() {
			return " " + getOperator();
		}

		/**
		 * バインド変数文字列を生成する
		 *
		 * @param keyNames バインド変数名。複数指定した場合、"."区切りで結合する
		 * @return バインド変数文字列
		 */
		protected String wrap(final String... keyNames) {
			return "/*" + String.join(".", keyNames) + "*/";
		}
	}

	/**
	 * 値を1つもつオペレータ
	 */
	public static abstract class SingleOperator extends Operator {
		protected final Object value;

		/**
		 * Constructor
		 * @param col bind column name
		 * @param value 値
		 */
		public SingleOperator(final String col, final Object value) {
			super(col);
			this.value = value;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getCol()
		 */
		@Override
		public String getCol() {
			return col;
		}

		/**
		 * 値の取得
		 * @return 値
		 */
		public Object getValue() {
			return value;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#toConditionString()
		 */
		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "value");
		}
	}

	/**
	 * Listを持つオペレータ
	 */
	public static abstract class ListOperator extends Operator {
		protected final Iterable<?> valueList;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param valueList 値のリスト
		 */
		public ListOperator(final String col, final Iterable<?> valueList) {
			super(col);
			this.valueList = valueList;
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param values 値の配列
		 */
		public ListOperator(final String col, final Object... values) {
			super(col);
			valueList = Arrays.asList(values);
		}

		/**
		 * 値のリストを取得する
		 *
		 * @return 値のリスト
		 */
		public Iterable<?> getValueList() {
			return valueList;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#toConditionString()
		 */
		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "valueList") + "()";
		}
	}

	/**
	 * Equal Operator
	 */
	public static class Equal extends SingleOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public Equal(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "=";
		}
	}

	/**
	 * NotEqual Operator
	 */
	public static class NotEqual extends SingleOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public NotEqual(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "!=";
		}
	}

	/**
	 * Greater Than Operator
	 */
	public static class GreaterThan extends SingleOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public GreaterThan(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return ">";
		}
	}

	/**
	 * Less Than Operator
	 */
	public static class LessThan extends SingleOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public LessThan(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "<";
		}
	}

	/**
	 * Greater Equal Operator
	 */
	public static class GreaterEqual extends SingleOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public GreaterEqual(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return ">=";
		}
	}

	/**
	 * Less Than Operator
	 */
	public static class LessEqual extends SingleOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public LessEqual(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "<=";
		}
	}

	/**
	 * In Operator
	 */
	public static class In extends ListOperator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param valueList 値リスト
		 */
		public In(final String col, final Iterable<?> valueList) {
			super(col, valueList);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param values 値の配列
		 */
		public In(final String col, final Object... values) {
			super(col, values);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "IN";
		}
	}

	/**
	 * Not In Operator
	 */
	public static class NotIn extends In {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param valueList 値リスト
		 */
		public NotIn(final String col, final Iterable<?> valueList) {
			super(col, valueList);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param values 値の配列
		 */
		public NotIn(final String col, final Object... values) {
			super(col, values);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.In#getOperator()
		 */
		@Override
		public String getOperator() {
			return "NOT IN";
		}
	}

	/**
	 * Like Operator
	 */
	public static class Like extends SingleOperator {
		protected boolean prefix;
		protected boolean suffix;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public Like(final String col, final Object value) {
			this(col, true, value, true);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 */
		public Like(final String col, final boolean prefix, final Object value) {
			this(col, prefix, value, false);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 * @param prefix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 */
		public Like(final String col, final Object value, final boolean suffix) {
			this(col, false, value, suffix);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 * @param prefix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 */
		public Like(final String col, final boolean prefix, final Object value, final boolean suffix) {
			super(col, value);
			this.prefix = prefix;
			this.suffix = suffix;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.SingleOperator#getValue()
		 */
		@Override
		public Object getValue() {
			String searchValue = Objects.toString(super.getValue(), "");
			if (prefix) {
				searchValue = "%" + searchValue;
			}
			if (suffix) {
				searchValue = searchValue + "%";
			}
			return searchValue;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "LIKE";
		}
	}

	/**
	 * Not Like Operator
	 */
	public static class NotLike extends Like {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public NotLike(final String col, final Object value) {
			super(col, value);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 */
		public NotLike(final String col, final boolean prefix, final Object value) {
			super(col, prefix, value);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 * @param prefix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 */
		public NotLike(final String col, final Object value, final boolean suffix) {
			super(col, value, suffix);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 * @param prefix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 */
		public NotLike(final String col, final boolean prefix, final Object value, final boolean suffix) {
			super(col, prefix, value, suffix);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Like#getOperator()
		 */
		@Override
		public String getOperator() {
			return "NOT LIKE";
		}
	}

	/**
	 * Between Operator
	 */
	public static class Between extends Operator {
		protected final Object from;
		protected final Object to;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param from from value
		 * @param to to value
		 */
		public Between(final String col, final Object from, final Object to) {
			super(col);
			this.from = from;
			this.to = to;
		}

		/**
		 * From値の取得
		 *
		 * @return From値
		 */
		public Object getFrom() {
			return from;
		}

		/**
		 * To値の取得
		 *
		 * @return To値
		 */
		public Object getTo() {
			return to;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#toConditionString()
		 */
		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "from") + " AND " + wrap(getCol(), "to");
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "BETWEEN";
		}
	}

	/**
	 * IS NULL Operator
	 */
	public static class IsNull extends Operator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 */
		public IsNull(final String col) {
			super(col);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "IS NULL";
		}
	}

	/**
	 * IS NOT NULL Operator
	 */
	public static class IsNotNull extends Operator {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 */
		public IsNotNull(final String col) {
			super(col);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.SqlEntityQueryImpl.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "IS NOT NULL";
		}
	}

	private static class SortOrder {
		private final String col;
		private final Order order;
		private final Nulls nulls;

		SortOrder(final String col, final Order order) {
			this(col, order, Nulls.LAST);
		}

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
