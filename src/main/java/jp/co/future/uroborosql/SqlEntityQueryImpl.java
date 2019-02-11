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
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException;
import jp.co.future.uroborosql.exception.EntitySqlRuntimeException.EntityProcKind;
import jp.co.future.uroborosql.fluent.SqlEntityQuery;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.mapping.TableMetadata;
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
	private String rawString;
	private List<String> orderByColumns;
	private boolean ascendingOrder;

	/**
	 * コンストラクタ
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
		this.orderByColumns = null;
		this.ascendingOrder = true;
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
			context().setSql(context().getSql() + whereAndOrderClause());
			return this.entityHandler.doSelect(agent(), context(), this.entityType);
		} catch (final SQLException e) {
			throw new EntitySqlRuntimeException(EntityProcKind.SELECT, e);
		}

	}

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
		if (this.orderByColumns == null || this.orderByColumns.isEmpty()) {
			// ソート条件の指定がない場合は主キーでソートする
			keys = (List<TableMetadata.Column>) this.tableMetadata.getKeyColumns();
		} else {
			// ソート条件の指定がある場合は指定されたカラムでソートする
			keys = new ArrayList<>();
			for (String col : orderByColumns) {
				String snakeCol = CaseFormat.UPPER_SNAKE_CASE.convert(col);
				for (TableMetadata.Column metaCol : columns) {
					if (snakeCol.equalsIgnoreCase(metaCol.getColumnName())) {
						keys.add(metaCol);
						break;
					}
				}
			}
		}

		if (!keys.isEmpty()) {
			sql.append("ORDER BY").append(System.lineSeparator());
			firstFlag = true;
			for (final TableMetadata.Column col : keys) {
				sql.append("\t");
				if (firstFlag) {
					sql.append("  ");
					firstFlag = false;
				} else {
					sql.append(", ");
				}
				sql.append(col.getColumnIdentifier()).append(System.lineSeparator());
			}
			sql.append(this.ascendingOrder ? "ASC" : "DESC").append(System.lineSeparator());
		}

		return sql.toString();
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#equal(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> equal(final String col, final Object value) {
		context().param(col, new Equal(col, value));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notEqual(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> notEqual(final String col, final Object value) {
		context().param(col, new NotEqual(col, value));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#greaterThan(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> greaterThan(final String col, final Object value) {
		context().param(col, new GreaterThan(col, value));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#lessThan(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> lessThan(final String col, final Object value) {
		context().param(col, new LessThan(col, value));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#greaterEqual(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> greaterEqual(final String col, final Object value) {
		context().param(col, new GreaterEqual(col, value));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#lessEqual(java.lang.String, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> lessEqual(final String col, final Object value) {
		context().param(col, new LessEqual(col, value));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#in(java.lang.String, java.lang.Object[])
	 */
	@Override
	public SqlEntityQuery<E> in(final String col, final Object... values) {
		context().param(col, new In(col, values));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#in(java.lang.String, java.util.List)
	 */
	@Override
	public SqlEntityQuery<E> in(final String col, final List<Object> valueList) {
		context().param(col, new In(col, valueList));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notIn(java.lang.String, java.lang.Object[])
	 */
	@Override
	public SqlEntityQuery<E> notIn(final String col, final Object... values) {
		context().param(col, new NotIn(col, values));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notIn(java.lang.String, java.util.List)
	 */
	@Override
	public SqlEntityQuery<E> notIn(final String col, final List<Object> valueList) {
		context().param(col, new NotIn(col, valueList));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#like(java.lang.String, java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> like(final String col, final String searchValue) {
		context().param(col, new Like(col, searchValue));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#like(java.lang.String, boolean, java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> like(final String col, final boolean prefix, final String searchValue) {
		context().param(col, new Like(col, prefix, searchValue));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#like(java.lang.String, java.lang.String, boolean)
	 */
	@Override
	public SqlEntityQuery<E> like(final String col, final String searchValue, final boolean suffix) {
		context().param(col, new Like(col, searchValue, suffix));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#like(java.lang.String, boolean, java.lang.String, boolean)
	 */
	@Override
	public SqlEntityQuery<E> like(final String col, final boolean prefix, final String searchValue,
			final boolean suffix) {
		context().param(col, new Like(col, prefix, searchValue, suffix));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notLike(java.lang.String, java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> notLike(final String col, final String searchValue) {
		context().param(col, new NotLike(col, searchValue));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notLike(java.lang.String, boolean, java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> notLike(final String col, final boolean prefix, final String searchValue) {
		context().param(col, new NotLike(col, prefix, searchValue));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notLike(java.lang.String, java.lang.String, boolean)
	 */
	@Override
	public SqlEntityQuery<E> notLike(final String col, final String searchValue, final boolean suffix) {
		context().param(col, new NotLike(col, searchValue, suffix));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#notLike(java.lang.String, boolean, java.lang.String, boolean)
	 */
	@Override
	public SqlEntityQuery<E> notLike(final String col, final boolean prefix, final String searchValue,
			final boolean suffix) {
		context().param(col, new NotLike(col, prefix, searchValue, suffix));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#between(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	@Override
	public SqlEntityQuery<E> between(final String col, final Object fromValue, final Object toValue) {
		context().param(col, new Between(col, fromValue, toValue));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#isNull(java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> isNull(final String col) {
		context().param(col, new IsNull(col));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#isNotNull(java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> isNotNull(final String col) {
		context().param(col, new IsNotNull(col));
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#where(java.lang.String)
	 */
	@Override
	public SqlEntityQuery<E> where(final String rawString) {
		this.rawString = rawString;
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#orderByAsc(java.lang.String[])
	 */
	@Override
	public SqlEntityQuery<E> orderByAsc(final String... cols) {
		this.orderByColumns = Arrays.asList(cols);
		this.ascendingOrder = true;
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#orderByDesc(java.lang.String[])
	 */
	@Override
	public SqlEntityQuery<E> orderByDesc(final String... cols) {
		this.orderByColumns = Arrays.asList(cols);
		this.ascendingOrder = false;
		return this;
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#limit(long)
	 */
	@Override
	public SqlEntityQuery<E> limit(final long limit) {
		// TODO 未実装
		throw new UnsupportedOperationException();
	}

	/**
	 * @see jp.co.future.uroborosql.fluent.SqlEntityQuery#offset(long)
	 */
	@Override
	public SqlEntityQuery<E> offset(final long offset) {
		// TODO 未実装
		throw new UnsupportedOperationException();
	}

	public static abstract class Operator {
		protected final String col;

		public Operator(final String col) {
			this.col = col;
		}

		public String getCol() {
			return col;
		}

		public String getOperator() {
			throw new UnsupportedOperationException();
		}

		public String toConditionString() {
			return " " + getOperator();
		}

		protected String wrap(final String... keyNames) {
			return "/*" + String.join(".", keyNames) + "*/";
		}
	}

	public static abstract class SingleOperator extends Operator {
		protected final Object value;

		public SingleOperator(final String col, final Object value) {
			super(col);
			this.value = value;
		}

		@Override
		public String getCol() {
			return col;
		}

		public Object getValue() {
			return value;
		}

		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "value");
		}
	}

	public static abstract class ListOperator extends Operator {
		protected final List<?> valueList;

		public ListOperator(final String col, final List<?> valueList) {
			super(col);
			this.valueList = valueList;
		}

		public ListOperator(final String col, final Object... values) {
			super(col);
			valueList = Arrays.asList(values);
		}

		public List<?> getValueList() {
			return valueList;
		}

		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "valueList") + "()";
		}
	}

	public static class Equal extends SingleOperator {
		public Equal(final String col, final Object value) {
			super(col, value);
		}

		@Override
		public String getOperator() {
			return "=";
		}
	}

	public static class NotEqual extends SingleOperator {
		public NotEqual(final String col, final Object value) {
			super(col, value);
		}

		@Override
		public String getOperator() {
			return "!=";
		}
	}

	public static class GreaterThan extends SingleOperator {
		public GreaterThan(final String col, final Object value) {
			super(col, value);
		}

		@Override
		public String getOperator() {
			return ">";
		}
	}

	public static class LessThan extends SingleOperator {
		public LessThan(final String col, final Object value) {
			super(col, value);
		}

		@Override
		public String getOperator() {
			return "<";
		}
	}

	public static class GreaterEqual extends SingleOperator {
		public GreaterEqual(final String col, final Object value) {
			super(col, value);
		}

		@Override
		public String getOperator() {
			return ">=";
		}
	}

	public static class LessEqual extends SingleOperator {
		public LessEqual(final String col, final Object value) {
			super(col, value);
		}

		@Override
		public String getOperator() {
			return "<=";
		}
	}

	public static class In extends ListOperator {
		public In(final String col, final List<?> valueList) {
			super(col, valueList);
		}

		public In(final String col, final Object... values) {
			super(col, values);
		}

		@Override
		public String getOperator() {
			return "IN";
		}
	}

	public static class NotIn extends In {
		public NotIn(final String col, final List<?> valueList) {
			super(col, valueList);
		}

		public NotIn(final String col, final Object... values) {
			super(col, values);
		}

		@Override
		public String getOperator() {
			return "NOT IN";
		}
	}

	public static class Like extends SingleOperator {
		protected boolean prefix;
		protected boolean suffix;

		public Like(final String col, final Object value) {
			this(col, true, value, true);
		}

		public Like(final String col, final boolean prefix, final Object value) {
			this(col, prefix, value, false);
		}

		public Like(final String col, final Object value, final boolean suffix) {
			this(col, false, value, suffix);
		}

		public Like(final String col, final boolean prefix, final Object value, final boolean suffix) {
			super(col, value);
			this.prefix = prefix;
			this.suffix = suffix;
		}

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

		@Override
		public String getOperator() {
			return "LIKE";
		}
	}

	public static class NotLike extends Like {
		public NotLike(final String col, final Object value) {
			super(col, value);
		}

		public NotLike(final String col, final boolean prefix, final Object value) {
			super(col, prefix, value);
		}

		public NotLike(final String col, final Object value, final boolean suffix) {
			super(col, value, suffix);
		}

		public NotLike(final String col, final boolean prefix, final Object value, final boolean suffix) {
			super(col, prefix, value, suffix);
		}

		@Override
		public String getOperator() {
			return "NOT LIKE";
		}
	}

	public static class Between extends Operator {
		protected final Object from;
		protected final Object to;

		public Between(final String col, final Object from, final Object to) {
			super(col);
			this.from = from;
			this.to = to;
		}

		public Object getFrom() {
			return from;
		}

		public Object getTo() {
			return to;
		}

		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "from") + " AND " + wrap(getCol(), "to");
		}

		@Override
		public String getOperator() {
			return "BETWEEN";
		}
	}

	public static class IsNull extends Operator {
		public IsNull(final String col) {
			super(col);
		}

		@Override
		public String getOperator() {
			return "IS NULL";
		}
	}

	public static class IsNotNull extends Operator {
		public IsNotNull(final String col) {
			super(col);
		}

		@Override
		public String getOperator() {
			return "IS NOT NULL";
		}
	}

}
