/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.fluent.ExtractionCondition;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * 抽出条件の生成を担当するクラス
 *
 * @param <T> SqlFluent型を継承するSqlEntity実装型
 * @author H.Sugimoto
 */
abstract class AbstractExtractionCondition<T extends SqlFluent<T>> extends AbstractSqlFluent<T>
		implements ExtractionCondition<T> {

	protected final TableMetadata tableMetadata;
	private final List<CharSequence> rawStrings;
	private boolean useOperator = false;

	/**
	 * Constructor
	 *
	 * @param agent SqlAgent
	 * @param tableMetadata TableMetadata
	 * @param context SqlContext
	 */
	AbstractExtractionCondition(final SqlAgent agent, final TableMetadata tableMetadata, final SqlContext context) {
		super(agent, context);
		this.tableMetadata = tableMetadata;
		this.rawStrings = new ArrayList<>();
		this.useOperator = false;
	}

	/**
	 * WHERE句を生成する
	 *
	 * @return WHERE句の文字列
	 */
	protected String getWhereClause() {
		StringBuilder where = new StringBuilder();
		for (final TableMetadata.Column col : this.tableMetadata.getColumns()) {
			final String camelColName = col.getCamelColumnName();

			Parameter param = context().getParam(camelColName);
			if (param != null) {
				if (param.getValue() instanceof Operator) {
					Operator ope = (Operator) param.getValue();
					where.append("\t").append("AND ").append(col.getColumnIdentifier())
							.append(ope.toConditionString()).append(System.lineSeparator());
				} else if (!this.useOperator) {
					where.append("\t").append("AND ").append(col.getColumnIdentifier())
							.append(" = ").append("/*").append(camelColName).append("*/''")
							.append(System.lineSeparator());
				}
			}
		}
		if (!this.rawStrings.isEmpty()) {
			for (CharSequence raw : rawStrings) {
				if (where.length() > 0) {
					where.append("\t").append("AND ( ").append(raw).append(" )").append(System.lineSeparator());
				} else {
					where.append("\t").append("( ").append(raw).append(" )").append(System.lineSeparator());
				}
			}
		}

		if (where.length() > 0) {
			return new StringBuilder().append("WHERE").append(System.lineSeparator()).append(where.toString())
					.toString();
		} else {
			return "";
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#equal(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T equal(final String col, final Object value) {
		context().param(col, new Equal(col, value));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notEqual(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notEqual(final String col, final Object value) {
		context().param(col, new NotEqual(col, value));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#greaterThan(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T greaterThan(final String col, final Object value) {
		context().param(col, new GreaterThan(col, value));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#lessThan(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T lessThan(final String col, final Object value) {
		context().param(col, new LessThan(col, value));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#greaterEqual(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T greaterEqual(final String col, final Object value) {
		context().param(col, new GreaterEqual(col, value));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#lessEqual(java.lang.String, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T lessEqual(final String col, final Object value) {
		context().param(col, new LessEqual(col, value));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#in(java.lang.String, java.lang.Object[])
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T in(final String col, final Object... values) {
		context().param(col, new In(col, values));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#in(java.lang.String, java.lang.Iterable)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T in(final String col, final Iterable<?> valueList) {
		context().param(col, new In(col, valueList));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notIn(java.lang.String, java.lang.Object[])
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notIn(final String col, final Object... values) {
		context().param(col, new NotIn(col, values));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notIn(java.lang.String, java.lang.Iterable)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notIn(final String col, final Iterable<?> valueList) {
		context().param(col, new NotIn(col, valueList));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#like(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T like(final String col, final CharSequence searchValue) {
		context().param(col, new Like(col, searchValue));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#startsWith(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T startsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new Like(col, escaped, true));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#endsWith(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T endsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new Like(col, true, escaped));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#contains(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T contains(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new Like(col, true, escaped, true));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notLike(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notLike(final String col, final CharSequence searchValue) {
		context().param(col, new NotLike(col, searchValue));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notStartsWith(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notStartsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new NotLike(col, escaped, true));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notEndsWith(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notEndsWith(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new NotLike(col, true, escaped));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notContains(java.lang.String, java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T notContains(final String col, final CharSequence searchValue) {
		String escaped = agent().getSqlConfig().getDialect().escapeLikePattern(searchValue);
		context().param(col, new NotLike(col, true, escaped, true));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#between(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T between(final String col, final Object fromValue, final Object toValue) {
		context().param(col, new Between(col, fromValue, toValue));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#isNull(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T isNull(final String col) {
		context().param(col, new IsNull(col));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#isNotNull(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T isNotNull(final String col) {
		context().param(col, new IsNotNull(col));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#where(java.lang.CharSequence)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public T where(final CharSequence rawString) {
		this.rawStrings.add(rawString);
		this.useOperator = true;
		return (T) this;
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getCol()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#toConditionString()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#toConditionString()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "<>";
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.In#getOperator()
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
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
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
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 */
		public Like(final String col, final boolean prefix, final Object value, final boolean suffix) {
			super(col, value);
			this.prefix = prefix;
			this.suffix = suffix;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.SingleOperator#getValue()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
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
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 */
		public NotLike(final String col, final boolean prefix, final Object value, final boolean suffix) {
			super(col, prefix, value, suffix);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Like#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#toConditionString()
		 */
		@Override
		public String toConditionString() {
			return " " + getOperator() + " " + wrap(getCol(), "from") + " AND " + wrap(getCol(), "to");
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
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
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "IS NOT NULL";
		}
	}
}
