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
import java.util.Map;
import java.util.Objects;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.fluent.ExtractionCondition;
import jp.co.future.uroborosql.fluent.SqlFluent;
import jp.co.future.uroborosql.mapping.TableMetadata;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * 抽出条件の生成を担当するクラス
 *
 * @param <T> SqlFluent型を継承するSqlEntity実装型
 * @author H.Sugimoto
 */
abstract class AbstractExtractionCondition<T extends SqlFluent<T>> extends AbstractSqlFluent<T>
		implements ExtractionCondition<T> {

	/** 抽出条件のパラメータ名に付与するプレフィックス */
	protected static final String PREFIX = "_";

	/** テーブルメタデータ */
	protected final TableMetadata tableMetadata;

	/** where句文字列 */
	protected final List<CharSequence> rawStrings;

	/** オペレータを使用したかどうか */
	protected boolean useOperator = false;

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
		context.setSchema(tableMetadata.getSchema());
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

			if (this.useOperator) {
				Parameter param = context().getParam(PREFIX + camelColName);
				if (param != null) {
					if (param.getValue() instanceof Operator) {
						Operator ope = (Operator) param.getValue();
						where.append("\t").append("AND ");
						if (ope.useColumnIdentifier()) {
							where.append(col.getColumnIdentifier());
						}
						where.append(ope.toConditionString()).append(System.lineSeparator());
					}
				}
			} else {
				Parameter param = context().getParam(camelColName);
				if (param != null) {
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
	public <V> T equal(final String col, final V value) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new Equal<>(col, value));
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
	public <V> T notEqual(final String col, final V value) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new NotEqual<>(col, value));
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
	public <V> T greaterThan(final String col, final V value) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new GreaterThan<>(col, value));
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
	public <V> T lessThan(final String col, final V value) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new LessThan<>(col, value));
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
	public <V> T greaterEqual(final String col, final V value) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new GreaterEqual<>(col, value));
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
	public <V> T lessEqual(final String col, final V value) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new LessEqual<>(col, value));
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
	public <V> T in(final String col, final V... values) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new In<>(col, values));
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
	public <V> T in(final String col, final Iterable<V> valueList) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new In<>(col, valueList));
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
	public <V> T notIn(final String col, final V... values) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new NotIn<>(col, values));
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
	public <V> T notIn(final String col, final Iterable<V> valueList) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new NotIn<>(col, valueList));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new Like(col, searchValue, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		String escaped = dialect.escapeLikePattern(searchValue);
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new Like(col, escaped, true, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		String escaped = dialect.escapeLikePattern(searchValue);
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new Like(col, true, escaped, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		String escaped = dialect.escapeLikePattern(searchValue);
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new Like(col, true, escaped, true, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new NotLike(col, searchValue, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		String escaped = dialect.escapeLikePattern(searchValue);
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new NotLike(col, escaped, true, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		String escaped = dialect.escapeLikePattern(searchValue);
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new NotLike(col, true, escaped, dialect.getEscapeChar()));
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
		Dialect dialect = agent().getSqlConfig().getDialect();
		String escaped = dialect.escapeLikePattern(searchValue);
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col),
				new NotLike(col, true, escaped, true, dialect.getEscapeChar()));
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
	public <V> T between(final String col, final V fromValue, final V toValue) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new Between<>(col, fromValue, toValue));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notBetween(java.lang.String, java.lang.Object, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T notBetween(final String col, final V fromValue, final V toValue) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new NotBetween<>(col, fromValue, toValue));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#betweenColumns(java.lang.Object, java.lang.String, java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T betweenColumns(final V value, final String fromCol, final String toCol) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(fromCol),
				new BetweenColumns<>(value, fromCol, toCol, tableMetadata));
		this.useOperator = true;
		return (T) this;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#notBetweenColumns(java.lang.Object, java.lang.String, java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <V> T notBetweenColumns(final V value, final String fromCol, final String toCol) {
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(fromCol),
				new NotBetweenColumns<>(value, fromCol, toCol, tableMetadata));
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
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new IsNull(col));
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
		context().param(PREFIX + CaseFormat.CAMEL_CASE.convert(col), new IsNotNull(col));
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
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#where(java.lang.CharSequence, java.lang.String, java.lang.Object)
	 */
	@Override
	public <V> T where(final CharSequence rawString, final String paramName, final V value) {
		this.param(paramName, value);
		return this.where(rawString);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.fluent.ExtractionCondition#where(java.lang.CharSequence, java.util.Map)
	 */
	@Override
	public T where(final CharSequence rawString, final Map<String, Object> paramMap) {
		this.paramMap(paramMap);
		return this.where(rawString);
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
			this.col = CaseFormat.CAMEL_CASE.convert(col);
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
		 * 条件生成時にカラム識別子を使用するかどうか
		 *
		 * @return カラム識別子を使用する場合<code>true</code>
		 */
		public boolean useColumnIdentifier() {
			return true;
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
			return "/*" + PREFIX + String.join(".", keyNames) + "*/";
		}
	}

	/**
	 * 値を1つもつオペレータ
	 */
	public static abstract class SingleOperator<V> extends Operator {
		protected final V value;

		/**
		 * Constructor
		 * @param col bind column name
		 * @param value 値
		 */
		public SingleOperator(final String col, final V value) {
			super(col);
			this.value = value;
		}

		/**
		 * 値の取得
		 * @return 値
		 */
		public V getValue() {
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
	public static abstract class ListOperator<V> extends Operator {
		protected final Iterable<V> valueList;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param valueList 値のリスト
		 */
		public ListOperator(final String col, final Iterable<V> valueList) {
			super(col);
			this.valueList = valueList;
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param values 値の配列
		 */
		@SafeVarargs
		public ListOperator(final String col, final V... values) {
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
	public static class Equal<V> extends SingleOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public Equal(final String col, final V value) {
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
	public static class NotEqual<V> extends SingleOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public NotEqual(final String col, final V value) {
			super(col, value);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#getOperator()
		 */
		@Override
		public String getOperator() {
			return "!=";
		}
	}

	/**
	 * Greater Than Operator
	 */
	public static class GreaterThan<V> extends SingleOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public GreaterThan(final String col, final V value) {
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
	public static class LessThan<V> extends SingleOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public LessThan(final String col, final V value) {
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
	public static class GreaterEqual<V> extends SingleOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public GreaterEqual(final String col, final V value) {
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
	public static class LessEqual<V> extends SingleOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 */
		public LessEqual(final String col, final V value) {
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
	public static class In<V> extends ListOperator<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param valueList 値リスト
		 */
		public In(final String col, final Iterable<V> valueList) {
			super(col, valueList);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param values 値の配列
		 */
		@SafeVarargs
		public In(final String col, final V... values) {
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
	public static class NotIn<V> extends In<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param valueList 値リスト
		 */
		public NotIn(final String col, final Iterable<V> valueList) {
			super(col, valueList);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param values 値の配列
		 */
		@SafeVarargs
		public NotIn(final String col, final V... values) {
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
	public static class Like extends SingleOperator<CharSequence> {
		protected boolean prefix;
		protected boolean suffix;
		protected char escapeChar;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 * @param escapeChar エスケープ文字
		 */
		public Like(final String col, final CharSequence value, final char escapeChar) {
			this(col, true, value, true, escapeChar);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 * @param escapeChar エスケープ文字
		 */
		public Like(final String col, final boolean prefix, final CharSequence value, final char escapeChar) {
			this(col, prefix, value, false, escapeChar);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param escapeChar エスケープ文字
		 */
		public Like(final String col, final CharSequence value, final boolean suffix, final char escapeChar) {
			this(col, false, value, suffix, escapeChar);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param escapeChar エスケープ文字
		 */
		public Like(final String col, final boolean prefix, final CharSequence value, final boolean suffix,
				final char escapeChar) {
			super(col, value);
			this.prefix = prefix;
			this.suffix = suffix;
			this.escapeChar = escapeChar;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.SingleOperator#getValue()
		 */
		@Override
		public CharSequence getValue() {
			CharSequence searchValue = Objects.toString(super.getValue(), "");
			if (prefix) {
				searchValue = "%" + searchValue;
			}
			if (suffix) {
				searchValue = searchValue + "%";
			}
			return searchValue;
		}

		@Override
		public String toConditionString() {
			return super.toConditionString() + " ESCAPE '" + escapeChar + "'";
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
		 * @param escapeChar エスケープ文字
		 */
		public NotLike(final String col, final CharSequence value, final char escapeChar) {
			super(col, value, escapeChar);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 * @param escapeChar エスケープ文字
		 */
		public NotLike(final String col, final boolean prefix, final CharSequence value, final char escapeChar) {
			super(col, prefix, value, escapeChar);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param value 値
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param escapeChar エスケープ文字
		 */
		public NotLike(final String col, final CharSequence value, final boolean suffix, final char escapeChar) {
			super(col, value, suffix, escapeChar);
		}

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param prefix 前にワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param value 値
		 * @param suffix 後ろにワイルドカードを挿入するかどうか。trueの場合%を追加
		 * @param escapeChar エスケープ文字
		 */
		public NotLike(final String col, final boolean prefix, final CharSequence value, final boolean suffix,
				final char escapeChar) {
			super(col, prefix, value, suffix, escapeChar);
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
	public static class Between<V> extends Operator {
		protected final V from;
		protected final V to;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param from from value
		 * @param to to value
		 */
		public Between(final String col, final V from, final V to) {
			super(col);
			this.from = from;
			this.to = to;
		}

		/**
		 * From値の取得
		 *
		 * @return From値
		 */
		public V getFrom() {
			return from;
		}

		/**
		 * To値の取得
		 *
		 * @return To値
		 */
		public V getTo() {
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
	 * NotBetween Operator
	 */
	public static class NotBetween<V> extends Between<V> {
		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param from from value
		 * @param to to value
		 */
		public NotBetween(final String col, final V from, final V to) {
			super(col, from, to);
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Between#getOperator()
		 */
		@Override
		public String getOperator() {
			return "NOT BETWEEN";
		}
	}

	/**
	 * BetweenColumns Operator
	 */
	public static class BetweenColumns<V> extends Operator {
		protected final String toCol;
		protected final V value;
		protected final TableMetadata metadata;

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param from from value
		 * @param to to value
		 */
		public BetweenColumns(final V value, final String fromCol, final String toCol, final TableMetadata metadata) {
			super(fromCol);
			this.toCol = CaseFormat.CAMEL_CASE.convert(toCol);
			this.value = value;
			this.metadata = metadata;
		}

		/**
		 * 値の取得
		 *
		 * @return To値
		 */
		public V getValue() {
			return value;
		}

		/**
		 * FromColの取得
		 *
		 * @return FromCol
		 */
		public String getFromCol() {
			return super.getCol();
		}

		/**
		 * toColの取得
		 *
		 * @return toCol
		 */
		public String getToCol() {
			return this.toCol;
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#useColumnIdentifier()
		 */
		@Override
		public boolean useColumnIdentifier() {
			return false;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.Operator#toConditionString()
		 */
		@Override
		public String toConditionString() {
			TableMetadata.Column fromColumn = this.metadata.getColumn(getFromCol());
			TableMetadata.Column toColumn = this.metadata.getColumn(getToCol());

			return wrap(getCol(), "value") + " " + getOperator() + " "
					+ fromColumn.getColumnIdentifier() + " AND "
					+ toColumn.getColumnIdentifier();
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
	 * NotBetweenColumns Operator
	 */
	public static class NotBetweenColumns<V> extends BetweenColumns<V> {

		/**
		 * Constructor
		 *
		 * @param col bind column name
		 * @param from from value
		 * @param to to value
		 */
		public NotBetweenColumns(final V value, final String fromCol, final String toCol,
				final TableMetadata metadata) {
			super(value, fromCol, toCol, metadata);
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.AbstractExtractionCondition.BetweenColumns#getOperator()
		 */
		@Override
		public String getOperator() {
			return "NOT BETWEEN";
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
