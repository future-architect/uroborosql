/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.math.BigDecimal;
import java.sql.JDBCType;
import java.sql.Ref;
import java.sql.SQLType;
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.enums.ForUpdateType;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.utils.StringFunction;

/**
 * Dialectの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractDialect implements Dialect {
	private static final char[] DEFAULT_WILDCARDS = { '%', '_' };

	/** SQLTypeの値(int)とJavaTypeのマッピング */
	protected static final Map<Integer, JavaType> DEFAULT_TYPE_MAP;

	/** like検索時のエスケープ文字 */
	private final char escapeChar;
	/** like検索時のワイルドカード文字配列 */
	private final char[] wildcards;
	/** エスケープするパターン */
	private final Pattern escapePattern;

	private final StringFunction expressionFunction;

	protected final Map<Integer, JavaType> typeMap;

	static {
		// initialize DEFAULT_TYPE_MAP
		DEFAULT_TYPE_MAP = new HashMap<>();
		DEFAULT_TYPE_MAP.put(JDBCType.CHAR.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.VARCHAR.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.LONGVARCHAR.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.NCHAR.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.NVARCHAR.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.LONGNVARCHAR.getVendorTypeNumber(), JavaType.of(String.class));

		DEFAULT_TYPE_MAP.put(JDBCType.NUMERIC.getVendorTypeNumber(), JavaType.of(BigDecimal.class));
		DEFAULT_TYPE_MAP.put(JDBCType.DECIMAL.getVendorTypeNumber(), JavaType.of(BigDecimal.class));

		DEFAULT_TYPE_MAP.put(JDBCType.BIT.getVendorTypeNumber(), JavaType.of(Boolean.class));
		DEFAULT_TYPE_MAP.put(JDBCType.BOOLEAN.getVendorTypeNumber(), JavaType.of(Boolean.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TINYINT.getVendorTypeNumber(), JavaType.of(byte.class));
		DEFAULT_TYPE_MAP.put(JDBCType.SMALLINT.getVendorTypeNumber(), JavaType.of(Short.class));
		DEFAULT_TYPE_MAP.put(JDBCType.INTEGER.getVendorTypeNumber(), JavaType.of(Integer.class));
		DEFAULT_TYPE_MAP.put(JDBCType.BIGINT.getVendorTypeNumber(), JavaType.of(Long.class));
		DEFAULT_TYPE_MAP.put(JDBCType.REAL.getVendorTypeNumber(), JavaType.of(Float.class));
		DEFAULT_TYPE_MAP.put(JDBCType.FLOAT.getVendorTypeNumber(), JavaType.of(Double.class));
		DEFAULT_TYPE_MAP.put(JDBCType.DOUBLE.getVendorTypeNumber(), JavaType.of(Double.class));

		DEFAULT_TYPE_MAP.put(JDBCType.BINARY.getVendorTypeNumber(), JavaType.of(byte[].class));
		DEFAULT_TYPE_MAP.put(JDBCType.VARBINARY.getVendorTypeNumber(), JavaType.of(byte[].class));
		DEFAULT_TYPE_MAP.put(JDBCType.LONGVARBINARY.getVendorTypeNumber(), JavaType.of(byte[].class));

		DEFAULT_TYPE_MAP.put(JDBCType.DATE.getVendorTypeNumber(), JavaType.of(ZonedDateTime.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIME.getVendorTypeNumber(), JavaType.of(LocalTime.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIMESTAMP.getVendorTypeNumber(), JavaType.of(Timestamp.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIME_WITH_TIMEZONE.getVendorTypeNumber(), JavaType.of(OffsetTime.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIMESTAMP_WITH_TIMEZONE.getVendorTypeNumber(), JavaType.of(ZonedDateTime.class));

		DEFAULT_TYPE_MAP.put(JDBCType.CLOB.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.BLOB.getVendorTypeNumber(), JavaType.of(byte[].class));
		DEFAULT_TYPE_MAP.put(JDBCType.NCLOB.getVendorTypeNumber(), JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.REF.getVendorTypeNumber(), JavaType.of(Ref.class));
		DEFAULT_TYPE_MAP.put(JDBCType.SQLXML.getVendorTypeNumber(), JavaType.of(SQLXML.class));

		DEFAULT_TYPE_MAP.put(JDBCType.ARRAY.getVendorTypeNumber(), JavaType.of(Object[].class));

		DEFAULT_TYPE_MAP.put(JDBCType.OTHER.getVendorTypeNumber(), JavaType.of(Object.class));
	}

	/**
	 * コンストラクタ
	 */
	protected AbstractDialect() {
		this('$', DEFAULT_WILDCARDS);
	}

	/**
	 * コンストラクタ
	 * @param escapeChar like検索時のエスケープ文字
	 * @param wildcards like検索時のワイルドカード文字配列
	 */
	protected AbstractDialect(final char escapeChar, final char[] wildcards) {
		this.escapeChar = escapeChar;
		this.wildcards = wildcards != null ? wildcards : DEFAULT_WILDCARDS;
		this.escapePattern = generateEscapePattern(this.escapeChar, this.wildcards);
		this.typeMap = new ConcurrentHashMap<>(DEFAULT_TYPE_MAP);
		this.expressionFunction = new StringFunction(this);
	}

	/**
	 * LIKEパターン文字列をエスケープする正規表現を生成する
	 * @param escapeChar エスケープ文字
	 * @param wildcards ワイルドカードキャラクタ配列
	 * @return LIKEパターン文字列をエスケープする正規表現
	 */
	protected Pattern generateEscapePattern(final char escapeChar, final char[] wildcards) {
		StringBuilder builder = new StringBuilder();
		builder.append("[");
		for (char wildcard : wildcards) {
			if (escapeChar == '[' || escapeChar == ']') {
				builder.append("\\");
			}
			builder.append(Matcher.quoteReplacement(String.valueOf(escapeChar)));
			if (wildcard == '[' || wildcard == ']') {
				builder.append("\\");
			}
			builder.append(wildcard);
		}
		builder.append("]");
		return Pattern.compile(builder.toString());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getExpressionFunction()
	 */
	@Override
	public StringFunction getExpressionFunction() {
		return this.expressionFunction;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseType()
	 */
	@Override
	public String getDatabaseType() {
		return getDatabaseName().toLowerCase();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#escapeLikePattern(java.lang.CharSequence)
	 */
	@Override
	public String escapeLikePattern(final CharSequence pattern) {
		if (pattern == null) {
			return null;
		}
		Matcher matcher = escapePattern.matcher(pattern);
		return matcher.replaceAll(Matcher.quoteReplacement(String.valueOf(escapeChar)) + "$0");
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getLimitClause(long, long)
	 */
	@Override
	public String getLimitClause(final long limit, final long offset) {
		StringBuilder builder = new StringBuilder();
		if (limit > 0) {
			builder.append("LIMIT ").append(limit).append(" ");
		}
		if (offset > 0) {
			builder.append("OFFSET ").append(offset);
		}
		if (builder.length() > 0) {
			builder.append(System.lineSeparator());
		}
		return builder.toString();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getJavaType(java.sql.SQLType, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final SQLType sqlType, final String sqlTypeName) {
		return this.getJavaType(sqlType.getVendorTypeNumber(), sqlTypeName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getJavaType(int, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final int sqlType, final String sqlTypeName) {
		return this.typeMap.getOrDefault(sqlType, JavaType.of(Object.class));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getEscapeChar()
	 */
	@Override
	public char getEscapeChar() {
		return escapeChar;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addForUpdateClause(java.lang.StringBuilder, jp.co.future.uroborosql.enums.ForUpdateType, int)
	 */
	@Override
	public StringBuilder addForUpdateClause(final StringBuilder sql, final ForUpdateType forUpdateType,
			final int waitSeconds) {
		switch (forUpdateType) {
		case WAIT:
			return new StringBuilder().append(sql.toString()).append("FOR UPDATE WAIT ").append(waitSeconds);
		case NOWAIT:
			return new StringBuilder().append(sql.toString()).append("FOR UPDATE NOWAIT");
		default:
			return new StringBuilder().append(sql.toString()).append("FOR UPDATE");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#addOptimizerHints(java.lang.StringBuilder, java.util.List)
	 */
	@Override
	public StringBuilder addOptimizerHints(final StringBuilder sql, final List<String> hints) {
		if (!supportsOptimizerHints()) {
			throw new IllegalStateException("Optimizer Hints is not supported.");
		} else {
			return sql;
		}
	}
}
