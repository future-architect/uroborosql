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
import java.sql.SQLXML;
import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.utils.StringFunction;

/**
 * Dialectの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractDialect implements Dialect {
	private static final char[] DEFAULT_WILDCARDS = { '%', '_' };

	/** JDBCTypeとJavaTyoeのマッピング */
	protected static final Map<JDBCType, JavaType> DEFAULT_TYPE_MAP;

	/** like検索時のエスケープ文字 */
	private final char escapeChar;
	/** like検索時のワイルドカード文字配列 */
	private final char[] wildcards;
	/** エスケープするパターン */
	private final Pattern escapePattern;

	private final StringFunction expressionFunction;

	protected final Map<JDBCType, JavaType> typeMap;

	static {
		// initialize DEFAULT_TYPE_MAP
		DEFAULT_TYPE_MAP = new HashMap<>();
		DEFAULT_TYPE_MAP.put(JDBCType.CHAR, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.VARCHAR, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.LONGVARCHAR, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.NCHAR, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.NVARCHAR, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.LONGNVARCHAR, JavaType.of(String.class));

		DEFAULT_TYPE_MAP.put(JDBCType.NUMERIC, JavaType.of(BigDecimal.class));
		DEFAULT_TYPE_MAP.put(JDBCType.DECIMAL, JavaType.of(BigDecimal.class));

		DEFAULT_TYPE_MAP.put(JDBCType.BIT, JavaType.of(Boolean.class));
		DEFAULT_TYPE_MAP.put(JDBCType.BOOLEAN, JavaType.of(Boolean.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TINYINT, JavaType.of(byte.class));
		DEFAULT_TYPE_MAP.put(JDBCType.SMALLINT, JavaType.of(Short.class));
		DEFAULT_TYPE_MAP.put(JDBCType.INTEGER, JavaType.of(Integer.class));
		DEFAULT_TYPE_MAP.put(JDBCType.BIGINT, JavaType.of(Long.class));
		DEFAULT_TYPE_MAP.put(JDBCType.REAL, JavaType.of(Float.class));
		DEFAULT_TYPE_MAP.put(JDBCType.FLOAT, JavaType.of(Double.class));
		DEFAULT_TYPE_MAP.put(JDBCType.DOUBLE, JavaType.of(Double.class));

		DEFAULT_TYPE_MAP.put(JDBCType.BINARY, JavaType.of(byte[].class));
		DEFAULT_TYPE_MAP.put(JDBCType.VARBINARY, JavaType.of(byte[].class));
		DEFAULT_TYPE_MAP.put(JDBCType.LONGVARBINARY, JavaType.of(byte[].class));

		DEFAULT_TYPE_MAP.put(JDBCType.DATE, JavaType.of(ZonedDateTime.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIME, JavaType.of(LocalTime.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIMESTAMP, JavaType.of(Timestamp.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIME_WITH_TIMEZONE, JavaType.of(OffsetTime.class));
		DEFAULT_TYPE_MAP.put(JDBCType.TIMESTAMP_WITH_TIMEZONE, JavaType.of(ZonedDateTime.class));

		DEFAULT_TYPE_MAP.put(JDBCType.CLOB, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.BLOB, JavaType.of(byte[].class));
		DEFAULT_TYPE_MAP.put(JDBCType.NCLOB, JavaType.of(String.class));
		DEFAULT_TYPE_MAP.put(JDBCType.REF, JavaType.of(Ref.class));
		DEFAULT_TYPE_MAP.put(JDBCType.SQLXML, JavaType.of(SQLXML.class));

		DEFAULT_TYPE_MAP.put(JDBCType.ARRAY, JavaType.of(Object[].class));

		DEFAULT_TYPE_MAP.put(JDBCType.OTHER, JavaType.of(Object.class));
	}

	protected AbstractDialect() {
		this('$', DEFAULT_WILDCARDS);
	}

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
	 * @see jp.co.future.uroborosql.dialect.Dialect#getJavaType(java.sql.JDBCType, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final JDBCType jdbcType, final String jdbcTypeName) {
		return this.typeMap.getOrDefault(jdbcType, JavaType.of(Object.class));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.dialect.Dialect#getJavaType(int, java.lang.String)
	 */
	@Override
	public JavaType getJavaType(final int jdbcType, final String jdbcTypeName) {
		JDBCType type = null;
		try {
			type = JDBCType.valueOf(jdbcType);
		} catch (IllegalArgumentException ex) {
			type = JDBCType.OTHER;
		}
		return getJavaType(type, jdbcTypeName);
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
}
