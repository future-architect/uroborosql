/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.dialect;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Dialectの抽象親クラス
 *
 * @author H.Sugimoto
 */
public abstract class AbstractDialect implements Dialect {
	private static final char[] DEFAULT_WILDCARDS = { '%', '_' };

	private final char escapeChar;
	private final char[] wildcards;
	private final Pattern escapePattern;

	protected AbstractDialect() {
		this('$', DEFAULT_WILDCARDS);
	}

	protected AbstractDialect(final char escapeChar, final char[] wildcards) {
		this.escapeChar = escapeChar;
		this.wildcards = wildcards != null ? wildcards : DEFAULT_WILDCARDS;
		this.escapePattern = generateEscapePattern(this.escapeChar, this.wildcards);
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
	 * @see jp.co.future.uroborosql.dialect.Dialect#getDatabaseType()
	 */
	@Override
	public String getDatabaseType() {
		return getDatabaseName().toLowerCase();
	}

	@Override
	public String escapeLikePattern(final CharSequence pattern) {
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
}
