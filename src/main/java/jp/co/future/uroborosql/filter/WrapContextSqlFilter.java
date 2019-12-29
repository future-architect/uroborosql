/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * SQL文字列の前後をWrapするSqlFilter
 *
 * @author H.Sugimoto
 */
public class WrapContextSqlFilter extends AbstractSqlFilter {

	/** Wrap用SQL（前部分） */
	private String wrappedSqlBeginParts;

	/** Wrap用SQL（後部分） */
	private String wrappedSqlEndParts;

	/** Wrapを無視するSQLのパターン */
	private String wrapIgnorePattern;

	/** Wrapを無視するSQLの正規表現 */
	private Pattern ignorePattern;

	/**
	 * コンストラクタ
	 */
	public WrapContextSqlFilter() {
		super();
	}

	/**
	 * コンストラクタ
	 *
	 * @param wrappedSqlBeginParts Wrap用SQL（前部分）
	 * @param wrappedSqlEndParts Wrap用SQL（後部分）
	 * @param wrapIgnorePattern Wrapを無視するSQLのパターン
	 */
	public WrapContextSqlFilter(final String wrappedSqlBeginParts, final String wrappedSqlEndParts,
			final String wrapIgnorePattern) {
		super();
		this.wrappedSqlBeginParts = wrappedSqlBeginParts;
		this.wrappedSqlEndParts = wrappedSqlEndParts;
		this.wrapIgnorePattern = wrapIgnorePattern;
	}

	@Override
	public void initialize() {
		if (getWrapIgnorePattern() != null && !"".equals(getWrapIgnorePattern())) {
			ignorePattern = Pattern.compile(getWrapIgnorePattern(), Pattern.DOTALL | Pattern.CASE_INSENSITIVE);
		}
	}

	/**
	 * SQLの前後を別のSQLでWrapする加工を行う。
	 * ただし、以下の場合は加工対象外とする。
	 * <ul>
	 * <li>wrapIgnorePatternに当てはまるSQLの場合</li>
	 * <li>wrapIgnorePatternの指定がない場合</li>
	 * </ul>
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doTransformSql(jp.co.future.uroborosql.context.SqlContext, java.lang.String)
	 */
	@Override
	public String doTransformSql(final SqlContext sqlContext, final String sql) {
		boolean wrapIgnore = true;
		if (ignorePattern != null) {
			Matcher matcher = ignorePattern.matcher(sql);
			wrapIgnore = matcher.matches();
		}

		String newSql = sql;
		// sqlを別のSQLで囲む場合のSQLを追加
		if (!wrapIgnore && StringUtils.isNotEmpty(getWrappedSqlBeginParts())) {
			newSql = getWrappedSqlBeginParts() + newSql;
		}
		if (!wrapIgnore && StringUtils.isNotEmpty(getWrappedSqlEndParts())) {
			newSql = newSql + getWrappedSqlEndParts();
		}

		return newSql;
	}

	/**
	 * Wrapを無視するSQLのパターン を取得します。
	 *
	 * @return Wrapを無視するSQLのパターン
	 */
	public String getWrapIgnorePattern() {
		return wrapIgnorePattern;
	}

	/**
	 * Wrapを無視するSQLのパターン を設定します。
	 *
	 * @param wrapIgnorePattern Wrapを無視するSQLのパターン
	 */
	public void setWrapIgnorePattern(final String wrapIgnorePattern) {
		this.wrapIgnorePattern = wrapIgnorePattern;
	}

	/**
	 * Wrap用SQL（前部分） を取得します。
	 *
	 * @return Wrap用SQL（前部分）
	 */
	public String getWrappedSqlBeginParts() {
		return wrappedSqlBeginParts;
	}

	/**
	 * Wrap用SQL（前部分） を設定します。
	 *
	 * @param wrappedSqlBeginParts Wrap用SQL（前部分）
	 */
	public void setWrappedSqlBeginParts(final String wrappedSqlBeginParts) {
		this.wrappedSqlBeginParts = wrappedSqlBeginParts;
	}

	/**
	 * Wrap用SQL（後部分） を取得します。
	 *
	 * @return Wrap用SQL（後部分）
	 */
	public String getWrappedSqlEndParts() {
		return wrappedSqlEndParts;
	}

	/**
	 * Wrap用SQL（後部分） を設定します。
	 *
	 * @param wrappedSqlEndParts Wrap用SQL（後部分）
	 */
	public void setWrappedSqlEndParts(final String wrappedSqlEndParts) {
		this.wrappedSqlEndParts = wrappedSqlEndParts;
	}
}
