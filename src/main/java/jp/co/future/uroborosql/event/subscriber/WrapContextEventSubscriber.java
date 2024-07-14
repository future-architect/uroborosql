/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.util.regex.Pattern;

import jp.co.future.uroborosql.event.TransformSqlEvent;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * SQL文字列の前後をWrapするイベントサブスクライバ
 *
 * @author H.Sugimoto
 * @since v1.0.0
 */
public class WrapContextEventSubscriber extends EventSubscriber {

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
	public WrapContextEventSubscriber() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param wrappedSqlBeginParts Wrap用SQL（前部分）
	 * @param wrappedSqlEndParts Wrap用SQL（後部分）
	 * @param wrapIgnorePattern Wrapを無視するSQLのパターン
	 */
	public WrapContextEventSubscriber(final String wrappedSqlBeginParts, final String wrappedSqlEndParts,
			final String wrapIgnorePattern) {
		this.wrappedSqlBeginParts = wrappedSqlBeginParts;
		this.wrappedSqlEndParts = wrappedSqlEndParts;
		this.wrapIgnorePattern = wrapIgnorePattern;
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.subscriber.EventSubscriber#initialize()
	 */
	@Override
	public void initialize() {
		if (getWrapIgnorePattern() != null && !"".equals(getWrapIgnorePattern())) {
			ignorePattern = Pattern.compile(getWrapIgnorePattern(), Pattern.DOTALL | Pattern.CASE_INSENSITIVE);
		}

		transformSqlListener(this::transformSql);
	}

	void transformSql(final TransformSqlEvent evt) {
		// SQLの前後を別のSQLでWrapする加工を行う。
		// ただし、以下の場合は加工対象外とする。
		// - wrapIgnorePatternに当てはまるSQLの場合
		// - wrapIgnorePatternの指定がない場合

		var wrapIgnore = true;
		if (ignorePattern != null) {
			var matcher = ignorePattern.matcher(evt.getSql());
			wrapIgnore = matcher.matches();
		}

		var newSql = evt.getSql();
		// sqlを別のSQLで囲む場合のSQLを追加
		if (!wrapIgnore && ObjectUtils.isNotEmpty(getWrappedSqlBeginParts())) {
			newSql = getWrappedSqlBeginParts() + newSql;
		}
		if (!wrapIgnore && ObjectUtils.isNotEmpty(getWrappedSqlEndParts())) {
			newSql = newSql + getWrappedSqlEndParts();
		}

		if (!newSql.equals(evt.getSql())) {
			evt.setSql(newSql);
		}
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
	 * @return WrapContextEventSubscriber
	 */
	public WrapContextEventSubscriber setWrapIgnorePattern(final String wrapIgnorePattern) {
		this.wrapIgnorePattern = wrapIgnorePattern;
		return this;
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
	 * @return WrapContextEventSubscriber
	 */
	public WrapContextEventSubscriber setWrappedSqlBeginParts(final String wrappedSqlBeginParts) {
		this.wrappedSqlBeginParts = wrappedSqlBeginParts;
		return this;
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
	 * @return WrapContextEventSubscriber
	 */
	public WrapContextEventSubscriber setWrappedSqlEndParts(final String wrappedSqlEndParts) {
		this.wrappedSqlEndParts = wrappedSqlEndParts;
		return this;
	}
}
