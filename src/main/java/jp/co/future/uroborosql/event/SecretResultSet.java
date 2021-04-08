/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.function.Function;

import jp.co.future.uroborosql.AbstractResultSetWrapper;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * ResultSetのラッパークラス。検索結果に暗号化したカラムが含まれる場合復号化して値を返す
 *
 * @author H.Sugimoto
 */
public class SecretResultSet extends AbstractResultSetWrapper {

	private final Function<Object, String> decode;

	/**
	 * キャラクタセット（デフォルトUTF-8）
	 */
	private Charset charset = StandardCharsets.UTF_8;

	/**
	 * 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定する
	 */
	private List<String> cryptColumnNames = null;

	/**
	 * コンストラクタ
	 *
	 * @param wrapped 元のResultSet
	 * @param secretKey 暗号キー
	 * @param cipher 暗号器
	 * @param useIV IVを利用するかどうか
	 * @param cryptColumnNames 暗号対象カラム名リスト
	 * @param charset キャラクタセット
	 */
	public SecretResultSet(final ResultSet wrapped, final Function<Object, String> decode,
			final List<String> cryptColumnNames, final Charset charset) {
		super(wrapped);
		this.cryptColumnNames = cryptColumnNames;
		this.charset = charset;
		this.decode = decode;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#close()
	 */
	@Override
	public void close() throws SQLException {
		this.cryptColumnNames = null;
		this.charset = null;
		super.close();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getString(int)
	 */
	@Override
	public String getString(final int columnIndex) throws SQLException {
		var columnLabel = getWrapped().getMetaData().getColumnLabel(columnIndex);

		return getString(columnLabel);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getString(java.lang.String)
	 */
	@Override
	public String getString(final String columnLabel) throws SQLException {
		var val = getWrapped().getString(columnLabel);
		if (this.cryptColumnNames.contains(CaseFormat.UPPER_SNAKE_CASE.convert(columnLabel))) {
			return decode.apply(val);
		} else {
			return val;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getObject(int)
	 */
	@Override
	public Object getObject(final int columnIndex) throws SQLException {
		var columnLabel = getWrapped().getMetaData().getColumnLabel(columnIndex);

		return getObject(columnLabel);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getObject(int, java.lang.Class)
	 */
	@Override
	public <T> T getObject(final int columnIndex, final Class<T> type) throws SQLException {
		var columnLabel = getWrapped().getMetaData().getColumnLabel(columnIndex);

		return getObject(columnLabel, type);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getObject(java.lang.String)
	 */
	@Override
	public Object getObject(final String columnLabel) throws SQLException {
		var val = getWrapped().getObject(columnLabel);
		if (this.cryptColumnNames.contains(CaseFormat.UPPER_SNAKE_CASE.convert(columnLabel)) && val instanceof String) {
			return decode.apply(val);
		} else {
			return val;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getObject(java.lang.String, java.lang.Class)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T> T getObject(final String columnLabel, final Class<T> type) throws SQLException {
		var val = getWrapped().getObject(columnLabel, type);
		if (this.cryptColumnNames.contains(CaseFormat.UPPER_SNAKE_CASE.convert(columnLabel)) && val instanceof String) {
			return (T) decode.apply(val);
		} else {
			return val;
		}
	}

	/**
	 * キャラクタセット（デフォルトUTF-8）を取得します。
	 *
	 * @return キャラクタセット（デフォルトUTF-8）
	 */
	public Charset getCharset() {
		return this.charset;
	}

	/**
	 * 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定 を取得します。
	 *
	 * @return 暗号化、復号化を行うカラム名のリスト. カラム名はスネークケース（大文字）で指定する
	 */
	public List<String> getCryptColumnNames() {
		return this.cryptColumnNames;
	}
}
