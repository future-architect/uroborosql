/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Base64;
import java.util.List;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;

import jp.co.future.uroborosql.AbstractResultSetWrapper;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.ArrayUtils;

/**
 * ResultSetのラッパークラス。検索結果に暗号化したカラムが含まれる場合復号化して値を返す
 *
 * @author H.Sugimoto
 */
public class SecretResultSet extends AbstractResultSetWrapper {
	/** 暗号キー */
	private SecretKey secretKey = null;

	/** 暗号器 */
	private Cipher cipher = null;

	/** IVを利用するかどうか */
	private boolean useIV = false;

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
	SecretResultSet(final ResultSet wrapped, final SecretKey secretKey, final Cipher cipher, final boolean useIV,
			final List<String> cryptColumnNames,
			final Charset charset) {
		super(wrapped);
		this.secretKey = secretKey;
		this.cipher = cipher;
		this.useIV = useIV;
		this.cryptColumnNames = cryptColumnNames;
		this.charset = charset;
	}

	/**
	 * 復号化
	 *
	 * @param secret 暗号化文字列
	 * @return 復号化した文字列
	 */
	private String decode(final Object secret) {
		if (secret == null) {
			return null;
		}

		String secretStr = secret.toString();
		if (!secretStr.isEmpty()) {
			byte[] secretData = Base64.getUrlDecoder().decode(secretStr);

			synchronized (cipher) {
				try {
					if (useIV) {
						int blockSize = cipher.getBlockSize();
						byte[] iv = ArrayUtils.subarray(secretData, 0, blockSize);
						secretData = ArrayUtils.subarray(secretData, blockSize, secretData.length);
						IvParameterSpec ips = new IvParameterSpec(iv);
						cipher.init(Cipher.DECRYPT_MODE, secretKey, ips);
					}
					return new String(cipher.doFinal(secretData), getCharset());
				} catch (Exception ex) {
					return secretStr;
				}
			}
		} else {
			return secretStr;
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#close()
	 */
	@Override
	public void close() throws SQLException {
		this.cipher = null;
		this.useIV = false;
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
		String columnLabel = getWrapped().getMetaData().getColumnLabel(columnIndex);

		return getString(columnLabel);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getString(java.lang.String)
	 */
	@Override
	public String getString(final String columnLabel) throws SQLException {
		String val = getWrapped().getString(columnLabel);
		if (this.cryptColumnNames.contains(CaseFormat.UPPER_SNAKE_CASE.convert(columnLabel))) {
			return decode(val);
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
		String columnLabel = getWrapped().getMetaData().getColumnLabel(columnIndex);

		return getObject(columnLabel);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getObject(int, java.lang.Class)
	 */
	@Override
	public <T> T getObject(final int columnIndex, final Class<T> type) throws SQLException {
		String columnLabel = getWrapped().getMetaData().getColumnLabel(columnIndex);

		return getObject(columnLabel, type);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.sql.ResultSet#getObject(java.lang.String)
	 */
	@Override
	public Object getObject(final String columnLabel) throws SQLException {
		Object val = getWrapped().getObject(columnLabel);
		if (this.cryptColumnNames.contains(CaseFormat.UPPER_SNAKE_CASE.convert(columnLabel)) && val instanceof String) {
			return decode(val);
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
		T val = getWrapped().getObject(columnLabel, type);
		if (this.cryptColumnNames.contains(CaseFormat.UPPER_SNAKE_CASE.convert(columnLabel)) && val instanceof String) {
			return (T) decode(val);
		} else {
			return val;
		}
	}

	/**
	 * cipherを取得します。
	 *
	 * @return cipher
	 */
	public Cipher getCipher() {
		return this.cipher;
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
