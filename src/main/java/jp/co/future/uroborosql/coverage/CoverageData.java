/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.coverage;

import java.security.MessageDigest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.SqlAgent;

/**
 * カバレッジログ出力用データクラス
 *
 * @author ota
 */
public class CoverageData {
	protected static final Logger COVERAGE_LOG = LoggerFactory.getLogger(SqlAgent.class.getPackage().getName()
			+ ".sql.coverage");
	private final String sqlName;
	private final String sql;
	private final String md5;
	private final PassedRoute passRoute;

	/**
	 * コンストラクタ
	 *
	 * @param sqlName SQL名
	 * @param sql SQL
	 * @param passRoute 分岐情報
	 */
	public CoverageData(final String sqlName, final String sql, final PassedRoute passRoute) {
		super();
		this.sqlName = sqlName;
		this.sql = sql;
		this.md5 = makeMd5(sql);
		this.passRoute = passRoute;
	}

	/**
	 * MD5文字列の生成
	 *
	 * @param original 生成元文字列
	 * @return MD5文字列
	 */
	private String makeMd5(final String original) {
		MessageDigest digest = null;
		try {
			digest = MessageDigest.getInstance("MD5");
			byte[] hash = digest.digest(original.getBytes("UTF-8"));
			StringBuilder builder = new StringBuilder();
			for (byte element : hash) {
				if ((0xff & element) < 0x10) {
					builder.append("0" + Integer.toHexString(0xff & element));
				} else {
					builder.append(Integer.toHexString(0xff & element));
				}
			}
			return builder.toString();
		} catch (Exception ex) {
			COVERAGE_LOG.error(ex.getMessage(), ex);
		}
		return "";
	}

	/**
	 * JSON化
	 *
	 * @return JSON
	 */
	public String toJSON() {
		return "{\"sqlName\":" + sqlName.replaceAll("/", "\\/") + ",\"md5\":" + md5 + ",\"passRoute\":"
				+ passRoute + "}";
	}

	/**
	 * SQL名取得
	 *
	 * @return SQLファイルのルートからの相対パス（ファイル拡張子なし）
	 */
	public String getSqlName() {
		return sqlName;
	}

	/**
	 * 変換前SQL取得
	 *
	 * @return 変換前SQL
	 */
	public String getSql() {
		return sql;
	}

	/**
	 * SQLのMD5取得
	 *
	 * @return MD5
	 */
	public String getMd5() {
		return md5;
	}

	/**
	 * 分岐情報取得
	 *
	 * @return 分岐情報
	 */
	public PassedRoute getPassRoute() {
		return passRoute;
	}

	/**
	 * {@inheritDoc}
	 *
	 * JSON化文字列を返す
	 *
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return toJSON();
	}

}