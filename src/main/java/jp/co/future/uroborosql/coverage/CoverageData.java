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

/**
 * カバレッジログ出力用データクラス
 *
 * @author ota
 */
public class CoverageData {
	/** カバレッジロガー. */
	private static final Logger COVERAGE_LOG = LoggerFactory.getLogger("jp.co.future.uroborosql.sql.coverage");

	/** SQL名. */
	private final String sqlName;

	/** 変換前SQL. */
	private final String sql;

	/** MD5文字列. */
	private final String md5;

	/** 分岐情報. */
	private final PassedRoute passRoute;

	/**
	 * コンストラクタ
	 *
	 * @param sqlName SQL名
	 * @param sql SQL
	 * @param passRoute 分岐情報
	 */
	public CoverageData(final String sqlName, final String sql, final PassedRoute passRoute) {
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
		try {
			var digest = MessageDigest.getInstance("MD5");
			var hash = digest.digest(original.getBytes("UTF-8"));
			var builder = new StringBuilder();
			for (var element : hash) {
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
		var builder = new StringBuilder();
		builder.append("{\"sqlName\":");
		if (sqlName != null) {
			builder.append(sqlName.replace('/', '/'));
		} else {
			builder.append("");
		}
		builder.append(",\"md5\":").append(md5)
				.append(",\"passRoute\":").append(passRoute)
				.append("}");
		return builder.toString();
	}

	/**
	 * SQL名取得.
	 *
	 * @return SQLファイルのルートからの相対パス（ファイル拡張子なし）
	 */
	public String getSqlName() {
		return sqlName;
	}

	/**
	 * 変換前SQL取得.
	 *
	 * @return 変換前SQL
	 */
	public String getSql() {
		return sql;
	}

	/**
	 * SQLのMD5取得.
	 *
	 * @return MD5
	 */
	public String getMd5() {
		return md5;
	}

	/**
	 * 分岐情報取得.
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