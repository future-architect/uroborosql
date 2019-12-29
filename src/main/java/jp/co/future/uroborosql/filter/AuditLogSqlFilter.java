/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.filter;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.Parameter;

/**
 * 監査用ログを出力するSqlFilter
 *
 * @author K.Miyazaki, J.Choi
 *
 */
public class AuditLogSqlFilter extends AbstractSqlFilter {
	private static final Logger LOG = LoggerFactory.getLogger(AuditLogSqlFilter.class);

	/** 機能名取得用のパラメータキー名 */
	private String funcIdKey = "_funcId";

	/** ユーザ名取得用のパラメータキー名 */
	private String userNameKey = "_userName";

	/** ユーザ名の初期値 */
	private static final String DEFAULT_USER_NAME = "UNKNOWN";

	/** 機能名の初期値 */
	private static final String DEFAULT_FUNC_ID = "UNKNOWN";

	/**
	 * コンストラクタ
	 */
	public AuditLogSqlFilter() {
	}

	/**
	 * バインドパラメータに設定した機能IDのキー名を設定する.
	 *
	 * @param funcIdKey 機能IDのキー名
	 * @return AuditLogSqlFilter
	 */
	public AuditLogSqlFilter setFuncIdKey(final String funcIdKey) {
		this.funcIdKey = funcIdKey;
		return this;
	}

	/**
	 * バインドパラメータに設定したユーザ名のキー名を設定する.
	 *
	 * @param userNameKey ユーザ名のキー名
	 * @return AuditLogSqlFilter
	 */
	public AuditLogSqlFilter setUserNameKey(final String userNameKey) {
		this.userNameKey = userNameKey;
		return this;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, java.sql.ResultSet)
	 */
	@Override
	public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
			final ResultSet resultSet) {
		// カウント初期値
		int rowCount = -1;
		try {
			// resultSetのカーソル種別を取得
			// 種別「TYPE_FORWARD_ONLY」の場合、beforeFirstメソッドが効かないため除外
			if (resultSet.getType() != ResultSet.TYPE_FORWARD_ONLY) {
				// 件数結果取得
				resultSet.last();
				rowCount = resultSet.getRow();
				resultSet.beforeFirst();
			}
		} catch (SQLException e) {
			// ここでの例外は実処理に影響を及ぼさないよう握りつぶす
		}

		String userName = getParam(sqlContext, userNameKey);
		if (userName == null) {
			// ユーザ名が設定されていない時
			userName = DEFAULT_USER_NAME;
		}

		String funcId = getParam(sqlContext, funcIdKey);
		if (funcId == null) {
			// 機能IDが設定されていない時
			funcId = DEFAULT_FUNC_ID;
		}

		LOG.debug(new AuditData(userName, funcId, sqlContext.getSqlId(), sqlContext.getSqlName(), sqlContext
				.getExecutableSql(), rowCount).toString());
		return resultSet;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doUpdate(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int)
	 */
	@Override
	public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int result) {
		String userName = getParam(sqlContext, userNameKey);
		if (userName == null) {
			// ユーザ名が設定されていない時
			userName = DEFAULT_USER_NAME;
		}

		String funcId = getParam(sqlContext, funcIdKey);
		if (funcId == null) {
			// 機能IDが設定されていない時
			funcId = DEFAULT_FUNC_ID;
		}

		LOG.debug(new AuditData(userName, funcId, sqlContext.getSqlId(), sqlContext.getSqlName(), sqlContext
				.getExecutableSql(), result).toString());
		return result;

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doBatch(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int[])
	 */
	@Override
	public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int[] result) {
		String userName = getParam(sqlContext, userNameKey);
		if (userName == null) {
			// ユーザ名が設定されていない時
			userName = DEFAULT_USER_NAME;
		}

		String funcId = getParam(sqlContext, funcIdKey);
		if (funcId == null) {
			// 機能IDが設定されていない時
			funcId = DEFAULT_FUNC_ID;
		}
		int rowCount = -1;
		if (LOG.isDebugEnabled()) {
			try {
				rowCount = preparedStatement.getUpdateCount();
			} catch (SQLException ex) {
				// ここでの例外は実処理に影響を及ぼさないよう握りつぶす
			}
		}
		LOG.debug(new AuditData(userName, funcId, sqlContext.getSqlId(), sqlContext.getSqlName(), sqlContext
				.getExecutableSql(), rowCount).toString());
		return result;
	}

	/**
	 * パラメータ値を取得する
	 *
	 * @param ctx SqlContext
	 * @param key パラメータのキー
	 * @return パラメータの値。キーに対するパラメータが存在しない場合は<code>null</code>.
	 */
	private String getParam(final SqlContext ctx, final String key) {
		Parameter param = ctx.getParam(key);
		if (param == null) {
			return null;
		} else {
			return String.valueOf(param.getValue());
		}
	}

	/**
	 * 監査用データ
	 */
	private static final class AuditData {
		/** SQL文中でJSONとして不正な文字を置換するためのMap */
		private static final String[] ESC_CHARS = {
				"\\\\:\\\\\\\\",
				"\\\":\\\\\\\"",
				"/:\\\\/",
				"\\t:\\\\t",
				"\\f:\\\\f",
				"\\r\\n: ",
				"\\n: ",
				"\\r: "
		};

		private final String userName;
		private final String funcId;
		private final String sqlId;
		private final String sqlName;
		private final String sql;
		private final int rowCount;

		/**
		 * コンストラクタ
		 *
		 * @param userName ユーザ名
		 * @param funcId 機能ID
		 * @param sqlId SQL-ID
		 * @param sqlName SQL名
		 * @param sql SQL文
		 * @param rowCount 件数
		 */
		private AuditData(final String userName, final String funcId, final String sqlId, final String sqlName,
				final String sql, final int rowCount) {
			this.userName = userName;
			this.funcId = funcId;
			this.sqlId = sqlId;
			this.sqlName = sqlName;
			this.sql = sql;
			this.rowCount = rowCount;
		}

		/**
		 * JSONとして不正な文字をエスケープする.
		 *
		 * @param str エスケープ対象文字列
		 * @return エスケープ後文字列
		 */
		private String escapeJson(final String str) {
			if (str == null) {
				return null;
			}
			String buff = str;
			for (String escChar : ESC_CHARS) {
				String[] parts = escChar.split(":");
				buff = buff.replaceAll(parts[0], parts[1]);
			}
			return buff;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.lang.Object#toString()
		 */
		@Override
		public String toString() {
			return "{\"userName\":\"" + escapeJson(userName)
					+ "\",\"funcId\":\"" + escapeJson(funcId)
					+ "\",\"sqlId\":\"" + escapeJson(sqlId)
					+ "\",\"sqlName\":\"" + escapeJson(sqlName)
					+ "\",\"sql\":\"" + escapeJson(sql)
					+ "\",\"rowCount\":" + rowCount + "}";
		}

	}
}
