/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.event.subscriber;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.slf4j.Logger;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.event.AfterSqlBatchEvent;
import jp.co.future.uroborosql.event.AfterSqlQueryEvent;
import jp.co.future.uroborosql.event.AfterSqlUpdateEvent;
import jp.co.future.uroborosql.log.EventLogger;

/**
 * 監査用ログを出力するイベントサブスクライバ
 *
 * @author H.Sugimoto
 * @since v1.0.0
 *
 */
public class AuditLogEventSubscriber extends EventSubscriber {
	/** イベントロガー */
	private static final Logger EVENT_LOG = EventLogger.getEventLogger("auditlog");

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
	public AuditLogEventSubscriber() {
	}

	/**
	 *
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.event.subscriber.EventSubscriber#initialize()
	 */
	@Override
	public void initialize() {
		afterSqlQueryListener(this::afterSqlQuery);
		afterSqlUpdateListener(this::afterSqlUpdate);
		afterSqlBatchListener(this::afterSqlBatch);
	}

	void afterSqlQuery(final AfterSqlQueryEvent evt) {
		var resultSet = evt.getResultSet();
		// カウント初期値
		var rowCount = -1;
		try {
			// resultSetのカーソル種別を取得
			// 種別「TYPE_FORWARD_ONLY」の場合、beforeFirstメソッドが効かないため除外
			if (resultSet.getType() != ResultSet.TYPE_FORWARD_ONLY) {
				// 件数結果取得
				resultSet.last();
				rowCount = resultSet.getRow();
				resultSet.beforeFirst();
			}
		} catch (SQLException ex) {
			// ここでの例外は実処理に影響を及ぼさないよう握りつぶす
		}

		var userName = getParam(evt.getExecutionContext(), userNameKey, DEFAULT_USER_NAME);
		var funcId = getParam(evt.getExecutionContext(), funcIdKey, DEFAULT_FUNC_ID);
		var reportRowCount = rowCount;
		EVENT_LOG.atDebug()
				.setMessage("AuditData: {}")
				.addArgument(() -> new AuditData(userName,
						funcId,
						evt.getExecutionContext().getSqlId(),
						evt.getExecutionContext().getSqlName(),
						evt.getExecutionContext().getExecutableSql(),
						reportRowCount))
				.log();
	}

	void afterSqlUpdate(final AfterSqlUpdateEvent evt) {
		var userName = getParam(evt.getExecutionContext(), userNameKey, DEFAULT_USER_NAME);
		var funcId = getParam(evt.getExecutionContext(), funcIdKey, DEFAULT_FUNC_ID);
		EVENT_LOG.atDebug()
				.setMessage("AuditData: {}")
				.addArgument(() -> new AuditData(userName,
						funcId,
						evt.getExecutionContext().getSqlId(),
						evt.getExecutionContext().getSqlName(),
						evt.getExecutionContext().getExecutableSql(),
						evt.getCount()))
				.log();
	}

	void afterSqlBatch(final AfterSqlBatchEvent evt) {
		var userName = getParam(evt.getExecutionContext(), userNameKey, DEFAULT_USER_NAME);
		var funcId = getParam(evt.getExecutionContext(), funcIdKey, DEFAULT_FUNC_ID);
		EVENT_LOG.atDebug()
				.setMessage("AuditData: {}")
				.addArgument(() -> {
					var rowCount = -1;
					try {
						rowCount = evt.getPreparedStatement().getUpdateCount();
					} catch (SQLException ex) {
						// ここでの例外は実処理に影響を及ぼさないよう握りつぶす
					}
					return new AuditData(userName,
							funcId,
							evt.getExecutionContext().getSqlId(),
							evt.getExecutionContext().getSqlName(),
							evt.getExecutionContext().getExecutableSql(),
							rowCount);
				})
				.log();
	}

	/**
	 * バインドパラメータに設定した機能IDのキー名を設定する.
	 *
	 * @param funcIdKey 機能IDのキー名
	 * @return AuditLogEventSubscriber
	 */
	public AuditLogEventSubscriber setFuncIdKey(final String funcIdKey) {
		this.funcIdKey = funcIdKey;
		return this;
	}

	/**
	 * バインドパラメータに設定したユーザ名のキー名を設定する.
	 *
	 * @param userNameKey ユーザ名のキー名
	 * @return AuditLogEventSubscriber
	 */
	public AuditLogEventSubscriber setUserNameKey(final String userNameKey) {
		this.userNameKey = userNameKey;
		return this;
	}

	/**
	 * パラメータ値を取得する
	 *
	 * @param ctx ExecutionContext
	 * @param key パラメータのキー
	 * @param nullDefault パラメータ値が取得できなかった時のデフォルト値
	 * @return パラメータの値。キーに対するパラメータが存在しない場合は<code>null</code>.
	 */
	private String getParam(final ExecutionContext ctx, final String key, final String nullDefault) {
		var param = ctx.getParam(key);
		if (param == null) {
			return nullDefault;
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
			var buff = str;
			for (var escChar : ESC_CHARS) {
				var parts = escChar.split(":");
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
