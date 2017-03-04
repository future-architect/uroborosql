package jp.co.future.uroborosql.filter;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.regex.Pattern;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.Parameter;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 監査用ログを出力するSqlFilter
 *
 * @author K.Miyazaki, J.Choi
 *
 */
public class AuditLogSqlFilter extends AbstractSqlFilter {
	private static final Logger LOG = LoggerFactory.getLogger(AuditLogSqlFilter.class);

	/** 機能名取得用のパラメータキー名 */
	private static final String FUNC_ID_KEY = "_funcId";

	/** ユーザ名取得用のパラメータキー名 */
	private static final String USER_NAME_KEY = "_userName";

	/** ユーザ名の初期値 */
	private static final String DEFAULT_USER_NAME = "UNKNOWN";

	/** 機能名の初期値 */
	private static final String DEFAULT_FUNC_ID = "UNKNOWN";

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

		String userName = getParam(sqlContext, USER_NAME_KEY);
		if (userName == null) {
			// ユーザ名が設定されていない時
			userName = DEFAULT_USER_NAME;
		}

		String funcId = getParam(sqlContext, FUNC_ID_KEY);
		if (funcId == null) {
			// 機能IDが設定されていない時
			funcId = DEFAULT_FUNC_ID;
		}

		LOG.debug(ToStringBuilder.reflectionToString(
				new AuditData(userName, funcId, sqlContext.getSqlId(), sqlContext.getSqlName(), sqlContext
						.getExecutableSql(), rowCount), ToStringStyle.JSON_STYLE));

		return resultSet;
	}

	@Override
	public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int result) {
		String userName = getParam(sqlContext, USER_NAME_KEY);
		if (userName == null) {
			// ユーザ名が設定されていない時
			userName = DEFAULT_USER_NAME;
		}

		String funcId = getParam(sqlContext, FUNC_ID_KEY);
		if (funcId == null) {
			// 機能IDが設定されていない時
			funcId = DEFAULT_FUNC_ID;
		}

		LOG.debug(ToStringBuilder.reflectionToString(
				new AuditData(userName, funcId, sqlContext.getSqlId(), sqlContext.getSqlName(), sqlContext
						.getExecutableSql(), result), ToStringStyle.JSON_STYLE));

		return result;

	}

	@Override
	public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int[] result) {
		String userName = getParam(sqlContext, USER_NAME_KEY);
		if (userName == null) {
			// ユーザ名が設定されていない時
			userName = DEFAULT_USER_NAME;
		}

		String funcId = getParam(sqlContext, FUNC_ID_KEY);
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
		LOG.debug(ToStringBuilder.reflectionToString(
				new AuditData(userName, funcId, sqlContext.getSqlId(), sqlContext.getSqlName(), sqlContext
						.getExecutableSql(), rowCount), ToStringStyle.JSON_STYLE));

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
	 *
	 * @author H.Sugimoto
	 */
	private static final class AuditData {
		/** SQL文中の改行文字除去用正規表現 */
		private static final Pattern PAT = Pattern.compile("(?m)(\r\n|\r|\n)");

		@SuppressWarnings("unused")
		private final String userName;
		@SuppressWarnings("unused")
		private final String funcId;
		@SuppressWarnings("unused")
		private final String sqlId;
		@SuppressWarnings("unused")
		private final String sqlName;
		@SuppressWarnings("unused")
		private final String sql;
		@SuppressWarnings("unused")
		private final int rowCount;

		/**
		 * @param userName ユーザ名
		 * @param funcId 機能I
		 * @param sqlId SQL-ID
		 * @param sqlName SQL名
		 * @param sql SQL文
		 * @param rowCount
		 */
		private AuditData(final String userName, final String funcId, final String sqlId, final String sqlName,
				final String sql, final int rowCount) {
			super();
			this.userName = StringEscapeUtils.escapeJson(userName);
			this.funcId = StringEscapeUtils.escapeJson(funcId);
			this.sqlId = StringEscapeUtils.escapeJson(sqlId);
			this.sqlName = StringEscapeUtils.escapeJson(sqlName);
			this.sql = StringEscapeUtils.escapeJson(PAT.matcher(sql).replaceAll(" "));
			this.rowCount = rowCount;
		}
	}
}
