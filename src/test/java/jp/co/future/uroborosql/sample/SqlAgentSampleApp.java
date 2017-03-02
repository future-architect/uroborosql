package jp.co.future.uroborosql.sample;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;

/**
 * SqlAgent（SQL実行エンジン）実装サンプル
 *
 * @author H.Sugimoto
 */
public class SqlAgentSampleApp {
	/**
	 * SQL実行のための設定オブジェクト（SqlConfig）。
	 * このオブジェクトから各種オブジェクトを取得できる。
	 * 環境で１つ（シングルトン）保持する
	 */
	private final SqlConfig config;

	/**
	 * SqlAgentSampleAppのコンストラクタ
	 *
	 * @param driver
	 * @param url
	 * @param user
	 * @param password
	 * @throws Exception
	 */
	public SqlAgentSampleApp(final String url, final String user, final String password) throws Exception {
		// SQL設定オブジェクトを生成する(JDBCを利用する場合）
		config = DefaultSqlConfig.getConfig(url, user, password);

		// JNDIなどのDataSourceを利用する場合は、DataSource用のConnectionSupplierを指定してください
		// Connectionの管理をDataSource側に委譲します
		// config = SqlConfig.getConfig(dataSourceName);

		// SQL実行時に行う各種フィルタ処理を管理するクラスを生成
		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());

	}

	/**
	 * 検索SQLの実行
	 *
	 * @param sqlName
	 * @return
	 * @throws SQLException SQL例外
	 */
	public List<Map<String, Object>> query(final String sqlName) throws SQLException {
		return query(sqlName, null);
	}

	/**
	 * 検索SQLの実行
	 *
	 * @param sqlName
	 * @param params
	 * @return
	 * @throws SQLException SQL例外
	 */
	public List<Map<String, Object>> query(final String sqlName, final Map<String, Object> params)
			throws SQLException {

		List<Map<String, Object>> ans = new ArrayList<>();

		// SqlAgent（SQL実行エンジン）を設定オブジェクトから取得
		try (SqlAgent agent = config.createAgent()) {
			// SQLファイル名や、SQLに渡すパラメータを管理するSqlContextを生成
			SqlContext ctx = agent.contextFrom(sqlName);

			// 生成したSqlContextに実行するSQLファイル名とバインドパラメータを設定
			if (params != null) {
				params.forEach((key, val) -> ctx.param(key, val));
			}

			// SQLの実行
			try (ResultSet rs = agent.query(ctx)) {
				// 実行結果はResultSetで返ってくるので、値を取得
				List<String> headers = new ArrayList<>();
				ResultSetMetaData rsmd = rs.getMetaData();
				int count = rsmd.getColumnCount();
				for (int i = 1; i <= count; i++) {
					String label = rsmd.getColumnLabel(i);
					headers.add(label);
				}

				while (rs.next()) {
					Map<String, Object> line = new LinkedHashMap<>();
					headers.forEach(header -> {
						try {
							Object value = rs.getObject(header);
							line.put(header, value == null ? "" : value);
						} catch (Exception ex) {
							ex.printStackTrace();
						}
					});
					ans.add(line);
				}

				// 最後にコミット
				// ここはQueryなのでCommitは不要だが更新時は必要になるのでお作法として記述
				agent.commit();
			} catch (SQLException ex) {
				ex.printStackTrace();
				// 失敗した場合はロールバック
				agent.rollback();
				throw ex;
			}
		}
		return ans;
	}
}
