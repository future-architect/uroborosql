package jp.co.future.uroborosql.sample;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.AuditLogEventSubscriber;

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
		// SQL実行時に行う各種フィルタ処理を管理するクラスを設定
		config = UroboroSQL.builder(url, user, password).build();
		config.getEventListenerHolder().addEventSubscriber(new AuditLogEventSubscriber());

		// JNDIなどのDataSourceを利用する場合は、DataSource用のConnectionSupplierを指定してください
		// Connectionの管理をDataSource側に委譲します
		// config = SqlConfig.getConfig(dataSourceName);
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
	public List<Map<String, Object>> query(final String sqlName, final Map<String, Object> params) throws SQLException {

		List<Map<String, Object>> ans = new ArrayList<>();

		// SqlAgent（SQL実行エンジン）を設定オブジェクトから取得
		try (var agent = config.agent()) {
			// SQLの実行
			try (var rs = agent.query(sqlName).paramMap(params).resultSet()) {
				// 実行結果はResultSetで返ってくるので、値を取得
				List<String> headers = new ArrayList<>();
				var rsmd = rs.getMetaData();
				var count = rsmd.getColumnCount();
				for (var i = 1; i <= count; i++) {
					var label = rsmd.getColumnLabel(i);
					headers.add(label);
				}

				while (rs.next()) {
					Map<String, Object> line = new LinkedHashMap<>();
					headers.forEach(header -> {
						try {
							var value = rs.getObject(header);
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
