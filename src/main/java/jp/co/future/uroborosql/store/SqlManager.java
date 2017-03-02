package jp.co.future.uroborosql.store;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;

/**
 * SQL管理インターフェース
 *
 * @author H.Sugimoto
 */
public interface SqlManager {

	/**
	 * 初期化<br>
	 *
	 * @throws URISyntaxException リソースへのアクセスに失敗した場合
	 * @throws IOException IO例外
	 */
	void initialize() throws IOException, URISyntaxException;

	/**
	 * SQL文取得<br>
	 * @param sqlPath ルートパスからの相対パス
	 * @return SQL文
	 */
	String getSql(String sqlPath);

	/**
	 * SQLが存在するかどうかを判定する
	 *
	 * @param sqlPath ルートパスからの相対パス
	 * @return 存在する場合は<code>true</code>
	 */
	boolean existSql(String sqlPath);

	/**
	 * ロードしたSQLのパス一覧を取得する
	 * @return ロードしたSQLパス一覧
	 */
	List<String> getSqlPathList();

}