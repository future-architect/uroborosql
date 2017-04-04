package jp.co.future.uroborosql.mapping;

/**
 * テーブル情報
 *
 * @author ota
 */
public interface Table {
	/**
	 * テーブル名取得
	 *
	 * @return テーブル名
	 */
	String getName();

	/**
	 * スキーマ名取得
	 *
	 * @return スキーマ名
	 */
	String getSchema();
}