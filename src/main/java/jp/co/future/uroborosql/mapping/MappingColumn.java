package jp.co.future.uroborosql.mapping;

/**
 * カラムマッピングインターフェース
 *
 * @author ota
 */
public interface MappingColumn {

	/**
	 * エンティティから値を取得
	 *
	 * @param entity エンティティ
	 * @return 取得した値
	 */
	Object getValue(Object entity);

	/**
	 * エンティティに値をセット
	 *
	 * @param entity エンティティ
	 * @param value 値
	 */
	void setValue(Object entity, Object value);

	/**
	 * カラム名取得
	 *
	 * @return カラム名
	 */
	String getName();

	/**
	 * キャメルケースカラム名取得
	 *
	 * @return キャメルケースカラム名
	 */
	String getCamelName();

	/**
	 * {@link JavaType}取得
	 *
	 * @return {@link JavaType}
	 */
	JavaType getJavaType();

	/**
	 * バージョン情報カラムかどうか
	 *
	 * @return バージョンカラムの場合は<code>true</code>
	 */
	boolean isVersion();
}
