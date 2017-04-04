package jp.co.future.uroborosql.mapping.mapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * ResultSetの値をプロパティの型に変換するインターフェース
 *
 * @param <T> 変換対象の型
 *
 * @author ota
 */
public interface PropertyMapper<T> {
	/**
	 * 変換可能な型であるかを検証
	 *
	 * @param type 変換対象の型
	 * @return 変換可能な型
	 */
	boolean canAccept(Class<?> type);

	/**
	 * 処理前に変換可能な型であるかを検証
	 *
	 * @param type 変換対象の型
	 * @param rs ResultSet
	 * @param columnIndex 変換対象データのカラムIndex
	 * @param mapperManager 再起処理用パラメータ変換マネジャー
	 * @return 変換可能な型
	 * @throws SQLException SQL例外
	 */
	default boolean canAcceptTest(final JavaType type, final ResultSet rs, final int columnIndex,
			final PropertyMapperManager mapperManager) throws SQLException {
		return true;
	}

	/**
	 * プロパティの型に変換した値を取得します
	 *
	 * @param type 変換対象の型
	 * @param rs ResultSet
	 * @param columnIndex 変換対象データのカラムIndex
	 * @param mapperManager 再起処理用パラメータ変換マネジャー
	 * @return プロパティの型に変換した値
	 * @throws SQLException SQL例外
	 */
	T getValue(JavaType type, ResultSet rs, int columnIndex, PropertyMapperManager mapperManager) throws SQLException;
}
