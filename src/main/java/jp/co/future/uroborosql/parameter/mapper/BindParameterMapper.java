package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

/**
 * パラメータをJDBCが受け入れられる型に変換するインターフェース
 *
 * @param <T> 変換対象の型
 *
 * @author ota
 */
public interface BindParameterMapper<T> {
	/**
	 * 変換対象の型
	 *
	 * @return 変換対象の型
	 */
	Class<T> targetType();

	/**
	 * JDBCが受け入れ可能な型に変換します
	 *
	 * @param original 変換対象データ
	 * @param connection パラメータクラス生成用Connection（パラメータクラス生成以外の用途では利用しないでください。{@link JdbcParameterFactory}のメソッドも利用してください）
	 * @param parameterMapperManager 再起処理用パラメータ変換マネジャー
	 * @return JDBCが受け入れ可能な型に変換した値
	 */
	Object toJdbc(T original, Connection connection, BindParameterMapperManager parameterMapperManager);
}
