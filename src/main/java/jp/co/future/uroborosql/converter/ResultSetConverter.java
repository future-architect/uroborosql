package jp.co.future.uroborosql.converter;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * 検索結果を{@literal <T>}で指定された型に変換するためのインタフェース
 *
 * @author H.Sugimoto
 *
 * @param <T> ResultSetの1行を変換した型
 */
@FunctionalInterface
public interface ResultSetConverter<T> {

	/**
	 * 検索結果の1行分のデータをT型のオブジェクトに変換する
	 *
	 * @param rs 検索結果
	 * @return 変換したオブジェクト
	 * @throws SQLException 例外発生時
	 */
	T createRecord(ResultSet rs) throws SQLException;
}
