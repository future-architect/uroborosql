/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SQL Query 実行インタフェース
 *
 * @author H.Sugimoto
 */
public interface SqlQuery extends SqlFluent<SqlQuery> {

	/**
	 * ResultSetの取得（終端処理）
	 *
	 * @return ResultSet
	 *
	 * @see SqlAgent#query(SqlContext)
	 * @throws SQLException SQL例外
	 */
	ResultSet resultSet() throws SQLException;

	/**
	 * 検索結果の取得（終端処理）
	 *
	 * @return 検索結果のリスト. MapのキーはSnakeCaseに変換される.
	 * @throws SQLException SQL例外
	 *
	 * @see SqlAgent#query(SqlContext, boolean)
	 */
	List<Map<String, Object>> collect() throws SQLException;

	/**
	 * 検索結果の取得（終端処理）
	 *
	 * @param caseFormat Mapのキーの変換書式
	 * @return 検索結果のリスト
	 * @throws SQLException SQL例外
	 *
	 * @see SqlAgent#query(SqlContext, boolean)
	 */
	List<Map<String, Object>> collect(CaseFormat caseFormat) throws SQLException;

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @param caseFormat Mapのキーの変換書式
	 * @return 検索結果の先頭行をMapに詰めたもの
	 * @throws DataNotFoundException 検索結果が０件の場合
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> first(CaseFormat caseFormat) throws DataNotFoundException, SQLException;

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @return 検索結果の先頭行をMapに詰めたもの. MapのキーはSnakeCase式に変換される.
	 * @throws DataNotFoundException 検索結果が０件の場合
	 * @throws SQLException SQL例外
	 */
	Map<String, Object> first() throws DataNotFoundException, SQLException;

	/**
	 * 検索結果をStreamとして取得（終端処理）
	 *
	 * @param converter ResultSetの各行を変換するための変換器
	 * @return 検索結果を順次取得するStream
	 * @throws SQLException SQL例外
	 */
	<T> Stream<T> stream(ResultSetConverter<T> converter) throws SQLException;

	/**
	 * 検索結果をMapのStreamとして取得（終端処理）
	 *
	 * @return 検索結果を順次取得するStream. MapのキーはCamel式に変換される.
	 * @throws SQLException SQL例外
	 */
	Stream<Map<String, Object>> stream() throws SQLException;
}
