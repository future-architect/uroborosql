/**
 *
 */
package jp.co.future.uroborosql.fluent;

import java.sql.ResultSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
	 * @see SqlAgent#query(SqlContext)
	 */
	ResultSet resultSet();

	/**
	 * 検索結果の取得（終端処理）
	 *
	 * @return 検索結果のリスト. MapのキーはSnakeCaseに変換される.
	 * @see SqlAgent#query(String)
	 */
	List<Map<String, Object>> collect();

	/**
	 * 検索結果の取得（終端処理）
	 *
	 * @param caseFormat Mapのキーの変換書式
	 * @return 検索結果のリスト
	 * @see SqlAgent#query(String)
	 */
	List<Map<String, Object>> collect(CaseFormat caseFormat);

	/**
	 * 検索結果の取得（終端処理）
	 *
	 * @param <T>  Entityの型
	 * @param type 受け取りたいEntityの型
	 * @return 検索結果のリスト
	 * @see SqlAgent#query(String)
	 */
	<T> List<T> collect(Class<T> type);

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @param caseFormat Mapのキーの変換書式
	 * @return 検索結果の先頭行をMapに詰めたもの
	 * @throws DataNotFoundException 検索結果が０件の場合
	 */
	Map<String, Object> first(CaseFormat caseFormat);

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @return 検索結果の先頭行をMapに詰めたもの. MapのキーはSnakeCase式に変換される.
	 * @throws DataNotFoundException 検索結果が０件の場合
	 */
	Map<String, Object> first();

	/**
	 * 検索結果の先頭行をEntityとして取得（終端処理）
	 *
	 * @param <T>  Entityの型
	 * @param type 受け取りたいEntityの型
	 * @return 検索結果の先頭行のEntity
	 * @throws DataNotFoundException 検索結果が０件の場合
	 */
	<T> T first(Class<T> type);

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @param caseFormat Mapのキーの変換書式
	 * @return 検索結果の先頭行をMapに詰めたものをOptionalとして返却
	 */
	Optional<Map<String, Object>> findFirst(CaseFormat caseFormat);

	/**
	 * 検索結果の先頭行を取得（終端処理）
	 *
	 * @return 検索結果の先頭行をMapに詰めたものをOptionalとして返却. MapのキーはSnakeCase式に変換される.
	 */
	Optional<Map<String, Object>> findFirst();

	/**
	 * 検索結果の先頭行をEntityとして取得（終端処理）
	 *
	 * @param <T>  Entityの型
	 * @param type 受け取りたいEntityの型
	 * @return 検索結果の先頭行のEntityをOptionalとして返却
	 */
	<T> Optional<T> findFirst(Class<T> type);

	/**
	 * 検索結果をStreamとして取得（終端処理）
	 *
	 * @param <T>       Streamの型
	 * @param converter ResultSetの各行を変換するための変換器
	 * @return 検索結果を順次取得するStream
	 */
	<T> Stream<T> stream(ResultSetConverter<T> converter);

	/**
	 * 検索結果をMapのStreamとして取得（終端処理）
	 *
	 * @return 検索結果を順次取得するStream. MapのキーはCamel式に変換される.
	 */
	Stream<Map<String, Object>> stream();

	/**
	 * 検索結果をMapのStreamとして取得（終端処理）
	 *
	 * @param caseFormat Mapのキーの変換書式
	 * @return 検索結果を順次取得するStream. MapのキーはCamel式に変換される.
	 */
	Stream<Map<String, Object>> stream(CaseFormat caseFormat);

	/**
	 * 検索結果をEntityのStreamとして取得（終端処理）
	 *
	 * @param <T>       Streamの型
	 * @param type 受け取りたいEntityの型
	 * @return 検索結果を順次取得するStream.
	 */
	<T> Stream<T> stream(Class<T> type);
}
