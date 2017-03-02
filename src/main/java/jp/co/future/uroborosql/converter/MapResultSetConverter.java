package jp.co.future.uroborosql.converter;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

import jp.co.future.uroborosql.utils.ConvertUtils;

/**
 * 検索結果の1行をMap型に変換する変換器
 *
 * @author H.Sugimoto
 *
 */
public class MapResultSetConverter implements ResultSetConverter<Map<String, Object>> {
	private boolean toCamel = false;

	public MapResultSetConverter() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param toCamel 変換するMapのキーをCamelCase（先頭小文字）に変換する
	 */
	public MapResultSetConverter(final boolean toCamel) {
		this.toCamel = toCamel;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@Override
	public Map<String, Object> createRecord(final ResultSet rs) throws SQLException {
		ResultSetMetaData rsmd = rs.getMetaData();
		int columnCount = rsmd.getColumnCount();
		Map<String, Object> record = new LinkedHashMap<>(columnCount);
		for (int i = 1; i <= columnCount; i++) {
			record.put(toCamel ? ConvertUtils.toCamel(rsmd.getColumnLabel(i)) : rsmd.getColumnLabel(i), rs.getObject(i));
		}
		return record;
	}

}
