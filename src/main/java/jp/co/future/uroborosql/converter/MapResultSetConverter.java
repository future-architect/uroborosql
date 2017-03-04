package jp.co.future.uroborosql.converter;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * 検索結果の1行をMap型に変換する変換器
 *
 * @author H.Sugimoto
 *
 */
public class MapResultSetConverter implements ResultSetConverter<Map<String, Object>> {
	private CaseFormat caseFormat = CaseFormat.SnakeCase;

	public MapResultSetConverter() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param caseFormat 変換するMapのキーの書式
	 */
	public MapResultSetConverter(final CaseFormat caseFormat) {
		this.caseFormat = caseFormat;
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
			record.put(caseFormat.convert(rsmd.getColumnLabel(i)), rs.getObject(i));
		}
		return record;
	}

}
