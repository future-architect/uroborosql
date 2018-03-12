package jp.co.future.uroborosql.converter;

import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.JDBCType;
import java.sql.NClob;
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
	private CaseFormat caseFormat;

	/**
	 * コンストラクタ
	 */
	public MapResultSetConverter() {
		this(CaseFormat.UPPER_SNAKE_CASE);
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
			record.put(caseFormat.convert(rsmd.getColumnLabel(i)), getValue(rs, rsmd, i));
		}
		return record;
	}

	private Object getValue(final ResultSet rs, final ResultSetMetaData rsmd, final int columnIndex)
			throws SQLException {
		JDBCType type = JDBCType.valueOf(rsmd.getColumnType(columnIndex));
		switch (type) {
		case CLOB:
			Clob clob = rs.getClob(columnIndex);
			return clob.getSubString(1, (int) clob.length());
		case NCLOB:
			NClob nclob = rs.getNClob(columnIndex);
			return nclob.getSubString(1, (int) nclob.length());
		case BLOB:
			Blob blob = rs.getBlob(columnIndex);
			return blob.getBytes(1, (int) blob.length());
		case ARRAY:
			Array arr = rs.getArray(columnIndex);
			return arr.getArray();
		default:
			return rs.getObject(columnIndex);
		}

	}

}
