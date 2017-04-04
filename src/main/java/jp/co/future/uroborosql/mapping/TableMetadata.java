package jp.co.future.uroborosql.mapping;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.StringUtils;

/**
 * テーブルメタ情報
 *
 * @author ota
 */
public interface TableMetadata {
	/**
	 * カラム情報
	 */
	interface Column {
		/**
		 * カラム名取得
		 *
		 * @return カラム名
		 */
		String getColumnName();

		/**
		 * CamelCase カラム名取得
		 *
		 * @return CamelCase カラム名
		 */
		default String getCamelColumnName() {
			return CaseFormat.CamelCase.convert(getColumnName());
		}

		/**
		 * カラム型取得
		 *
		 * @return カラム型
		 */
		JDBCType getDataType();

		/**
		 * 主キー内の連番取得 (値1は主キーの最初の列、値2は主キーの2番目の列を表す)。
		 *
		 * @return 主キー内の連番
		 */
		int getKeySeq();

		/**
		 * 主キー判定
		 *
		 * @return true:主キー
		 */
		boolean isKey();
	}

	/**
	 * テーブル名取得
	 *
	 * @return テーブル名
	 */
	String getTableName();

	/**
	 * スキーマ名取得
	 *
	 * @return スキーマ名
	 */
	String getSchema();

	/**
	 * テーブル識別名の取得
	 *
	 * @return テーブル識別名
	 */
	default String getTableIdentifier() {
		if (StringUtils.isEmpty(getSchema())) {
			return getTableName();
		} else {
			return getSchema() + "." + getTableName();
		}
	}

	/**
	 * カラム取得
	 *
	 * @return カラム
	 */
	List<? extends TableMetadata.Column> getColumns();

	/**
	 * Keyカラム取得
	 *
	 * @return カラム
	 */
	default List<? extends TableMetadata.Column> getKeyColumns() {
		return getColumns().stream()
				.filter(TableMetadata.Column::isKey)
				.sorted(Comparator.comparingInt(TableMetadata.Column::getKeySeq))
				.collect(Collectors.toList());
	}

	/**
	 * テーブル名から、DatabaseMetaDataを利用して、TableMetadataの生成
	 *
	 * @param connectionManager コネクションマネージャー
	 * @param table テーブル情報
	 * @return TableMetadata
	 * @throws SQLException SQL例外
	 */
	@SuppressWarnings("resource")
	static TableMetadata createTableEntityMetadata(final ConnectionManager connectionManager, final Table table) throws SQLException {
		TableMetadataImpl entityMetadata = new TableMetadataImpl(table);

		String schemaPattern = StringUtils.isEmpty(table.getSchema()) ? "%" : table.getSchema();

		Connection connection = connectionManager.getConnection();
		DatabaseMetaData metaData = connection.getMetaData();
		Map<String, TableMetadataImpl.Column> columns = new HashMap<>();
		try (ResultSet rs = metaData.getColumns(null, schemaPattern, table.getName(), "%")) {
			while (rs.next()) {
				TableMetadataImpl.Column column = new TableMetadataImpl.Column(
						rs.getString("COLUMN_NAME"), rs.getInt("DATA_TYPE"));
				entityMetadata.addColumn(column);
				columns.put(column.getColumnName(), column);
			}
		}
		try (ResultSet rs = metaData.getPrimaryKeys(null, schemaPattern, table.getName())) {
			while (rs.next()) {
				String columnName = rs.getString("COLUMN_NAME");
				short keySeq = rs.getShort("KEY_SEQ");
				columns.get(columnName).setKeySeq(keySeq);
			}
		}
		return entityMetadata;
	}
}
