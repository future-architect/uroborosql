package jp.co.future.uroborosql.mapping;

import java.sql.JDBCType;
import java.util.ArrayList;
import java.util.List;

import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * テーブルメタ情報クラス
 *
 * @author ota
 */
public class TableMetadataImpl implements TableMetadata {
	/**
	 * カラム情報クラス
	 */
	public static class Column implements TableMetadata.Column {
		private String columnName;
		private String camelName;
		private JDBCType dataType;
		private Integer keySeq = null;

		/**
		 * コンストラクタ
		 */
		public Column() {
		}

		/**
		 * コンストラクタ
		 *
		 * @param columnName カラム名
		 * @param dataType タイプ
		 */
		public Column(final String columnName, final JDBCType dataType) {
			this.columnName = columnName;
			this.dataType = dataType;
		}

		/**
		 * コンストラクタ
		 *
		 * @param columnName カラム名
		 * @param dataType タイプ
		 */
		public Column(final String columnName, final int dataType) {
			this(columnName, JDBCType.valueOf(dataType));
		}

		@Override
		public String getColumnName() {
			return this.columnName;
		}

		@Override
		public String getCamelColumnName() {
			return this.camelName != null ? this.camelName : (this.camelName = CaseFormat.CAMEL_CASE.convert(getColumnName()));
		}

		/**
		 * カラム名設定
		 *
		 * @param columnName カラム名
		 */
		public void setColumnName(final String columnName) {
			this.columnName = columnName;
			this.camelName = null;
		}

		@Override
		public JDBCType getDataType() {
			return this.dataType;
		}

		/**
		 * タイプ設定
		 *
		 * @param dataType タイプ
		 */
		public void setDataType(final JDBCType dataType) {
			this.dataType = dataType;
		}

		@Override
		public int getKeySeq() {
			return this.keySeq;
		}

		@Override
		public boolean isKey() {
			return this.keySeq != null;
		}

		/**
		 * 主キー内の連番設定
		 *
		 * @param keySeq 主キー内の連番
		 */
		public void setKeySeq(final int keySeq) {
			this.keySeq = keySeq;
		}

	}

	private String tableName;
	private String schema;
	private final List<TableMetadata.Column> columns = new ArrayList<>();

	/**
	 * コンストラクタ
	 */
	public TableMetadataImpl() {
	}

	/**
	 * コンストラクタ
	 *
	 * @param schema スキーマ名
	 * @param tableName テーブル名
	 */
	public TableMetadataImpl(final String schema, final String tableName) {
		this.tableName = tableName;
		this.schema = schema;
	}

	/**
	 * カラム情報追加
	 *
	 * @param column カラム情報
	 */
	public void addColumn(final TableMetadata.Column column) {
		this.columns.add(column);
	}

	@Override
	public String getTableName() {
		return this.tableName;
	}

	/**
	 * テーブル名設定
	 *
	 * @param tableName テーブル名
	 */
	public void setTableName(final String tableName) {
		this.tableName = tableName;
	}

	@Override
	public String getSchema() {
		return this.schema;
	}

	/**
	 * スキーマ名設定
	 *
	 * @param schema スキーマ名
	 */
	public void setSchema(final String schema) {
		this.schema = schema;
	}

	@Override
	public List<? extends TableMetadata.Column> getColumns() {
		return this.columns;
	}

}