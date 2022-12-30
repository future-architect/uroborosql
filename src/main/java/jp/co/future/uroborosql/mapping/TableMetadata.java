/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.sql.SQLException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.connection.ConnectionManager;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * テーブルメタ情報
 *
 * @author ota
 */
public interface TableMetadata {
	/** QuoteStringで囲む必要があるテーブルのパターン(大文字小文字混在、もしくは英数字以外を含む) */
	Pattern TABLE_NAME_PATTERN = Pattern.compile("(?=.*[a-z])(?=.*[A-Z]).*|.*[^A-Za-z0-9_].*");

	/** remark中の改行文字を除外するためのパターン */
	Pattern NEWLINE_CHARS_PATTERN = Pattern.compile("\r\n|\r|\n");

	/** SQL文構築時、スキーマ名で修飾されたテーブル名を使用するかどうか. */
	boolean USE_QUALIFIED_TABLE_NAME = Boolean
			.valueOf(System.getProperty("uroborosql.use.qualified.table.name", "true"));

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
		 * カラム識別名取得
		 *
		 * @return カラム識別名
		 */
		default String getColumnIdentifier() {
			return getColumnName();
		}

		/**
		 * CamelCase カラム名取得
		 *
		 * @return CamelCase カラム名
		 */
		default String getCamelColumnName() {
			return CaseFormat.CAMEL_CASE.convert(getColumnName());
		}

		/**
		 * カラム型取得
		 *
		 * @return カラム型を表す値
		 */
		int getDataType();

		/**
		 * カラムサイズ取得
		 *
		 * @return カラムサイズ
		 */
		int getColumnSize();

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

		/**
		 * コメント文字列取得
		 *
		 * @return コメント文字列
		 */
		String getRemarks();

		/**
		 * カラムのデフォルト値（文字列）取得
		 *
		 * @return カラムのデフォルト値（文字列）
		 */
		String getColumnDefault();

		/**
		 * NULLが許可されるかどうかを取得
		 *
		 * @return NULLが許可される場合<code>true</code>
		 */
		boolean isNullable();

		/**
		 * 自動インクリメントされるかどうかを取得
		 *
		 * @return 自動インクリメントの場合<code>true</code>
		 */
		boolean isAutoincrement();

		/**
		 * バージョンカラムかどうかを取得
		 *
		 * @return バージョンカラムの場合<code>true</code>
		 */
		boolean isVersion();

		/**
		 * 楽観ロック方法を取得
		 *
		 * @return 楽観ロック方法
		 */
		Class<? extends OptimisticLockSupplier> getOptimisticLockType();

		/**
		 * 列のインデックス(1から始まる)を取得
		 *
		 * @return 列のインデックス(1から始まる)
		 */
		int getOrdinalPosition();
	}

	/**
	 * テーブル名取得
	 *
	 * @return テーブル名
	 */
	String getTableName();

	/**
	 * テーブル名設定
	 *
	 * @param tableName テーブル名
	 */
	void setTableName(String tableName);

	/**
	 * スキーマ名取得
	 *
	 * @return スキーマ名
	 */
	String getSchema();

	/**
	 * スキーマ名設定
	 *
	 * @param schema スキーマ名
	 */
	void setSchema(String schema);

	/**
	 * SQL識別子を引用するのに使用する文字列を取得
	 *
	 * @return SQL識別子を引用するのに使用する文字列
	 */
	default String getIdentifierQuoteString() {
		return "\"";
	}

	/**
	 * SQL識別子を引用するのに使用する文字列を設定
	 *
	 * @param identifierQuoteString SQL識別子を引用するのに使用する文字列
	 */
	default void setIdentifierQuoteString(final String identifierQuoteString) {
		//noop
	}

	/**
	 * テーブル識別名の取得
	 *
	 * @return テーブル識別名
	 */
	default String getTableIdentifier() {
		var identifierQuoteString = getIdentifierQuoteString();
		if (StringUtils.isEmpty(identifierQuoteString)) {
			identifierQuoteString = "";
		}
		if (!USE_QUALIFIED_TABLE_NAME || StringUtils.isEmpty(getSchema())) {
			return identifierQuoteString + getTableName() + identifierQuoteString;
		} else {
			return identifierQuoteString + getSchema() + identifierQuoteString + "." + identifierQuoteString
					+ getTableName() + identifierQuoteString;
		}
	}

	/**
	 * カラム取得
	 *
	 * @param camelColumnName カラムのキャメル名
	 * @return カラム
	 * @exception UroborosqlRuntimeException 指定したキャメルカラム名に該当するカラムが見つからなかった場合
	 */
	TableMetadata.Column getColumn(String camelColumnName);

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
	static TableMetadata createTableEntityMetadata(final ConnectionManager connectionManager, final Table table)
			throws SQLException {

		var connection = connectionManager.getConnection();
		var metaData = connection.getMetaData();

		var schema = StringUtils.isNotEmpty(table.getSchema()) ? table.getSchema() : connection.getSchema();
		var tableName = table.getName();
		var identifierQuoteString = metaData.getIdentifierQuoteString();

		var entityMetadata = new TableMetadataImpl();

		Map<String, TableMetadataImpl.Column> columns = new HashMap<>();
		String actualSchema = schema;

		int tryCount = 0;//1回目：case変換なしで検索, 2回目：case変換後で検索, 3回目:schema指定なしで検索
		while (tryCount < 3 && columns.isEmpty()) {
			tryCount++;
			if (tryCount == 2) {
				// case 変換
				if (metaData.storesLowerCaseIdentifiers()) {
					tableName = tableName.toLowerCase();
				} else if (metaData.storesUpperCaseIdentifiers()) {
					tableName = tableName.toUpperCase();
				}
				if (StringUtils.isNotEmpty(schema)) {
					if (metaData.storesLowerCaseIdentifiers()) {
						schema = schema.toLowerCase();
					} else if (metaData.storesUpperCaseIdentifiers()) {
						schema = schema.toUpperCase();
					}
				}
			} else if (tryCount == 3) {
				// スキーマ指定なし
				schema = null;
			}
			String versionColumnName = null;
			Class<? extends OptimisticLockSupplier> optimisticLockType = null;
			if (table instanceof MetaTable) {
				var metaTable = (MetaTable) table;
				versionColumnName = metaTable.getVersionColumnName();
				optimisticLockType = metaTable.getOptimisticLockType();
			}

			try (var rs = metaData.getColumns(null, StringUtils.isEmpty(schema) ? "%" : schema, tableName, "%")) {
				while (rs.next()) {
					var columnName = rs.getString("COLUMN_NAME");
					actualSchema = rs.getString("TABLE_SCHEM");
					var sqlType = rs.getInt("DATA_TYPE");
					// If Types.DISTINCT like SQL DOMAIN, then get Source Date Type of SQL-DOMAIN
					if (sqlType == java.sql.Types.DISTINCT) {
						sqlType = rs.getInt("SOURCE_DATA_TYPE");
					}
					var remarks = rs.getString("REMARKS");
					if (remarks != null) {
						remarks = NEWLINE_CHARS_PATTERN.matcher(remarks).replaceAll(" ");
					}
					var columnDefault = rs.getString("COLUMN_DEF");
					var isNullable = rs.getString("IS_NULLABLE");
					var isAutoincrement = rs.getString("IS_AUTOINCREMENT");
					var isVersion = columnName.equalsIgnoreCase(versionColumnName);
					var ordinalPosition = rs.getInt("ORDINAL_POSITION");

					var columnSize = rs.getInt("COLUMN_SIZE");

					var column = new TableMetadataImpl.Column(columnName,
							sqlType,
							columnSize,
							remarks,
							columnDefault,
							isNullable,
							isAutoincrement,
							isVersion,
							isVersion ? optimisticLockType : null,
							ordinalPosition,
							identifierQuoteString);
					entityMetadata.addColumn(column);
					columns.put(column.getColumnName(), column);
				}
			}
		}
		entityMetadata.setSchema(StringUtils.isNotEmpty(actualSchema) ? actualSchema : schema);
		entityMetadata.setTableName(tableName);
		if (TABLE_NAME_PATTERN.matcher(tableName).matches()) {
			entityMetadata.setIdentifierQuoteString(identifierQuoteString);
		} else {
			entityMetadata.setIdentifierQuoteString("");
		}
		try (var rs = metaData.getPrimaryKeys(null, entityMetadata.getSchema(), entityMetadata.getTableName())) {
			while (rs.next()) {
				var columnName = rs.getString("COLUMN_NAME");
				var keySeq = rs.getShort("KEY_SEQ");
				columns.get(columnName).setKeySeq(keySeq);
			}
		}
		return entityMetadata;
	}
}
