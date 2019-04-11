/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.sql.JDBCType;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
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
		private String identifier;
		private JDBCType dataType;
		private Integer keySeq = null;
		private final String identifierQuoteString;
		private final String remarks;
		private final boolean nullable;
		private final int ordinalPosition;

		/**
		 * コンストラクタ
		 *
		 * @param columnName カラム名
		 * @param dataType データタイプ
		 * @param remarks コメント文字列
		 * @param nullable NULL可かどうか
		 * @param ordinalPosition 列インデックス
		 * @param identifierQuoteString SQL識別子を引用するのに使用する文字列
		 */
		public Column(final String columnName, final JDBCType dataType, final String remarks, final String nullable,
				final int ordinalPosition, final String identifierQuoteString) {
			this.columnName = columnName;
			this.dataType = dataType;
			this.remarks = remarks;
			this.nullable = "YES".equalsIgnoreCase(nullable);
			this.ordinalPosition = ordinalPosition;

			if (StringUtils.isEmpty(identifierQuoteString)) {
				this.identifierQuoteString = "";
			} else {
				this.identifierQuoteString = identifierQuoteString;
			}
		}

		/**
		 * コンストラクタ
		 *
		 * @param columnName カラム名
		 * @param dataType データタイプ
		 * @param remarks コメント文字列
		 * @param nullable NULL可かどうか
		 * @param ordinalPosition 列インデックス
		 * @param identifierQuoteString SQL識別子を引用するのに使用する文字列
		 */
		public Column(final String columnName, final int dataType, final String remarks, final String nullable,
				final int ordinalPosition, final String identifierQuoteString) {
			this(columnName, JDBCType.valueOf(dataType), remarks, nullable, ordinalPosition, identifierQuoteString);
		}

		/**
		 * コンストラクタ
		 *
		 * @param columnName カラム名
		 * @param dataType データタイプ
		 * @param remarks コメント文字列
		 * @param nullable NULL可かどうか
		 * @param ordinalPosition 列インデックス
		 */
		@Deprecated
		public Column(final String columnName, final JDBCType dataType, final String remarks, final String nullable,
				final int ordinalPosition) {
			this(columnName, dataType, remarks, nullable, ordinalPosition, null);
		}

		/**
		 * コンストラクタ
		 *
		 * @param columnName カラム名
		 * @param dataType データタイプ
		 * @param remarks コメント文字列
		 * @param nullable NULL可かどうか
		 * @param ordinalPosition 列インデックス
		 */
		@Deprecated
		public Column(final String columnName, final int dataType, final String remarks, final String nullable,
				final int ordinalPosition) {
			this(columnName, dataType, remarks, nullable, ordinalPosition, null);
		}

		@Override
		public String getColumnName() {
			return this.columnName;
		}

		@Override
		public String getCamelColumnName() {
			return this.camelName != null ? this.camelName
					: (this.camelName = CaseFormat.CAMEL_CASE
							.convert(getColumnName()));
		}

		/**
		 * カラム名設定
		 *
		 * @param columnName カラム名
		 */
		public void setColumnName(final String columnName) {
			this.columnName = columnName;
			this.camelName = null;
			this.identifier = null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#getDataType()
		 */
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

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#getColumnIdentifier()
		 */
		@Override
		public String getColumnIdentifier() {
			return this.identifier != null ? this.identifier
					: (this.identifier = identifierQuoteString + getColumnName() + identifierQuoteString);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#getKeySeq()
		 */
		@Override
		public int getKeySeq() {
			return this.keySeq;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#isKey()
		 */
		@Override
		public boolean isKey() {
			return this.keySeq != null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#getRemarks()
		 */
		@Override
		public String getRemarks() {
			return this.remarks;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#isNullable()
		 */
		@Override
		public boolean isNullable() {
			return this.nullable;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.mapping.TableMetadata.Column#getOrdinalPosition()
		 */
		@Override
		public int getOrdinalPosition() {
			return this.ordinalPosition;
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
	private String identifierQuoteString = "\"";
	private String identifier;
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

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#getTableName()
	 */
	@Override
	public String getTableName() {
		return this.tableName;
	}

	/**
	 * テーブル名設定
	 *
	 * @param tableName テーブル名
	 */
	@Override
	public void setTableName(final String tableName) {
		this.tableName = tableName;
		this.identifier = null;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#getSchema()
	 */
	@Override
	public String getSchema() {
		return this.schema;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#setSchema(java.lang.String)
	 */
	@Override
	public void setSchema(final String schema) {
		this.schema = schema;
		this.identifier = null;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#getIdentifierQuoteString()
	 */
	@Override
	public String getIdentifierQuoteString() {
		return identifierQuoteString;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#setIdentifierQuoteString(java.lang.String)
	 */
	@Override
	public void setIdentifierQuoteString(final String identifierQuoteString) {
		this.identifierQuoteString = identifierQuoteString;
		this.identifier = null;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#getTableIdentifier()
	 */
	@Override
	public String getTableIdentifier() {
		return this.identifier != null ? this.identifier : (this.identifier = TableMetadata.super.getTableIdentifier());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#getColumns()
	 */
	@Override
	public List<? extends TableMetadata.Column> getColumns() {
		return this.columns;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.mapping.TableMetadata#getColumn(java.lang.String)
	 */
	@Override
	public TableMetadata.Column getColumn(final String camelColumnName) {
		return this.columns.stream().filter(c -> c.getCamelColumnName().equals(camelColumnName)).findFirst()
				.orElseThrow(() -> new UroborosqlRuntimeException("No such column found. col=" + camelColumnName));
	}
}