/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.converter;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * 検索結果の1行をMap型に変換する変換器
 *
 * @author H.Sugimoto
 *
 */
public class MapResultSetConverter implements ResultSetConverter<Map<String, Object>> {
	private final PropertyMapperManager mapperManager;
	private final SqlConfig sqlConfig;
	private final CaseFormat caseFormat;
	private String[] columnLabels;
	private JavaType[] javaTypes;
	private int columnCount;

	/**
	 * コンストラクタ
	 *
	 * @param sqlConfig SQL設定クラス
	 */
	public MapResultSetConverter(final SqlConfig sqlConfig) {
		this(sqlConfig, CaseFormat.UPPER_SNAKE_CASE);
	}

	/**
	 * コンストラクタ
	 *
	 * @param sqlConfig SQL設定クラス
	 * @param caseFormat 変換するMapのキーの書式
	 */
	public MapResultSetConverter(final SqlConfig sqlConfig, final CaseFormat caseFormat) {
		this.sqlConfig = sqlConfig;
		this.caseFormat = caseFormat;
		this.mapperManager = new PropertyMapperManager(sqlConfig.getClock());
	}

	/**
	 * コンストラクタ
	 *
	 * @param sqlConfig SQL設定クラス
	 * @param caseFormat 変換するMapのキーの書式
	 * @param mapperManager コピー元のパラメータ変換クラス
	 */
	public MapResultSetConverter(final SqlConfig sqlConfig, final CaseFormat caseFormat,
			final PropertyMapperManager mapperManager) {
		this.sqlConfig = sqlConfig;
		this.caseFormat = caseFormat;
		this.mapperManager = new PropertyMapperManager(mapperManager, sqlConfig.getClock());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@Override
	public Map<String, Object> createRecord(final ResultSet rs) throws SQLException {
		if (this.javaTypes == null) {
			var rsmd = rs.getMetaData();
			this.columnCount = rsmd.getColumnCount();
			this.columnLabels = new String[columnCount + 1];
			this.javaTypes = new JavaType[columnCount + 1];
			for (var i = 1; i <= columnCount; i++) {
				this.columnLabels[i] = caseFormat.convert(rsmd.getColumnLabel(i));
				this.javaTypes[i] = this.sqlConfig.getDialect().getJavaType(rsmd.getColumnType(i),
						rsmd.getColumnTypeName(i));
			}
		}

		// resizeが発生しないよう、初期loadFactorで溢れないサイズを指定する。
		// MapのloadFactorはデフォルト0.75(3/4)なので 4/3 を掛けている。そのうえで切り捨てが発生してもキャパシティを越えないよう +1 している。
		var record = new LinkedHashMap<String, Object>(columnCount * 4 / 3 + 1);

		for (var i = 1; i <= columnCount; i++) {
			record.put(this.columnLabels[i], this.mapperManager.getValue(this.javaTypes[i], rs, i));
		}
		return record;
	}
}
