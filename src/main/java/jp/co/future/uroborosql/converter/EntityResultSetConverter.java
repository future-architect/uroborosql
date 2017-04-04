package jp.co.future.uroborosql.converter;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Objects;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.JavaType;
import jp.co.future.uroborosql.mapping.MappingColumn;
import jp.co.future.uroborosql.mapping.MappingUtils;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 検索結果の1行を任意型に変換する変換器
 *
 * @param <E> エンティティ型
 * @author ota
 *
 */
public class EntityResultSetConverter<E> implements ResultSetConverter<E> {
	/** ロガー */
	private static final Logger LOG = LoggerFactory.getLogger(EntityResultSetConverter.class);

	private class CacheMetaData {
		private final ResultSet cacheResultSet;
		private final int columnCount;
		private final String[] columnLabels;

		CacheMetaData(final ResultSet rs) throws SQLException {
			this.cacheResultSet = rs;
			ResultSetMetaData rsmd = rs.getMetaData();
			this.columnCount = rsmd.getColumnCount();

			this.columnLabels = new String[this.columnCount];
			for (int i = 1; i <= this.columnCount; i++) {
				this.columnLabels[i - 1] = rsmd.getColumnLabel(i);
			}
		}

		boolean equalResultSet(final ResultSet rs) {
			return Objects.equals(this.cacheResultSet, rs);
		}

		void bindValue(final E rec, final ResultSet rs, final MappingColumn column) throws SQLException {
			for (int i = 0; i < this.columnLabels.length; i++) {
				if (this.columnLabels[i].equals(column.getName())) {
					column.setValue(rec, getValue(column.getJavaType(), rs, i + 1));
					return;
				}
			}
		}
	}

	private final PropertyMapperManager mapperManager;
	private final Constructor<E> constructor;
	private final MappingColumn[] columns;
	private final ThreadLocal<CacheMetaData> cacheMetaData = new ThreadLocal<>();

	/**
	 * コンストラクタ
	 *
	 * @param entityType エンティティタイプ
	 * @param mapperManager PropertyMapperManager
	 */
	@SuppressWarnings({ "unchecked" })
	public EntityResultSetConverter(final Class<? extends E> entityType, final PropertyMapperManager mapperManager) {
		this.mapperManager = mapperManager;
		try {
			this.constructor = (Constructor<E>) entityType.getConstructor();
		} catch (NoSuchMethodException e) {
			throw new UroborosqlRuntimeException(e);
		}

		this.columns = MappingUtils.getMappingColumns(entityType);
	}

	Object getValue(final JavaType type, final ResultSet rs, final int columnIndex) throws SQLException {
		return this.mapperManager.getValue(type, rs, columnIndex);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.converter.ResultSetConverter#createRecord(java.sql.ResultSet)
	 */
	@Override
	public E createRecord(final ResultSet rs) throws SQLException {
		try {
			CacheMetaData metaData = this.cacheMetaData.get();
			if (metaData == null || !metaData.equalResultSet(rs)) {
				metaData = new CacheMetaData(rs);
				this.cacheMetaData.set(metaData);
			}
			E rec = this.constructor.newInstance();
			for (MappingColumn column : this.columns) {
				metaData.bindValue(rec, rs, column);
			}

			return rec;
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
			LOG.error("Error!!", e);
			throw new UroborosqlRuntimeException(e);
		} catch (SQLException | RuntimeException | Error e) {
			LOG.error("Error!!", e);
			throw e;
		}
	}
}
