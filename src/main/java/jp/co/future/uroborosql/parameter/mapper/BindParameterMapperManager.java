package jp.co.future.uroborosql.parameter.mapper;

import java.math.BigDecimal;
import java.sql.Connection;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * パラメータ変換クラス<br>
 * JDBCに渡すパラメータを調整する
 *
 * @author ota
 */
public final class BindParameterMapperManager {
	/** バインドパラメータMapperのリスト */
	private final List<BindParameterMapper<?>> mappers;

	/** デフォルトMapper */
	private static final BindParameterMapper<?>[] DEFAULT_MAPPERS = {
			new DateParameterMapper(),
			new DateTimeApiParameterMapper(),
			new BigIntegerParameterMapper(),
			new EnumParameterMapper(),
			new OptionalParameterMapper(),
			new OptionalIntParameterMapper(),
			new OptionalLongParameterMapper(),
			new OptionalDoubleParameterMapper(),
	};

	/**
	 * コンストラクタ
	 */
	public BindParameterMapperManager() {
		mappers = new CopyOnWriteArrayList<>();
	}

	/**
	 * コピーコンストラクタ
	 * @param parameterMapperManager コピー元のパラメータ変換クラス
	 */
	public BindParameterMapperManager(final BindParameterMapperManager parameterMapperManager) {
		mappers = new CopyOnWriteArrayList<>(parameterMapperManager.mappers);
	}

	/**
	 * {@link BindParameterMapper}を追加
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	public void addMapper(final BindParameterMapper<?> parameterMapper) {
		mappers.add(parameterMapper);
	}

	/**
	 * {@link BindParameterMapper}をremove
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	public void removeMapper(final BindParameterMapper<?> parameterMapper) {
		mappers.remove(parameterMapper);
	}

	/**
	 * 指定されたパラメータをPreparedStatementにセットするパラメータに変換
	 *
	 * @param object 指定パラメータ
	 * @param connection パラメータ生成用にConnectionを渡す
	 * @return PreparedStatementにセットするパラメータ
	 */
	@SuppressWarnings("unchecked")
	public Object toJdbc(final Object object, final Connection connection) {
		if (object == null) {
			return null;
		}
		for (@SuppressWarnings("rawtypes")
		BindParameterMapper parameterMapper : mappers) {
			if (parameterMapper.targetType().isInstance(object)) {
				return parameterMapper.toJdbc(object, connection, this);
			}
		}

		if (object instanceof Boolean
				|| object instanceof Byte
				|| object instanceof Short
				|| object instanceof Integer
				|| object instanceof Long
				|| object instanceof Float
				|| object instanceof Double
				|| object instanceof BigDecimal
				|| object instanceof String

				|| object instanceof byte[]

				|| object instanceof java.sql.Date
				|| object instanceof java.sql.Time
				|| object instanceof java.sql.Timestamp
				|| object instanceof java.sql.Array
				|| object instanceof java.sql.Ref
				|| object instanceof java.sql.Blob
				|| object instanceof java.sql.Clob
				|| object instanceof java.sql.SQLXML

				|| object instanceof java.sql.Struct) {
			return object;
		}

		for (@SuppressWarnings("rawtypes")
		BindParameterMapper parameterMapper : DEFAULT_MAPPERS) {
			if (parameterMapper.targetType().isInstance(object)) {
				return parameterMapper.toJdbc(object, connection, this);
			}
		}
		return object;
	}

	/**
	 * 配列用のバインドパラメータMapperが存在するかどうかを判定
	 *
	 * @param object マッピング対象オブジェクト
	 * @return 配列用のバインドパラメータMapperが存在する場合は<code>true</code>
	 */
	public boolean existsArrayMapper(final Object object) {
		if (!object.getClass().isArray()) {
			return false;
		}
		for (BindParameterMapper<?> parameterMapper : mappers) {
			if (parameterMapper.targetType().isInstance(object)) {
				return true;
			}
		}
		return false;
	}

}
