/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.math.BigDecimal;
import java.sql.Connection;
import java.time.Clock;
import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
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
			new BigIntegerParameterMapper(),
			new OptionalParameterMapper(),
			new OptionalIntParameterMapper(),
			new OptionalLongParameterMapper(),
			new OptionalDoubleParameterMapper(),
			new DomainParameterMapper(),
			new EnumParameterMapper(), // DomainParameterMapper・DateTimeApiParameterMapperより後に設定
			new StringArrayParameterMapper(),
			new IntArrayParameterMapper(),
			new IntWrapperArrayParameterMapper(),
			new LongArrayParameterMapper(),
			new LongWrapperArrayParameterMapper(),
			new DoubleArrayParameterMapper(),
			new DoubleWrapperArrayParameterMapper(),
	};

	/** Serviceに登録されたMapper */
	private static final List<BindParameterMapper<?>> LOADED_MAPPERS = load();

	private static List<BindParameterMapper<?>> load() {
		List<BindParameterMapper<?>> list = new ArrayList<>();
		ServiceLoader.load(BindParameterMapper.class).forEach(list::add);
		return list;
	}

	private final DateTimeApiParameterMapper dateTimeApiParameterMapper;

	private final Clock clock;

	/**
	 * コンストラクタ.
	 *
	 * @param clock Clock
	 */
	public BindParameterMapperManager(final Clock clock) {
		this.mappers = new CopyOnWriteArrayList<>(LOADED_MAPPERS);
		this.dateTimeApiParameterMapper = new DateTimeApiParameterMapper(clock);
		this.clock = clock;

		this.mappers.stream().filter(BindParameterMapperWithClock.class::isInstance)
				.forEach(m -> ((BindParameterMapperWithClock<?>) m).setClock(clock));
	}

	/**
	 * コピーコンストラクタ
	 *
	 * @param parameterMapperManager コピー元のパラメータ変換クラス
	 * @param clock clock
	 */
	public BindParameterMapperManager(final BindParameterMapperManager parameterMapperManager, final Clock clock) {
		this.mappers = new CopyOnWriteArrayList<>(parameterMapperManager.mappers);
		this.dateTimeApiParameterMapper = new DateTimeApiParameterMapper(clock);
		this.clock = clock;

		this.mappers.stream().filter(BindParameterMapperWithClock.class::isInstance)
				.forEach(m -> ((BindParameterMapperWithClock<?>) m).setClock(clock));
	}

	/**
	 * {@link BindParameterMapper}を追加
	 *
	 * @param parameterMapper {@link BindParameterMapper}
	 */
	public void addMapper(final BindParameterMapper<?> parameterMapper) {
		if (parameterMapper instanceof BindParameterMapperWithClock) {
			((BindParameterMapperWithClock<?>) parameterMapper).setClock(this.clock);
		}
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
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public Object toJdbc(final Object object, final Connection connection) {
		if (object == null) {
			return null;
		}
		for (BindParameterMapper parameterMapper : mappers) {
			if (parameterMapper.canAccept(object)) {
				return parameterMapper.toJdbc(object, connection, this);
			}
		}

		if (object instanceof Boolean || object instanceof Byte || object instanceof Short || object instanceof Integer
				|| object instanceof Long || object instanceof Float || object instanceof Double
				|| object instanceof BigDecimal || object instanceof String

				|| object instanceof byte[]

				|| object instanceof java.sql.Date || object instanceof java.sql.Time
				|| object instanceof java.sql.Timestamp || object instanceof java.sql.Array
				|| object instanceof java.sql.Ref || object instanceof java.sql.Blob || object instanceof java.sql.Clob
				|| object instanceof java.sql.SQLXML

				|| object instanceof java.sql.Struct) {
			return object;
		}

		if (dateTimeApiParameterMapper.canAccept(object)) {
			return ((BindParameterMapper) dateTimeApiParameterMapper).toJdbc(object, connection, this);
		}

		for (BindParameterMapper parameterMapper : DEFAULT_MAPPERS) {
			if (parameterMapper.canAccept(object)) {
				return parameterMapper.toJdbc(object, connection, this);
			}
		}
		return object;
	}

	/**
	 * 標準でパラメータとして受け入れ可能な値かを判定
	 *
	 * @param object 指定パラメータ
	 * @return 標準で受け入れ可能かどうか。可能な場合<code>true</code>
	 */
	public boolean canAcceptByStandard(final Object object) {
		if (object == null) {
			return true;
		}
		if (object instanceof Boolean || object instanceof Byte || object instanceof Short || object instanceof Integer
				|| object instanceof Long || object instanceof Float || object instanceof Double
				|| object instanceof BigDecimal || object instanceof String

				|| object instanceof byte[]

				|| object instanceof java.sql.Date || object instanceof java.sql.Time
				|| object instanceof java.sql.Timestamp || object instanceof java.sql.Array
				|| object instanceof java.sql.Ref || object instanceof java.sql.Blob || object instanceof java.sql.Clob
				|| object instanceof java.sql.SQLXML

				|| object instanceof java.sql.Struct) {
			return true;
		}
		for (var parameterMapper : mappers) {
			if (parameterMapper.canAccept(object)) {
				return true;
			}
		}

		if (dateTimeApiParameterMapper.canAccept(object)) {
			return true;
		}

		for (var parameterMapper : DEFAULT_MAPPERS) {
			if (parameterMapper.canAccept(object)) {
				return true;
			}
		}
		return false;
	}
}
