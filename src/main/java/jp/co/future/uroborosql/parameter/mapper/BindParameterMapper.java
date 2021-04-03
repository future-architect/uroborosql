/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.parameter.mapper;

import java.sql.Connection;

import jp.co.future.uroborosql.mapping.JavaType;

/**
 * パラメータをJDBCが受け入れられる型に変換するインターフェース
 *
 * @param <T> 変換対象の型
 *
 * @author ota
 */
public interface BindParameterMapper<T> {
	/**
	 * 変換対象の型
	 *
	 * @return 変換対象の型
	 */
	@SuppressWarnings("unchecked")
	default Class<T> targetType() {
		return (Class<T>) JavaType
				.of(new JavaType.ImplementClass(this.getClass()), BindParameterMapper.class.getTypeParameters()[0])
				.getRawType();
	}

	/**
	 * 変換可能な値であるかを検証
	 *
	 * @param object 変換対象の値
	 * @return 変換可能な値
	 */
	default boolean canAccept(final Object object) {
		return targetType().isInstance(object);
	}

	/**
	 * JDBCが受け入れ可能な型に変換します
	 *
	 * @param original 変換対象データ
	 * @param connection パラメータクラス生成用Connection（パラメータクラス生成以外の用途では利用しないでください。{@link JdbcParameterFactory}のメソッドも利用してください）
	 * @param parameterMapperManager 再起処理用パラメータ変換マネジャー
	 * @return JDBCが受け入れ可能な型に変換した値
	 */
	Object toJdbc(T original, Connection connection, BindParameterMapperManager parameterMapperManager);
}
