/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * ドメイン定義アノテーション
 *
 * @author ota
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Domain {

	/**
	 * ドメインクラスを生成するのに必要な値の型
	 *
	 * @return ドメインクラスを生成するのに必要な値の型
	 */
	Class<?> valueType();

	/**
	 * ドメインクラスを生成・取得するメソッド名
	 * <p>
	 * デフォルトではコンストラクタが呼び出されます
	 *
	 * @return ドメインクラスを生成・取得するメソッド名
	 */
	String factoryMethod() default "";

	/**
	 * JDBCが受け付けられる値に変換できる値を返すメソッド名
	 *
	 * @return JDBCが受け付けられる値に変換できる値を返すメソッド名
	 */
	String toJdbcMethod() default "getValue";

	/**
	 * NULLをインスタンスとして管理する場合はtrue
	 *
	 * @return NULLをインスタンスとして管理する場合はtrue
	 */
	boolean nullable() default false;
}