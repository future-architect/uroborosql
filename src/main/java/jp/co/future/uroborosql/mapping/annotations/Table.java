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
 * エンティティ テーブル情報アノテーション
 *
 * @author ota
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Table {

	/**
	 * テーブル名
	 * <p>
	 * デフォルトはエンティティ名から算出します.
	 *
	 * @return テーブル名
	 */
	String name() default "";

	/**
	 * スキーマ名
	 *
	 * @return スキーマ名
	 */
	String schema() default "";
}