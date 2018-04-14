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
 * エンティティ 永続化除外対象指定アノテーション
 *
 * @author ota
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Transient {
	/**
	 * insert文で除外対象とするかどうか
	 *
	 * @return 除外対象とする場合<code>true</code>
	 */
	boolean insert() default true;

	/** update文で除外対象とするかどうか
	 *
	 * @return 除外対象とする場合<code>true</code>
	 */
	boolean update() default true;
}