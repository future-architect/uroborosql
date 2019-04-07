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
 * SEQUENCEによるID生成を設定します
 *
 * @author H.Sugimoto
 * @since v0.12.0
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface SequenceGenerator {
	/**
	 * @return カタログ名
	 */
	String catalog() default "";

	/**
	 * @return スキーマ名
	 */
	String schema() default "";

	/**
	 * @return シーケンス名
	 */
	String sequence();
}
