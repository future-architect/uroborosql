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

import jp.co.future.uroborosql.mapping.LockVersionOptimisticLockSupplier;
import jp.co.future.uroborosql.mapping.OptimisticLockSupplier;

/**
 * エンティティ バージョン情報アノテーション
 *
 * @author hoshi
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Version {
	/** @return 楽観ロックのサプライヤクラス */
	Class<? extends OptimisticLockSupplier> supplier() default LockVersionOptimisticLockSupplier.class;
}
