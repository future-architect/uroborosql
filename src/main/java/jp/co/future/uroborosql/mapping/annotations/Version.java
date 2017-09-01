package jp.co.future.uroborosql.mapping.annotations;

import java.lang.annotation.*;

/**
 * エンティティ バージョン情報アノテーション
 *
 * @author hoshi
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Version {
}
