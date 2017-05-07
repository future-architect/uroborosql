package jp.co.future.uroborosql.mapping.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * エンティティ カラム情報アノテーション
 *
 * @author ota
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Column {

	/**
	 * カラム名
	 *
	 * @return カラム名
	 */
	String name() default "";
}