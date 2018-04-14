/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.utils;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * paramBeanメソッドで利用するアクセッサ
 *
 * @author hoshi-k
 *
 */
public class BeanAccessor {
	/**
	 * 指定したクラスの持つ全てのフィールドを親クラスを含めて取得する
	 *
	 * @param cls 型
	 * @return {@literal Set<Field>ｊ}
	 */
	public static Collection<Field> fields(final Class<?> cls) {
		Map<String, Field> fieldMap = new HashMap<>();
		walkFields(cls, fieldMap);
		return fieldMap.values();
	}

	/**
	 * 指定したクラスの持つ全てのフィールドを再帰的に探索して取得する
	 *
	 * @param cls 型
	 * @param fieldMap {@literal Map<String, Field>ｊ}
	 */
	private static void walkFields(final Class<?> cls, final Map<String, Field> fieldMap) {
		if (cls.equals(Object.class)) {
			return;
		}
		Class<?> superClass = cls.getSuperclass();
		walkFields(superClass, fieldMap);
		Arrays.stream(cls.getDeclaredFields())
				.filter(f -> !Modifier.isStatic(f.getModifiers()))
				.forEach(f -> fieldMap.put(f.getName(), f));
	}

	/**
	 * 指定した<code>Field</code>の値を取得する
	 *
	 * @param f <code>Field</code>
	 * @param bean 対象のオブジェクト
	 * @return フィールドの値
	 */
	public static Object value(final Field f, final Object bean) {
		try {
			f.setAccessible(true);
			return f.get(bean);
		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw new UroborosqlRuntimeException(e);
		}
	}
}
