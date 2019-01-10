/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.utils;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * paramBeanメソッドで利用するアクセッサ
 *
 * @author hoshi-k
 *
 */
public class BeanAccessor {

	/**
	 * Class to convert Object to Map<br>
	 * <br>
	 * We created our own class to reduce the number of iterations when using SqlBatch#by.
	 */
	private static class AsMap extends AbstractMap<String, Object> {
		private final Set<Entry<String, Object>> entrySet;

		public AsMap(final Object object) {
			Field[] fields = BeanAccessor.fields(object.getClass()).stream()
					.toArray(Field[]::new);
			@SuppressWarnings("unchecked")
			Entry<String, Object>[] entries = new Entry[fields.length];
			this.entrySet = new AbstractSet<Map.Entry<String, Object>>() {

				@Override
				public Iterator<Entry<String, Object>> iterator() {
					return new Iterator<Map.Entry<String, Object>>() {
						int index = 0;

						@Override
						public boolean hasNext() {
							return fields.length > index;
						}

						@Override
						public Entry<String, Object> next() {
							Entry<String, Object> next = entries[index];
							if (next == null) {
								Field f = fields[index];
								next = new AbstractMap.SimpleImmutableEntry<>(f.getName(),
										BeanAccessor.value(f, object));
								entries[index] = next;
							}
							index++;
							return next;
						}

					};
				}

				@Override
				public int size() {
					return fields.length;
				}
			};
		}

		@Override
		public Set<Entry<String, Object>> entrySet() {
			return this.entrySet;
		}

	}

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

	/**
	 * 与えられたオブジェクトをMap形式に変換します。
	 *
	 * @param object 対象のオブジェクト
	 * @return {@literal Map<String, Object>}
	 */
	public static Map<String, Object> asMap(final Object object) {
		return new AsMap(object);
	}
}
