package jp.co.future.uroborosql.mapping;

import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.stream.Collectors;

/**
 * 実装の型
 * <p>
 * 実装後の総称型を考慮した型
 *
 * @author ota
 */
public class JavaType {
	/**
	 * JavaType生成
	 *
	 * @param rawType クラス
	 * @return JavaType
	 */
	public static JavaType of(final Class<?> rawType) {
		return new JavaType(rawType, new JavaType[0]);
	}

	/**
	 * JavaType生成
	 *
	 * @param subclasses サブクラスリスト
	 * @param field Field
	 * @return JavaType
	 */
	public static JavaType create(final Collection<Class<?>> subclasses, final Field field) {
		return create(subclasses, field.getDeclaringClass(), field.getGenericType());
	}

	/**
	 * JavaType生成
	 *
	 * @param subclasses サブクラスリスト
	 * @param typeOwner Typeが設定されたクラス
	 * @param type Type
	 * @return JavaType
	 */
	public static JavaType create(final Collection<Class<?>> subclasses, final Class<?> typeOwner, final Type type) {
		if (type instanceof Class<?>) {
			return of((Class<?>) type);
		} else if (type instanceof TypeVariable<?>) {
			TypeVariable<?> variable = (TypeVariable<?>) type;
			return createByParamName(subclasses, typeOwner, variable.getName());
		} else if (type instanceof GenericArrayType) {
			GenericArrayType arrayType = (GenericArrayType) type;
			JavaType comp = create(subclasses, typeOwner, arrayType.getGenericComponentType());
			return comp.toArrayType();
		} else if (type instanceof ParameterizedType) {
			ParameterizedType parameterizedType = (ParameterizedType) type;
			JavaType[] params = Arrays.stream(parameterizedType.getActualTypeArguments())
					.map(t -> create(subclasses, typeOwner, t))
					.toArray(JavaType[]::new);

			return new JavaType((Class<?>) parameterizedType.getRawType(), params);
		} else if (type instanceof WildcardType) {
			WildcardType wildcardType = (WildcardType) type;
			return create(subclasses, typeOwner, wildcardType.getUpperBounds()[0]);
		}

		throw new IllegalArgumentException(type.getTypeName());

	}

	private final Class<?> rawType;
	private final JavaType[] params;

	private JavaType(final Class<?> rawType, final JavaType[] params) {
		this.rawType = rawType;
		this.params = params;
	}

	/**
	 * クラス(raw)取得
	 *
	 * @return クラス(raw)
	 */
	public Class<?> getRawType() {
		return this.rawType;
	}

	/**
	 * 指定総称型取得
	 *
	 * @param index 指定Index
	 * @return 指定総称型
	 */
	public JavaType getParam(final int index) {
		return this.params[index];
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		if (!this.rawType.isArray()) {
			sb.append(this.rawType.getName());
		} else {
			sb.append(this.rawType.getComponentType().getName());
		}
		if (this.params.length > 0) {
			sb.append("<")
					.append(Arrays.stream(this.params)
							.map(Object::toString)
							.collect(Collectors.joining(", ")))
					.append(">");
		}
		if (this.rawType.isArray()) {
			sb.append("[]");
		}
		return sb.toString();
	}

	private JavaType toArrayType() {
		Class<?> arrayType = java.lang.reflect.Array.newInstance(this.rawType, 0).getClass();
		return new JavaType(arrayType, this.params);
	}

	private static JavaType createByParamName(final Collection<Class<?>> subclasses, final Class<?> typeOwner, final String name) {
		TypeVariable<? extends Class<?>>[] superGenTypeAray = typeOwner.getTypeParameters();
		for (int index = 0; index < superGenTypeAray.length; index++) {
			TypeVariable<? extends Class<?>> type = superGenTypeAray[index];
			if (name.equals(type.getName())) {
				if (subclasses.isEmpty()) {
					return create(Collections.emptyList(), typeOwner, type.getBounds()[0]);
				} else {
					Deque<Class<?>> classes = new LinkedList<>(subclasses);
					Class<?> sub = classes.pop();
					Type genericSuperclass = sub.getGenericSuperclass();
					if (genericSuperclass instanceof ParameterizedType) {
						ParameterizedType parameterizedType = (ParameterizedType) genericSuperclass;
						return create(classes, sub, parameterizedType.getActualTypeArguments()[index]);
					} else {
						return create(Collections.emptyList(), typeOwner, type.getBounds()[0]);
					}
				}
			}
		}
		throw new IllegalArgumentException();
	}

}
