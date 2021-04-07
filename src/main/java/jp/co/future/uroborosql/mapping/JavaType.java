/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.mapping;

import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.IntStream;

/**
 * 実装の型
 * <p>
 * 実装の総称型を考慮した型
 *
 * @author ota
 */
public abstract class JavaType {

	/**
	 * 実装クラス情報管理クラス
	 */
	public static class ImplementClass {
		@SuppressWarnings("unused")
		private final Class<?> implement;
		private Class<?> next;
		// イテレーション済のキャッシュ用
		private final Map<Class<?>, Class<?>> subclasses = new HashMap<>();
		private final Map<Class<?>, Type> generics = new HashMap<>();

		/**
		 * コンストラクタ
		 *
		 * @param implement 実装クラス
		 */
		public ImplementClass(final Class<?> implement) {
			this.implement = implement;
			this.next = implement;
			this.subclasses.put(this.next, null);
		}

		/**
		 * サブクラスの取得
		 *
		 * @param type ターゲットクラス
		 * @return サブクラス
		 */
		public Class<?> getSubclass(final Class<?> type) {
			Class<?> ret = subclasses.get(type);
			if (ret != null) {
				return ret;
			}
			return findSubclass(type);
		}

		/**
		 * スーパークラス・またはインタフェースを表すTypeを返す。
		 *
		 * @param parentclass スーパークラス・またはインタフェース
		 * @return Type
		 */
		public Type getGenericParentClass(final Class<?> parentclass) {
			return this.generics.computeIfAbsent(parentclass, this::findGenericParentClass);
		}

		private Type findGenericParentClass(final Class<?> parentclass) {
			Class<?> subclass = getSubclass(parentclass);
			Class<?> superclass = subclass.getSuperclass();
			if (superclass != null && superclass.equals(parentclass)) {
				return subclass.getGenericSuperclass();
			}
			var interfaces = subclass.getInterfaces();
			for (var i = 0; i < interfaces.length; i++) {
				if (interfaces[i].equals(parentclass)) {
					return subclass.getGenericInterfaces()[i];
				}
			}
			throw new IllegalArgumentException();
		}

		private Class<?> findSubclass(final Class<?> type) {
			// superclassをイテレーション
			while (this.next != null) {
				Class<?> subclass = this.next;
				Class<?> superclass = this.next.getSuperclass();
				walkInterfaces(this.next, this.next.getInterfaces());
				this.next = superclass;
				if (superclass == null) {
					break;
				}
				this.subclasses.put(superclass, subclass);
				if (type.equals(superclass)) {
					return subclass;
				}
			}
			return subclasses.get(type);
		}

		private void walkInterfaces(final Class<?> subclass, final Class<?>[] interfaces) {
			for (Class<?> interfaceType : interfaces) {
				if (this.subclasses.putIfAbsent(interfaceType, subclass) != null) {
					// 追加済
					continue;
				}
				walkInterfaces(interfaceType, interfaceType.getInterfaces());
			}
		}

	}

	private static class ClassJavaType extends JavaType {
		private final Class<?> rawType;
		private final JavaType[] params;

		ClassJavaType(final ImplementClass implementClass, final Class<?> rawType) {
			this.rawType = rawType;
			TypeVariable<?>[] typeParameters = rawType.getTypeParameters();
			this.params = new JavaType[typeParameters.length];
			for (var i = 0; i < typeParameters.length; i++) {
				this.params[i] = create(implementClass, typeParameters[i]);
			}
		}

		@Override
		public Class<?> getRawType() {
			return this.rawType;
		}

		@Override
		public JavaType getParam(final int index) {
			return this.params[index];
		}

		@Override
		public JavaType[] getUpperBounds() {
			return new JavaType[] { this };
		}
	}

	private static class ArrayJavaType extends JavaType {
		private final Class<?> rawType;
		private final JavaType component;

		ArrayJavaType(final ImplementClass implementClass, final GenericArrayType arrayType) {
			this.component = create(implementClass, arrayType.getGenericComponentType());
			this.rawType = java.lang.reflect.Array.newInstance(this.component.getRawType(), 0).getClass();
		}

		@Override
		public Class<?> getRawType() {
			return this.rawType;
		}

		@Override
		public JavaType getParam(final int index) {
			return component.getParam(index);
		}

		@Override
		public JavaType[] getUpperBounds() {
			return component.getUpperBounds();
		}
	}

	private static class ParameterizedJavaType extends JavaType {
		private final Class<?> rawType;
		private final ImplementClass implementClass;
		private final Type[] actualTypeArguments;
		private final JavaType[] params;

		ParameterizedJavaType(final ImplementClass implementClass, final ParameterizedType parameterizedType) {
			this.rawType = (Class<?>) parameterizedType.getRawType();
			this.implementClass = implementClass;

			this.actualTypeArguments = parameterizedType.getActualTypeArguments();
			this.params = new JavaType[this.actualTypeArguments.length];
		}

		@Override
		public Class<?> getRawType() {
			return this.rawType;
		}

		@Override
		public JavaType getParam(final int index) {
			// 要求されたタイミングで生成
			return this.params[index] != null ? this.params[index]
					: (this.params[index] = create(this.implementClass, this.actualTypeArguments[index]));
		}

		@Override
		public JavaType[] getUpperBounds() {
			return new JavaType[] { this };
		}

		@Override
		protected String toParamString() {
			return this.rawType.getName();
		}
	}

	private static class WildcardJavaType extends JavaType {
		private final ImplementClass implementClass;
		private final WildcardType wildcardType;
		private final JavaType[] upperBounds;
		private final JavaType[] lowerBounds;// Up to language spec currently only one
		private final JavaType general;

		WildcardJavaType(final ImplementClass implementClass, final WildcardType wildcardType) {
			this.implementClass = implementClass;
			this.wildcardType = wildcardType;

			this.upperBounds = new JavaType[this.wildcardType.getUpperBounds().length];
			this.lowerBounds = new JavaType[this.wildcardType.getLowerBounds().length];

			if (isLowerBounds()) {
				this.general = getLower();
			} else {
				this.general = getUpper(0);
			}
		}

		@Override
		public Class<?> getRawType() {
			return this.general.getRawType();
		}

		@Override
		public JavaType getParam(final int index) {
			return this.general.getParam(index);
		}

		@Override
		public JavaType[] getUpperBounds() {
			return IntStream.range(0, this.upperBounds.length)
					.mapToObj(this::getUpper)
					.toArray(JavaType[]::new);
		}

		private JavaType getUpper(final int index) {
			return this.upperBounds[index] != null ? this.upperBounds[index]
					: (this.upperBounds[index] = create(this.implementClass,
							this.wildcardType.getUpperBounds()[index]));
		}

		@Override
		public JavaType getLower() {
			return this.lowerBounds.length > 0 ? getLower(0) : null;
		}

		private JavaType getLower(final int index) {
			return this.lowerBounds[index] != null ? this.lowerBounds[index]
					: (this.lowerBounds[index] = create(this.implementClass,
							this.wildcardType.getLowerBounds()[index]));
		}

		private boolean isLowerBounds() {
			return this.lowerBounds.length > 0;
		}

		private boolean isUnbounded() {
			if (isLowerBounds()) {
				return false;
			}
			if (this.upperBounds.length == 1) {
				return this.general.getRawType().equals(Object.class);
			}
			return false;
		}

		@Override
		public String toString() {
			return toString(JavaType::toString);
		}

		@Override
		protected String toParamString() {
			return toString(JavaType::toParamString);
		}

		private String toString(final Function<JavaType, String> toString) {
			if (isUnbounded()) {
				return "?";
			}
			if (isLowerBounds()) {
				return "? super " + toString.apply(this.getLower());
			} else {
				var sb = new StringBuilder("? extends ");
				for (var i = 0; i < this.upperBounds.length; i++) {
					if (i > 0) {
						sb.append(" & ");
					}
					sb.append(toString.apply(this.getUpper(i)));
				}
				return sb.toString();
			}
		}
	}

	private static class VariableJavaType extends JavaType {
		private final ImplementClass implementClass;
		private final String variableName;
		private final Type[] bounds;
		private final JavaType[] javaTypeBounds;
		private final JavaType general;

		VariableJavaType(final ImplementClass implementClass, final TypeVariable<?> typeVariable) {
			this.implementClass = implementClass;
			this.variableName = typeVariable.getName();
			this.bounds = typeVariable.getBounds();
			this.javaTypeBounds = new JavaType[this.bounds.length];
			this.general = getJavaType(0);
		}

		@Override
		public Class<?> getRawType() {
			return this.general.getRawType();
		}

		@Override
		public JavaType getParam(final int index) {
			return this.general.getParam(index);
		}

		@Override
		public JavaType[] getUpperBounds() {
			return IntStream.range(0, this.bounds.length)
					.mapToObj(this::getJavaType)
					.toArray(JavaType[]::new);
		}

		private JavaType getJavaType(final int index) {
			return this.javaTypeBounds[index] != null ? this.javaTypeBounds[index]
					: (this.javaTypeBounds[index] = create(this.implementClass, this.bounds[index]));
		}

		private boolean isMulti() {
			return this.bounds.length > 1;
		}

		@Override
		public String toString() {
			return toString(JavaType::toString);
		}

		@Override
		protected String toParamString() {
			return toString(JavaType::toParamString);
		}

		private String toString(final Function<JavaType, String> toString) {
			if (!isMulti()) {
				return toString.apply(this.general);
			}
			var sb = new StringBuilder(this.variableName)
					.append(" extends ");
			for (var i = 0; i < this.bounds.length; i++) {
				if (i > 0) {
					sb.append(" & ");
				}
				sb.append(toString.apply(this.getJavaType(i)));
			}
			return sb.toString();
		}
	}

	/**
	 * JavaType生成
	 *
	 * @param rawType クラス
	 * @return JavaType
	 */
	public static JavaType of(final Class<?> rawType) {
		return new ClassJavaType(new ImplementClass(rawType), rawType);
	}

	/**
	 * JavaType生成
	 *
	 * @param implementClass 実装クラス
	 * @param field Field
	 * @return JavaType
	 */
	public static JavaType of(final Class<?> implementClass, final Field field) {
		return of(new ImplementClass(implementClass), field);
	}

	/**
	 * JavaType生成
	 *
	 * @param implementClass 実装クラス情報
	 * @param field Field
	 * @return JavaType
	 */
	public static JavaType of(final ImplementClass implementClass, final Field field) {
		return of(implementClass, field.getGenericType());
	}

	/**
	 * JavaType生成
	 *
	 * @param implementClass 実装クラス情報
	 * @param type Type
	 * @return JavaType
	 */
	public static JavaType of(final ImplementClass implementClass, final Type type) {
		return create(implementClass, type);
	}

	private static JavaType create(final ImplementClass implementClass, final Type type) {
		if (type instanceof Class<?>) {
			return new ClassJavaType(implementClass, (Class<?>) type);
		} else if (type instanceof TypeVariable<?>) {
			TypeVariable<?> variable = (TypeVariable<?>) type;
			return createByParamName(implementClass, variable);
		} else if (type instanceof GenericArrayType) {
			var arrayType = (GenericArrayType) type;
			return new ArrayJavaType(implementClass, arrayType);
		} else if (type instanceof ParameterizedType) {
			var parameterizedType = (ParameterizedType) type;
			return new ParameterizedJavaType(implementClass, parameterizedType);
		} else if (type instanceof WildcardType) {
			var wildcardType = (WildcardType) type;
			return new WildcardJavaType(implementClass, wildcardType);
		}

		throw new IllegalArgumentException(type.getTypeName());

	}

	/**
	 * クラス(raw)取得
	 *
	 * @return クラス(raw)
	 */
	public abstract Class<?> getRawType();

	/**
	 * 指定総称型取得
	 *
	 * @param index 指定Index
	 * @return 指定総称型
	 */
	public abstract JavaType getParam(final int index);

	/**
	 * 上限JavaType取得
	 *
	 * @return 上限JavaType
	 */
	public abstract JavaType[] getUpperBounds();

	/**
	 * 下限JavaType取得
	 *
	 * @return 下限JavaType
	 */
	public JavaType getLower() {
		return null;
	}

	@Override
	public String toString() {
		var sb = new StringBuilder();
		Class<?> rawType = getRawType();
		if (!rawType.isArray()) {
			sb.append(rawType.getName());
		} else {
			sb.append(rawType.getComponentType().getName());
		}
		TypeVariable<? extends Class<?>>[] typeParameters = rawType.getTypeParameters();
		if (typeParameters.length > 0) {
			sb.append("<");
			for (var i = 0; i < typeParameters.length; i++) {
				if (i > 0) {
					sb.append(", ");
				}
				var s = this.getParam(i).toParamString();
				sb.append(s);
			}
			sb.append(">");
		}
		if (rawType.isArray()) {
			sb.append("[]");
		}
		return sb.toString();
	}

	/**
	 * パラメータの文字列化
	 *
	 * @return 文字列
	 */
	protected String toParamString() {
		return toString();
	}

	private static JavaType createByParamName(final ImplementClass implementClass, final TypeVariable<?> variable) {
		GenericDeclaration declaration = variable.getGenericDeclaration();
		var name = variable.getName();
		var typeParameters = declaration.getTypeParameters();
		for (var index = 0; index < typeParameters.length; index++) {
			TypeVariable<?> type = typeParameters[index];
			if (name.equals(type.getName())) {
				if (declaration instanceof Class) {
					Class<?> declarationClass = (Class<?>) declaration;
					Class<?> sub = implementClass.getSubclass(declarationClass);
					if (sub != null) {
						var genericSuperclass = implementClass.getGenericParentClass(declarationClass);
						if (genericSuperclass instanceof ParameterizedType) {
							var parameterizedType = (ParameterizedType) genericSuperclass;
							return create(implementClass, parameterizedType.getActualTypeArguments()[index]);
						}
						// Generics未指定
					}
					return new VariableJavaType(implementClass, type);
				} else if (declaration instanceof Executable) {
					return new VariableJavaType(implementClass, type);
				}
			}
		}
		throw new IllegalArgumentException();
	}
}
