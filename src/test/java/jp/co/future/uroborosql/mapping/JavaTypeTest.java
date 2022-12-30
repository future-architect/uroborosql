package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.Spliterator;

import org.junit.Test;

import jp.co.future.uroborosql.mapping.JavaType.ImplementClass;

@SuppressWarnings({ "rawtypes" })
public class JavaTypeTest {

	class Test01 {
		String test;
	}

	@Test
	public void test01() throws NoSuchFieldException, SecurityException {
		Field field = Test01.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test01.class, field);
		assertThat(javaType.toString(), is("java.lang.String"));
	}

	class Abs2Test02<A, T, B> {
		T test;
	}

	class Abs1Test02<A, T extends Number, B> extends Abs2Test02<A, T, B> {
	}

	class Test02 extends Abs1Test02<Object, BigDecimal, Object> {
	}

	@Test
	public void test02() throws NoSuchFieldException, SecurityException {
		Field field = Abs2Test02.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test02.class, field);
		assertThat(javaType.toString(), is("java.math.BigDecimal"));

		javaType = JavaType.of(Abs1Test02.class, field);
		assertThat(javaType.toString(), is("java.lang.Number"));
	}

	class Abs2Test03<A, T, B> {
		T test;
	}

	class Abs1Test03<A, T extends Number, B> extends Abs2Test03<A, T[], B> {
	}

	class Test03 extends Abs1Test03<Object, BigDecimal, Object> {
	}

	@Test
	public void test03() throws NoSuchFieldException, SecurityException {
		Field field = Abs2Test03.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test03.class, field);
		assertThat(javaType.toString(), is("java.math.BigDecimal[]"));

		javaType = JavaType.of(Abs1Test03.class, field);
		assertThat(javaType.toString(), is("java.lang.Number[]"));
	}

	class Abs2Test04<A, T extends Number, B> {
		List<T> test;
	}

	class Abs1Test04<C, D> extends Abs2Test04 {
	}

	class Test04 extends Abs1Test04 {
	}

	@Test
	public void test04() throws NoSuchFieldException, SecurityException {
		Field field = Abs2Test04.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test04.class, field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Number>"));

		javaType = JavaType.of(Abs1Test04.class, field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Number>"));
	}

	class Abs2Test05<A, T extends Number, B> {
		T[] test;
	}

	class Abs1Test05<C, D> extends Abs2Test05 {
	}

	class Test05 extends Abs1Test05 {
	}

	@Test
	public void test05() throws NoSuchFieldException, SecurityException {
		Field field = Abs2Test05.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test05.class, field);
		assertThat(javaType.toString(), is("java.lang.Number[]"));

		javaType = JavaType.of(Abs1Test05.class, field);
		assertThat(javaType.toString(), is("java.lang.Number[]"));
	}

	class Abs2Test06<A, T extends Number, B> {
		List<? extends T> test;
	}

	class Abs1Test06<C, D> extends Abs2Test06 {
	}

	class Test06 extends Abs1Test06 {
	}

	@Test
	public void test06() throws NoSuchFieldException, SecurityException {
		Field field = Abs2Test06.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test06.class, field);
		assertThat(javaType.toString(), is("java.util.List<? extends java.lang.Number>"));

		javaType = JavaType.of(Abs1Test06.class, field);
		assertThat(javaType.toString(), is("java.util.List<? extends java.lang.Number>"));
	}

	class Abs2Test07<A, T extends Number, B> {
		List<?> test;
	}

	class Abs1Test07<C, D> extends Abs2Test07 {
	}

	class Test07 extends Abs1Test07 {
	}

	@Test
	public void test07() throws NoSuchFieldException, SecurityException {
		Field field = Abs2Test07.class.getDeclaredField("test");
		JavaType javaType = JavaType.of(Test07.class, field);
		assertThat(javaType.toString(), is("java.util.List<?>"));
		assertThat(javaType.getParam(0).getRawType(), is(equalTo(Object.class)));

		javaType = JavaType.of(Abs1Test07.class, field);
		assertThat(javaType.toString(), is("java.util.List<?>"));
	}

	interface Test08 extends List<String>, Set<String> {

		@Override
		default Spliterator<String> spliterator() {
			return List.super.spliterator();
		}
	}

	@Test
	public void test08() throws SecurityException, NoSuchMethodException {
		ImplementClass implementClass = new ImplementClass(Test08.class);

		assertThat(implementClass.getSubclass(List.class), is(equalTo(Test08.class)));
		assertThat(implementClass.getSubclass(Collection.class), is(equalTo(List.class)));

		Method method = Iterable.class.getMethod("iterator");
		JavaType javaType = JavaType.of(new ImplementClass(Test08.class), method.getGenericReturnType());
		assertThat(javaType.toString(), is("java.util.Iterator<java.lang.String>"));
		assertThat(javaType.getParam(0).toString(), is("java.lang.String"));

	}

	enum Test09 {
		A, B
	}

	@Test
	public void test09() throws SecurityException, NoSuchMethodException {
		ImplementClass implementClass = new ImplementClass(Test09.class);

		assertThat(implementClass.getSubclass(Enum.class), is(equalTo(Test09.class)));

		Method method = Enum.class.getMethod("compareTo", Enum.class);
		JavaType javaType = JavaType.of(new ImplementClass(Test09.class),
				method.getParameters()[0].getParameterizedType());
		assertThat(javaType.toString(), is("jp.co.future.uroborosql.mapping.JavaTypeTest$Test09"));

		javaType = JavaType.of(new ImplementClass(Enum.class), method.getParameters()[0].getParameterizedType());
		assertThat(javaType.toString(), is("java.lang.Enum<java.lang.Enum>"));
	}

	interface Abs1Test10<T, E extends Abs1Test10<T, E, E2>, E2 extends Abs2Test10<E2>> {
		T t();

		E e();
	}

	interface Abs2Test10<T extends Abs2Test10<T>> extends Abs1Test10<T, T, T> {

	}

	interface Test10 extends Abs2Test10<Test10> {

	}

	@Test
	public void test10() throws SecurityException, NoSuchMethodException {

		Method method = Abs1Test10.class.getMethod("e");
		JavaType javaType = JavaType.of(new ImplementClass(Test10.class), method.getGenericReturnType());
		assertThat(javaType.toString(), is("jp.co.future.uroborosql.mapping.JavaTypeTest$Test10"));

		javaType = JavaType.of(new ImplementClass(Abs2Test10.class), method.getGenericReturnType());
		assertThat(
				javaType.toString(),
				is("jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10<jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10>"));
		assertThat(
				javaType.getParam(0).toString(),
				is("jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10<jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10>"));

		javaType = JavaType.of(new ImplementClass(Abs1Test10.class), method.getGenericReturnType());
		assertThat(
				javaType.toString(),
				is(
						"jp.co.future.uroborosql.mapping.JavaTypeTest$Abs1Test10<java.lang.Object, jp.co.future.uroborosql.mapping.JavaTypeTest$Abs1Test10, jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10>"));
		assertThat(javaType.getParam(0).toString(), is("java.lang.Object"));
		assertThat(
				javaType.getParam(1).toString(),
				is(
						"jp.co.future.uroborosql.mapping.JavaTypeTest$Abs1Test10<java.lang.Object, jp.co.future.uroborosql.mapping.JavaTypeTest$Abs1Test10, jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10>"));
		assertThat(
				javaType.getParam(2).toString(),
				is("jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10<jp.co.future.uroborosql.mapping.JavaTypeTest$Abs2Test10>"));

	}

	class Test11 {
		public void test(final List<? extends Comparable<?>> list1, final List<? super Comparable<?>> list2) {

		}

		public <T, E extends Comparable<?> & List<T> & Queue<T>> void test2(final List<E> list) {

		}

		// public void test2(final List<? extends Comparable<?> & List<?>> list) {
		//
		// }
	}

	@Test
	public void test11() throws SecurityException, NoSuchMethodException {

		Method method = Test11.class.getMethod("test", List.class, List.class);
		JavaType javaType = JavaType.of(new ImplementClass(Test11.class),
				method.getParameters()[0].getParameterizedType());
		assertThat(javaType.toString(), is("java.util.List<? extends java.lang.Comparable>"));
		assertThat(javaType.getParam(0).toString(), is("? extends java.lang.Comparable<?>"));
		assertThat(javaType.getParam(0).getLower(), is(nullValue()));
		assertThat(javaType.getParam(0).getUpperBounds()[0].toString(), is("java.lang.Comparable<?>"));
		javaType = JavaType.of(new ImplementClass(Test11.class), method.getParameters()[1].getParameterizedType());
		assertThat(javaType.toString(), is("java.util.List<? super java.lang.Comparable>"));
		assertThat(javaType.getParam(0).toString(), is("? super java.lang.Comparable<?>"));
		assertThat(javaType.getParam(0).getLower().toString(), is("java.lang.Comparable<?>"));
		assertThat(javaType.getParam(0).getUpperBounds()[0].toString(), is("java.lang.Object"));

		method = Test11.class.getMethod("test2", List.class);
		javaType = JavaType.of(new ImplementClass(Test11.class), method.getParameters()[0].getParameterizedType());
		assertThat(javaType.toString(),
				is("java.util.List<E extends java.lang.Comparable & java.util.List & java.util.Queue>"));
		assertThat(
				javaType.getParam(0).toString(),
				is("E extends java.lang.Comparable<?> & java.util.List<java.lang.Object> & java.util.Queue<java.lang.Object>"));
		assertThat(javaType.getParam(0).getUpperBounds()[0].toString(), is("java.lang.Comparable<?>"));
		assertThat(javaType.getParam(0).getUpperBounds()[1].toString(), is("java.util.List<java.lang.Object>"));
		assertThat(javaType.getParam(0).getUpperBounds()[2].toString(), is("java.util.Queue<java.lang.Object>"));

	}

	class Test12 {
		public <C extends Comparable<?>, T extends C> void test(final List<? extends T> list1,
				final List<? super T> list2) {

		}
	}

	@Test
	public void test12() throws SecurityException, NoSuchMethodException {

		Method method = Test12.class.getMethod("test", List.class, List.class);
		JavaType javaType = JavaType.of(new ImplementClass(Test12.class),
				method.getParameters()[0].getParameterizedType());
		assertThat(javaType.toString(), is("java.util.List<? extends java.lang.Comparable>"));
		assertThat(javaType.getParam(0).toString(), is("? extends java.lang.Comparable<?>"));
		javaType = JavaType.of(new ImplementClass(Test12.class), method.getParameters()[1].getParameterizedType());
		assertThat(javaType.toString(), is("java.util.List<? super java.lang.Comparable>"));
		assertThat(javaType.getParam(0).toString(), is("? super java.lang.Comparable<?>"));

	}

	interface Test13 extends List {
	}

	@Test
	public void test13() throws SecurityException, NoSuchMethodException {
		JavaType javaType = JavaType.of(new ImplementClass(Test13.class), Test13.class.getGenericInterfaces()[0]);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Object>"));
		assertThat(javaType.getParam(0).toString(), is("java.lang.Object"));

		javaType = JavaType.of(new ImplementClass(Test13.class), Collection.class.getTypeParameters()[0]);
		assertThat(javaType.toString(), is("java.lang.Object"));

		javaType = JavaType.of(new ImplementClass(Test13.class), List.class.getGenericInterfaces()[0]);
		assertThat(javaType.toString(), is("java.util.Collection<java.lang.Object>"));
		assertThat(javaType.getParam(0).toString(), is("java.lang.Object"));
	}

	interface Test14 extends List<String> {
	}

	@Test
	public void test14() throws SecurityException, NoSuchMethodException {
		JavaType javaType = JavaType.of(new ImplementClass(Test14.class), Test14.class.getGenericInterfaces()[0]);
		assertThat(javaType.toString(), is("java.util.List<java.lang.String>"));
		assertThat(javaType.getParam(0).toString(), is("java.lang.String"));

		javaType = JavaType.of(new ImplementClass(Test14.class), Collection.class.getTypeParameters()[0]);
		assertThat(javaType.toString(), is("java.lang.String"));

		javaType = JavaType.of(new ImplementClass(Test14.class), List.class.getGenericInterfaces()[0]);
		assertThat(javaType.toString(), is("java.util.Collection<java.lang.String>"));
		assertThat(javaType.getParam(0).toString(), is("java.lang.String"));
	}

}
