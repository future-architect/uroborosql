package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

@SuppressWarnings({ "rawtypes", "unused" })
public class JavaTypeTest {

	class Test01 {
		String test;
	}

	@Test
	public void test01() throws NoSuchFieldException, SecurityException {
		Field field = Test01.class.getDeclaredField("test");
		JavaType javaType = JavaType.create(Arrays.asList(), field);
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
		JavaType javaType = JavaType.create(Arrays.asList(Abs1Test02.class, Test02.class), field);
		assertThat(javaType.toString(), is("java.math.BigDecimal"));

		javaType = JavaType.create(Arrays.asList(Abs1Test02.class), field);
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
		JavaType javaType = JavaType.create(Arrays.asList(Abs1Test03.class, Test03.class), field);
		assertThat(javaType.toString(), is("java.math.BigDecimal[]"));

		javaType = JavaType.create(Arrays.asList(Abs1Test03.class), field);
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
		JavaType javaType = JavaType.create(Arrays.asList(Abs1Test04.class, Test04.class), field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Number>"));

		javaType = JavaType.create(Arrays.asList(Abs1Test04.class), field);
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
		JavaType javaType = JavaType.create(Arrays.asList(Abs1Test05.class, Test05.class), field);
		assertThat(javaType.toString(), is("java.lang.Number[]"));

		javaType = JavaType.create(Arrays.asList(Abs1Test05.class), field);
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
		JavaType javaType = JavaType.create(Arrays.asList(Abs1Test06.class, Test06.class), field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Number>"));

		javaType = JavaType.create(Arrays.asList(Abs1Test06.class), field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Number>"));
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
		JavaType javaType = JavaType.create(Arrays.asList(Abs1Test07.class, Test07.class), field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Object>"));

		javaType = JavaType.create(Arrays.asList(Abs1Test07.class), field);
		assertThat(javaType.toString(), is("java.util.List<java.lang.Object>"));
	}
}
