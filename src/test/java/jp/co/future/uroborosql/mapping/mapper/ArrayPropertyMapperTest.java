package jp.co.future.uroborosql.mapping.mapper;

import static jp.co.future.uroborosql.mapping.mapper.Helper.*;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.sql.SQLException;
import java.time.Clock;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.mapping.JavaType;

public class ArrayPropertyMapperTest {
	private Clock clock = null;

	@BeforeEach
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() throws NoSuchMethodException, SecurityException, SQLException {
		var mapper = new PropertyMapperManager(this.clock);
		mapper.addMapper(new ArrayPropertyMapper());
		var array = newDummyArray("a,b,c".split(","));
		assertThat(mapper.getValue(JavaType.of(String[].class), newResultSet("getArray", array), 1),
				is("a,b,c".split(",")));

		array = newDummyArray(new Number[] { 1, 2, 3 });
		assertThat(mapper.getValue(JavaType.of(Integer[].class), newResultSet(
				"getArray", array,
				"getObject", new Integer[] { 1, 2, 3 }), 1), is(new Integer[] { 1, 2, 3 }));

		assertThat(mapper.getValue(JavaType.of(Integer[].class), newResultSet("getArray", null), 1), is(nullValue()));

	}

	private java.sql.Array newDummyArray(final Object[] value) throws NoSuchMethodException, SecurityException {
		var target = java.sql.Array.class.getMethod("getArray");
		InvocationHandler handler = (proxy, method, args) -> {
			if (target.equals(method)) {
				return value;
			}
			return null;
		};
		return (java.sql.Array) Proxy.newProxyInstance(this.getClass().getClassLoader(),
				new Class[] { java.sql.Array.class }, handler);
	}
}
