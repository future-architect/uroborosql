package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.Array;
import java.sql.Connection;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

public class IntArrayParameterMapperTest {
	private Clock clock = null;

	@Before
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(this.clock);
		Array jdbcArray = newProxy(Array.class);
		int[] array = { 111 };

		Connection conn = newProxy(Connection.class, (proxy, method, args) -> {
			if (method.getName().equals("createArrayOf")) {
				assertThat(args[0], is("INTEGER"));
				assertThat(args[1], is(array));
				return jdbcArray;
			}
			return method.invoke(proxy, args);
		});

		assertThat(parameterMapperManager.toJdbc(array, conn), is(jdbcArray));

		Object[] objArray = { 1, "A" };
		assertThat(parameterMapperManager.toJdbc(objArray, conn), is(objArray));

	}

	public static <I> I newProxy(final Class<I> interfaceType, final InvocationHandler handler) {
		return newProxy(interfaceType, new Class<?>[0], handler);
	}

	@SuppressWarnings("unchecked")
	public static <I> I newProxy(final Class<I> interfaceType, final Class<?>[] interfaceTypes,
			final InvocationHandler handler) {
		List<Class<?>> types = new ArrayList<>(Arrays.asList(interfaceTypes));
		types.add(interfaceType);
		return (I) Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
				types.toArray(new Class<?>[types.size()]), handler);
	}

	interface ProxyContainer {
		Object getOriginal();
	}

	private static <I> I newProxy(final Class<I> interfaceType) {
		Object o = new Object();

		Method getOriginal;
		try {
			getOriginal = ProxyContainer.class.getMethod("getOriginal");
		} catch (NoSuchMethodException | SecurityException e) {
			throw new AssertionError(e);
		}

		I proxyInstance = newProxy(interfaceType, new Class<?>[] { ProxyContainer.class }, (proxy, method, args) -> {
			if (getOriginal.equals(method)) {
				return o;
			}

			for (int i = 0; i < args.length; i++) {
				if (args[i] instanceof ProxyContainer) {
					args[i] = ((ProxyContainer) args[i]).getOriginal();
				}
			}
			return method.invoke(o, args);
		});
		return proxyInstance;
	}
}
