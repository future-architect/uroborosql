package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.Array;
import java.sql.Connection;
import java.time.Clock;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class LongWrapperArrayParameterMapperTest {
	private Clock clock = null;

	@BeforeEach
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() {
		var parameterMapperManager = new BindParameterMapperManager(clock);
		var jdbcArray = newProxy(Array.class);
		Long[] array = { Long.valueOf(111L), Long.valueOf(222L) };

		var conn = newProxy(Connection.class, (proxy, method, args) -> {
			if (method.getName().equals("createArrayOf")) {
				assertThat(args[0], is("BIGINT"));
				assertThat(args[1], is(array));
				return jdbcArray;
			}
			return method.invoke(proxy, args);
		});

		assertThat(parameterMapperManager.toJdbc(array, conn), is(jdbcArray));

		Object[] objArray = { Long.valueOf(333L), "A" };
		assertThat(parameterMapperManager.toJdbc(objArray, conn), is(objArray));

	}

	public static <I> I newProxy(final Class<I> interfaceType, final InvocationHandler handler) {
		return newProxy(interfaceType, new Class<?>[0], handler);
	}

	@SuppressWarnings("unchecked")
	public static <I> I newProxy(final Class<I> interfaceType, final Class<?>[] interfaceTypes,
			final InvocationHandler handler) {
		var types = new ArrayList<>(Arrays.asList(interfaceTypes));
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

			for (var i = 0; i < args.length; i++) {
				if (args[i] instanceof ProxyContainer) {
					args[i] = ((ProxyContainer) args[i]).getOriginal();
				}
			}
			return method.invoke(o, args);
		});
		return proxyInstance;
	}
}
