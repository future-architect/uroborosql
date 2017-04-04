package jp.co.future.uroborosql.mapping.mapper;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

class Helper {

	public static ResultSet newResultSet(final String methodName, final Object value) throws NoSuchMethodException, SecurityException {
		return newResultSet(new Object[] { methodName, value });
	}

	public static ResultSet newResultSet(final Object... defs) throws NoSuchMethodException, SecurityException {
		if (defs.length % 2 != 0) {
			throw new IllegalArgumentException();
		}

		Method wasNull = ResultSet.class.getMethod("wasNull");

		Map<Method, Object> values = new HashMap<>();
		for (int i = 0; i < defs.length; i += 2) {
			Method target = defs[i].equals("wasNull") ? wasNull : ResultSet.class.getMethod(defs[i].toString(), int.class);
			values.put(target, defs[i + 1]);
		}

		AtomicBoolean flg = new AtomicBoolean(false);

		InvocationHandler handler = new InvocationHandler() {

			@Override
			public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
				if (values.containsKey(method)) {
					Object o = values.get(method);
					flg.set(o == null);
					return o;
				}
				if (wasNull.equals(method)) {
					return flg.get();
				}

				return null;
			}
		};
		return (ResultSet) Proxy.newProxyInstance(Helper.class.getClassLoader(), new Class[] { ResultSet.class }, handler);
	}

	interface ProxyContainer {
		Object getOriginal();
	}

	@SuppressWarnings("unchecked")
	public static <I> I newProxy(final Class<I> interfaceType) {
		Object o = new Object();

		Method getOriginal;
		try {
			getOriginal = ProxyContainer.class.getMethod("getOriginal");
		} catch (NoSuchMethodException | SecurityException e) {
			throw new AssertionError(e);
		}

		I proxyInstance = (I) Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(), new Class<?>[] {
				interfaceType, ProxyContainer.class }, (proxy, method, args) -> {
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
