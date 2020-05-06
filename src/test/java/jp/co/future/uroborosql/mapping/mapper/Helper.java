package jp.co.future.uroborosql.mapping.mapper;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.Types;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

class Helper {

	public static ResultSet newResultSet(final String methodName, final Object value)
			throws NoSuchMethodException, SecurityException {
		return newResultSet(new Object[] { methodName, value });
	}

	public static ResultSet newResultSet(final Object... defs) throws NoSuchMethodException, SecurityException {
		if (defs.length % 2 != 0) {
			throw new IllegalArgumentException();
		}

		Method wasNull = ResultSet.class.getMethod("wasNull");

		Map<Method, Object> values = new LinkedHashMap<>();
		for (int i = 0; i < defs.length; i += 2) {
			String methodName = defs[i].toString();
			Method target = methodName.equals("wasNull") ? wasNull
					: ResultSet.class.getMethod(methodName, int.class);
			Object value = defs[i + 1];
			values.put(target, value);
			if (!methodName.equals("wasNull") && value instanceof String) {
				values.put(ResultSet.class.getMethod("getString", int.class), value);
			}
		}

		AtomicBoolean flg = new AtomicBoolean(false);

		return (ResultSet) Proxy.newProxyInstance(Helper.class.getClassLoader(), new Class[] { ResultSet.class },
				(proxy, method, args) -> {
					if (method.getName().equals("getMetaData")) {
						return newResultSetMetaData(new ArrayList<>(values.values()));
					}
					if (values.containsKey(method)) {
						Object o = values.get(method);
						flg.set(o == null);
						return o;
					}
					if (wasNull.equals(method)) {
						return flg.get();
					}

					return null;
				});
	}

	private static ResultSetMetaData newResultSetMetaData(final List<Object> values)
			throws NoSuchMethodException, SecurityException {
		return (ResultSetMetaData) Proxy.newProxyInstance(Helper.class.getClassLoader(),
				new Class[] { ResultSetMetaData.class },
				(proxy, method, args) -> {
					if (method.getName().equals("getColumnType")) {
						int columnIndex = (int) args[0] - 1;
						if (values.get(columnIndex) instanceof String) {
							return Types.VARCHAR;
						} else {
							return Types.JAVA_OBJECT;
						}
					} else {
						return null;
					}
				});
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
