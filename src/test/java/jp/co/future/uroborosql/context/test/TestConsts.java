package jp.co.future.uroborosql.context.test;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Date;

public class TestConsts {
	public static final int INT = 1;
	public static final String STRING = "AAA";

	public static final boolean BOOLEAN = true;
	public static final byte BYTE = 1;
	public static final short SHORT = 1;
	public static final long LONG = 1;
	public static final double DOUBLE = 1;
	public static final float FLOAT = 1;
	public static final BigDecimal BIG_DECIMAL = BigDecimal.TEN;
	public static final byte[] BYTES = new byte[] { 1, 2 };

	public static final java.sql.Date SQL_DATE = new java.sql.Date(1);
	public static final java.sql.Time SQL_TIME = new java.sql.Time(2);
	public static final java.sql.Timestamp SQL_TIMESTAMP = new java.sql.Timestamp(3);
	public static final java.sql.Array SQL_ARRAY = newProxy(java.sql.Array.class);
	public static final java.sql.Ref SQL_REF = newProxy(java.sql.Ref.class);
	public static final java.sql.Blob SQL_BLOB = newProxy(java.sql.Blob.class);
	public static final java.sql.Clob SQL_CLOB = newProxy(java.sql.Clob.class);
	public static final java.sql.SQLXML SQL_SQLXML = newProxy(java.sql.SQLXML.class);
	public static final java.sql.Struct SQL_STRUCT = newProxy(java.sql.Struct.class);

	public static final RoundingMode ENUM = RoundingMode.CEILING;
	public static final LocalDate LOCAL_DATE = LocalDate.of(2000, 1, 1);
	public static final Date UTIL_DATE = new Date(10);

	public static final Object NULL = null;
	public static final Object IGNORE = new Object();
	public static final Object OBJECT_STR = "CCC";

	public static final Object CUSTOMMAPPER_TARGET = Arrays.asList(1, 2, 3);

	private static final String PRIVATE = "private";
	public final String notStatic = "not static";
	public static String NOT_FINAL = "not final";

	public final class InnerClass {
		public static final String ISTRING = "BBB";
	}

	public class NotFinalInnerClass {
		public static final String ISTRING = "BBB";
	}

	private final class PrivateInnerClass {
		public static final String ISTRING = "BBB";
	}

	public final class Overlap {
		public static final String OVERLAP_VAL = "重複テスト１";
	}

	public final class OverlapOverlap {
		public static final String VAL = "重複テスト２";
	}

	interface ProxyContainer {
		Object getOriginal();
	}

	@SuppressWarnings("unchecked")
	private static <I> I newProxy(final Class<I> interfaceType) {
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

					if (args != null) {
						for (int i = 0; i < args.length; i++) {
							if (args[i] instanceof ProxyContainer) {
								args[i] = ((ProxyContainer) args[i]).getOriginal();
							}
						}
					}
					return method.invoke(o, args);
				});
		return proxyInstance;
	}
}
