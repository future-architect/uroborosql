package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.sql.Connection;
import java.text.ParseException;
import java.time.Month;
import java.util.Date;

import org.apache.commons.lang3.time.DateUtils;
import org.junit.Test;

public class BindParameterMapperManagerTest {

	@Test
	public void test() throws ParseException {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager();

		assertThat(parameterMapperManager.toJdbc(null, null), is(nullValue()));

		assertThat(parameterMapperManager.toJdbc(true, null), is(true));
		assertThat(parameterMapperManager.toJdbc((byte) 1, null), is((byte) 1));
		assertThat(parameterMapperManager.toJdbc((short) 1, null), is((short) 1));
		assertThat(parameterMapperManager.toJdbc(1, null), is(1));
		assertThat(parameterMapperManager.toJdbc(1L, null), is(1L));
		assertThat(parameterMapperManager.toJdbc(1F, null), is(1F));
		assertThat(parameterMapperManager.toJdbc(1D, null), is(1D));
		assertThat(parameterMapperManager.toJdbc(BigDecimal.TEN, null), is(BigDecimal.TEN));
		assertThat(parameterMapperManager.toJdbc("A", null), is("A"));

		assertThat(parameterMapperManager.toJdbc(new byte[] { 1, 2 }, null), is(new byte[] { 1, 2 }));

		assertThat(parameterMapperManager.toJdbc(new java.sql.Date(1), null), is(new java.sql.Date(1)));
		assertThat(parameterMapperManager.toJdbc(new java.sql.Time(1), null), is(new java.sql.Time(1)));
		assertThat(parameterMapperManager.toJdbc(new java.sql.Timestamp(1), null), is(new java.sql.Timestamp(1)));
		java.sql.Array array = newProxy(java.sql.Array.class);
		assertThat(parameterMapperManager.toJdbc(array, null), is(array));
		java.sql.Ref ref = newProxy(java.sql.Ref.class);
		assertThat(parameterMapperManager.toJdbc(ref, null), is(ref));
		java.sql.Blob blob = newProxy(java.sql.Blob.class);
		assertThat(parameterMapperManager.toJdbc(blob, null), is(blob));
		java.sql.Clob clob = newProxy(java.sql.Clob.class);
		assertThat(parameterMapperManager.toJdbc(clob, null), is(clob));
		java.sql.SQLXML sqlxml = newProxy(java.sql.SQLXML.class);
		assertThat(parameterMapperManager.toJdbc(sqlxml, null), is(sqlxml));

		java.sql.Struct struct = newProxy(java.sql.Struct.class);
		assertThat(parameterMapperManager.toJdbc(struct, null), is(struct));

		Object object = new Object();
		assertThat(parameterMapperManager.toJdbc(object, null), is(object));

		Date date = DateUtils.parseDate("2000/01/01", new String[] { "yyyy/MM/dd" });
		assertThat(parameterMapperManager.toJdbc(date, null), is(new java.sql.Timestamp(date.getTime())));
		assertThat(parameterMapperManager.toJdbc(Month.APRIL, null), is(4));
	}

	@Test
	public void testCustom() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager();

		parameterMapperManager.addMapper(new BindParameterMapper<String>() {

			@Override
			public Class<String> targetType() {
				return String.class;
			}

			@Override
			public Object toJdbc(final String original, final Connection connection,
					final BindParameterMapperManager parameterMapperManager) {
				return original.toLowerCase();
			}
		});
		assertThat(parameterMapperManager.toJdbc("S", null), is("s"));

		assertThat(parameterMapperManager.toJdbc(true, null), is(true));

	}

	@Test
	public void testExistsArrayMapper() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager();
		String[] array = { "A" };
		assertThat(parameterMapperManager.existsArrayMapper(array), is(false));

		parameterMapperManager.addMapper(new DateParameterMapper());
		parameterMapperManager.addMapper(new BindParameterMapper<Integer[]>() {

			@Override
			public Class<Integer[]> targetType() {
				return Integer[].class;
			}

			@Override
			public Object toJdbc(final Integer[] original, final Connection connection,
					final BindParameterMapperManager parameterMapperManager) {
				return null;
			}
		});
		parameterMapperManager.addMapper(new StringArrayParameterMapper());
		assertThat(parameterMapperManager.existsArrayMapper(array), is(true));
		assertThat(parameterMapperManager.existsArrayMapper("test"), is(false));
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

		I proxyInstance = (I) Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
				new Class<?>[] { interfaceType, ProxyContainer.class }, (proxy, method, args) -> {
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
