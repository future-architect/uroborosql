package jp.co.future.uroborosql.parameter.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.Clock;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.util.Date;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.parameter.mapper.legacy.DateToStringParameterMapper;

public class BindParameterMapperManagerTest {
	private Clock clock;

	@Before
	public void setUp() {
		this.clock = Clock.systemDefaultZone();
	}

	@Test
	public void test() throws ParseException {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(this.clock);

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

		Date date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		assertThat(parameterMapperManager.toJdbc(date, null), is(new java.sql.Timestamp(date.getTime())));
		assertThat(parameterMapperManager.toJdbc(Month.APRIL, null), is(4));
	}

	@Test
	public void testWithCustom() throws ParseException {
		BindParameterMapperManager original = new BindParameterMapperManager(this.clock);
		original.addMapper(new EmptyStringToNullParameterMapper());
		DateToStringParameterMapper mapper = new DateToStringParameterMapper();
		original.addMapper(mapper);
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(original, this.clock);

		Date date = Date.from(LocalDate.parse("2000-01-01").atStartOfDay(this.clock.getZone()).toInstant());
		assertThat(parameterMapperManager.toJdbc(date, null), is("20000101"));

		assertThat(parameterMapperManager.canAcceptByStandard(date), is(true));

		parameterMapperManager.removeMapper(mapper);
		assertThat(parameterMapperManager.toJdbc(date, null), is(instanceOf(Timestamp.class)));
	}

	@Test
	public void testCustom() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(this.clock);

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
	public void testCustomWithClock() {
		BindParameterMapperManager parameterMapperManager = new BindParameterMapperManager(this.clock);

		parameterMapperManager.addMapper(new BindParameterMapperWithClock<String>() {
			private Clock clock;

			@Override
			public Class<String> targetType() {
				return String.class;
			}

			@Override
			public Object toJdbc(final String original, final Connection connection,
					final BindParameterMapperManager parameterMapperManager) {
				return original.toLowerCase();
			}

			@Override
			public Clock getClock() {
				return this.clock;
			}

			@Override
			public void setClock(final Clock clock) {
				this.clock = clock;
			}
		});
		assertThat(parameterMapperManager.toJdbc("S", null), is("s"));

		assertThat(parameterMapperManager.toJdbc(true, null), is(true));

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
