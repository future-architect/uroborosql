package jp.co.future.uroborosql.context;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.context.test.TestConsts;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.parameter.Parameter;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.event.Level;

public class SqlContextFactoryTest {

	private SqlContextFactory sqlContextFactory;

	private SqlFilterManager sqlFilterManager;

	@Before
	public void setUp() throws Exception {
		sqlFilterManager = new SqlFilterManagerImpl();

		sqlContextFactory = new SqlContextFactoryImpl();
		sqlContextFactory.setSqlFilterManager(sqlFilterManager);
	}

	@Test
	public void testConst_class() {
		sqlContextFactory.addBindParamMapper(new BindParameterMapper<Collection<?>>() {

			@Override
			public Object toJdbc(Collection<?> original, Connection connection, BindParameterMapperManager parameterMapperManager) {
				// テストでは使わない
				return null;
			}

		});

		sqlContextFactory.setConstantClassNames(Arrays.asList(TestConsts.class.getName()));

		sqlContextFactory.initialize();

		Map<String, Parameter> constParameterMap = sqlContextFactory.getConstParameterMap();
		Map<String, ?> map = constParameterMap.entrySet().stream()
				.collect(Collector.of(HashMap::new, (m, e) -> m.put(e.getKey(), e.getValue().getValue()), (m1, m2) -> {
					m1.putAll(m2);
					return m1;
				}));

		assertThat(map, is(mapOf(
				"CLS_STRING", "AAA",
				"CLS_INT", 1,
				"CLS_INNER_CLASS_ISTRING", TestConsts.InnerClass.ISTRING,
				// コンパイラによりバイトコード差異で安定しないためテストしない
				// "CLS_OVERLAP_OVERLAP_VAL", "重複テスト２",
				"CLS_BOOLEAN", TestConsts.BOOLEAN,
				"CLS_BYTE", TestConsts.BYTE,
				"CLS_SHORT", TestConsts.SHORT,
				"CLS_LONG", TestConsts.LONG,
				"CLS_DOUBLE", TestConsts.DOUBLE,
				"CLS_FLOAT", TestConsts.FLOAT,
				"CLS_BIG_DECIMAL", TestConsts.BIG_DECIMAL,
				"CLS_BYTES", TestConsts.BYTES,
				"CLS_SQL_DATE", TestConsts.SQL_DATE,
				"CLS_SQL_TIME", TestConsts.SQL_TIME,
				"CLS_SQL_TIMESTAMP", TestConsts.SQL_TIMESTAMP,
				"CLS_SQL_ARRAY", TestConsts.SQL_ARRAY,
				"CLS_SQL_REF", TestConsts.SQL_REF,
				"CLS_SQL_BLOB", TestConsts.SQL_BLOB,
				"CLS_SQL_CLOB", TestConsts.SQL_CLOB,
				"CLS_SQL_SQLXML", TestConsts.SQL_SQLXML,
				"CLS_SQL_STRUCT", TestConsts.SQL_STRUCT,
				"CLS_ENUM", TestConsts.ENUM,
				"CLS_LOCAL_DATE", TestConsts.LOCAL_DATE,
				"CLS_UTIL_DATE", TestConsts.UTIL_DATE,
				"CLS_NULL", TestConsts.NULL,
				"CLS_OBJECT_STR", TestConsts.OBJECT_STR,

				"CLS_CUSTOMMAPPER_TARGET", TestConsts.CUSTOMMAPPER_TARGET

		)));
	}

	private Map<String, ?> mapOf(Object... args) {
		Map<String, Object> map = new HashMap<>();
		for (int i = 0; i < args.length; i += 2) {
			map.put((String) args[i], args[i + 1]);
		}
		return map;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	public void testConst_enum() {
		sqlContextFactory.setEnumConstantPackageNames(Arrays.asList(TestEnum1.class.getPackage().getName()));

		sqlContextFactory.initialize();

		Map<String, Parameter> constParameterMap = sqlContextFactory.getConstParameterMap();
		Set<String> set = constParameterMap.entrySet().stream().map(e -> e.getKey() + "=" + e.getValue().getValue())
				.collect(Collectors.toSet());

		assertThat(
				set,
				is(new HashSet<>(Arrays.asList("CLS_TEST_ENUM1_A=A", "CLS_TEST_ENUM1_B=B", "CLS_TEST_ENUM1_C=C",
						"CLS_PACKTEST_TEST_ENUM2_D=D", "CLS_PACKTEST_TEST_ENUM2_E=E", "CLS_PACKTEST_TEST_ENUM2_F=F",
						"CLS_PACKTEST_TEST_ENUM2_INNER_G=G", "CLS_PACKTEST_TEST_ENUM2_INNER_H=H",
						"CLS_PACKTEST_TEST_ENUM2_INNER_I=I"))));

		for (Parameter parameter : constParameterMap.values()) {
			assertThat(parameter.getValue(), isA((Class) Enum.class));
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	public void testConst_enumForJar() {
		sqlContextFactory.setEnumConstantPackageNames(Arrays.asList(Level.class.getPackage().getName()));

		sqlContextFactory.initialize();

		Map<String, Parameter> constParameterMap = sqlContextFactory.getConstParameterMap();
		Set<String> set = constParameterMap.entrySet().stream().map(e -> e.getKey() + "=" + e.getValue().getValue())
				.collect(Collectors.toSet());

		assertThat(
				set,
				is(new HashSet<>(Arrays.asList("CLS_LEVEL_ERROR=ERROR", "CLS_LEVEL_DEBUG=DEBUG", "CLS_LEVEL_WARN=WARN",
						"CLS_LEVEL_TRACE=TRACE", "CLS_LEVEL_INFO=INFO"))));
		for (Parameter parameter : constParameterMap.values()) {
			assertThat(parameter.getValue(), isA((Class) Enum.class));
		}
	}
}
