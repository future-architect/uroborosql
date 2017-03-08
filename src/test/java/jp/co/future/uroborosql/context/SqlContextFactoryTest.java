package jp.co.future.uroborosql.context;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.context.test.TestConsts;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.parameter.Parameter;

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
		sqlContextFactory.setConstantClassNames(Arrays.asList(TestConsts.class.getName()));

		sqlContextFactory.initialize();

		Map<String, Parameter> constParameterMap = sqlContextFactory.getConstParameterMap();
		Set<String> set = constParameterMap.entrySet().stream().map(e -> e.getKey() + "=" + e.getValue().getValue())
				.collect(Collectors.toSet());

		assertThat(set, is(new HashSet<>(Arrays.asList("CLS_INT=1", "CLS_STRING=AAA"))));
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
