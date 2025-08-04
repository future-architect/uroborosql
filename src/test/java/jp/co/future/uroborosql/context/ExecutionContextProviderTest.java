package jp.co.future.uroborosql.context;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.ResultSet;
import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.event.Level;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.test.TestConsts;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.context.test.TestManyConsts;

public class ExecutionContextProviderTest {

	private SqlConfig sqlConfig;

	private ExecutionContextProvider executionContextProvider;

	@BeforeEach
	public void setUp() throws Exception {
		sqlConfig = UroboroSQL
				.builder("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1", "sa", "sa").build();
		executionContextProvider = sqlConfig.getExecutionContextProvider();
	}

	@Test
	void testConst_class() {
		executionContextProvider.addBindParamMapper((original, connection, parameterMapperManager) -> null);

		executionContextProvider.setConstantClassNames(List.of(TestConsts.class.getName()));

		executionContextProvider.initialize();

		var constParameterMap = executionContextProvider.getConstParameterMap();
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
				"CLS_IGNORE", TestConsts.IGNORE,

				"CLS_CUSTOMMAPPER_TARGET", TestConsts.CUSTOMMAPPER_TARGET

		)));
	}

	@Test
	public void testManyConst_class_performance() {
		executionContextProvider.addBindParamMapper((original, connection, parameterMapperManager) -> null);

		executionContextProvider.setConstantClassNames(List.of(TestManyConsts.class.getName()));

		executionContextProvider.initialize();

		try (var agent = sqlConfig.agent()) {
			var startTime = Instant.now(sqlConfig.getClock());

			IntStream.rangeClosed(1, 100000)
					.forEach(idx -> agent.queryWith("select 1"));

			// 実行結果が1秒以内で終了すること（パフォーマンス改善前は15秒かかっている）
			var elapsedTime = Duration.between(startTime, Instant.now(sqlConfig.getClock())).getSeconds();
			assertThat(elapsedTime < 1L, is(true));
		}
	}

	private Map<String, ?> mapOf(final Object... args) {
		Map<String, Object> map = new HashMap<>();
		for (var i = 0; i < args.length; i += 2) {
			map.put((String) args[i], args[i + 1]);
		}
		return map;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	void testConst_enum() {
		executionContextProvider.setEnumConstantPackageNames(List.of(TestEnum1.class.getPackage().getName()));

		executionContextProvider.initialize();

		var constParameterMap = executionContextProvider.getConstParameterMap();
		Set<String> set = constParameterMap.entrySet().stream().map(e -> e.getKey() + "=" + e.getValue().getValue())
				.collect(Collectors.toSet());

		assertThat(
				set,
				is(new HashSet<>(List.of("CLS_TEST_ENUM1_A=A", "CLS_TEST_ENUM1_B=B", "CLS_TEST_ENUM1_C=C",
						"CLS_PACKTEST_TEST_ENUM2_D=D", "CLS_PACKTEST_TEST_ENUM2_E=E", "CLS_PACKTEST_TEST_ENUM2_F=F",
						"CLS_PACKTEST_TEST_ENUM2_INNER_G=G", "CLS_PACKTEST_TEST_ENUM2_INNER_H=H",
						"CLS_PACKTEST_TEST_ENUM2_INNER_I=I"))));

		for (var parameter : constParameterMap.values()) {
			assertThat(parameter.getValue(), isA((Class) Enum.class));
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	void testConst_enumForJar() {
		executionContextProvider.setEnumConstantPackageNames(List.of(Level.class.getPackage().getName()));

		executionContextProvider.initialize();

		var constParameterMap = executionContextProvider.getConstParameterMap();
		Set<String> set = constParameterMap.entrySet().stream().map(e -> e.getKey() + "=" + e.getValue().getValue())
				.collect(Collectors.toSet());

		assertThat(
				set,
				is(new HashSet<>(List.of("CLS_LEVEL_ERROR=ERROR", "CLS_LEVEL_DEBUG=DEBUG", "CLS_LEVEL_WARN=WARN",
						"CLS_LEVEL_TRACE=TRACE", "CLS_LEVEL_INFO=INFO"))));
		for (var parameter : constParameterMap.values()) {
			assertThat(parameter.getValue(), isA((Class) Enum.class));
		}
	}

	@Test
	void testSetDefaultResultSetType() throws Exception {
		executionContextProvider.initialize();

		executionContextProvider.setDefaultResultSetType(ResultSet.TYPE_FORWARD_ONLY);
		assertThat(executionContextProvider.createExecutionContext().getResultSetType(),
				is(ResultSet.TYPE_FORWARD_ONLY));
		executionContextProvider.setDefaultResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
		assertThat(executionContextProvider.createExecutionContext().getResultSetType(),
				is(ResultSet.TYPE_SCROLL_INSENSITIVE));
		executionContextProvider.setDefaultResultSetType(ResultSet.TYPE_SCROLL_SENSITIVE);
		assertThat(executionContextProvider.createExecutionContext().getResultSetType(),
				is(ResultSet.TYPE_SCROLL_SENSITIVE));
	}

	@Test
	void testSetDefaultResultSetConcurrency() throws Exception {
		executionContextProvider.initialize();

		executionContextProvider.setDefaultResultSetConcurrency(ResultSet.CONCUR_READ_ONLY);
		assertThat(executionContextProvider.createExecutionContext().getResultSetConcurrency(),
				is(ResultSet.CONCUR_READ_ONLY));
		executionContextProvider.setDefaultResultSetConcurrency(ResultSet.CONCUR_UPDATABLE);
		assertThat(executionContextProvider.createExecutionContext().getResultSetConcurrency(),
				is(ResultSet.CONCUR_UPDATABLE));
	}

}
