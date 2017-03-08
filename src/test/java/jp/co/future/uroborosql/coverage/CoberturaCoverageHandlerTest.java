package jp.co.future.uroborosql.coverage;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

import jp.co.future.uroborosql.AbstractAgent;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class CoberturaCoverageHandlerTest {
	private static final String DB_NAME = "coberturatestdb";

	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@Before
	public void setUp() throws Exception {
		String url = "jdbc:h2:file:./target/db/" + DB_NAME;
		config = DefaultSqlConfig.getConfig(url, "test", "test");

		try (SqlAgent agent = config.createAgent()) {

			SqlContext ctx = agent.contextWith("create table if not exists test ( \n id VARCHAR, name  VARCHAR \n )");
			agent.update(ctx);
		}
	}

	@After
	public void tearDown() {
	}

	@Test
	public void testReport() throws Exception {
		Path path = Paths.get("target", "coverage", "test-sql-cover.xml");
		Files.deleteIfExists(path);
		//カバレッジ用インスタンスをクリア
		Field field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		AtomicReference<CoverageHandler> ref = (AtomicReference<CoverageHandler>) field.get(null);
		if (ref == null) {
			ref = new AtomicReference<CoverageHandler>(new CoberturaCoverageHandler());
			field.set(null, ref);
		}

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE, "true");
		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".file", "target/coverage/test-sql-cover.xml");
		CoverageHandler before = ref.get();
		ref.set(new CoberturaCoverageHandler());
		try (SqlAgent agent = config.createAgent()) {
			agent.query("example/select_test").param("id", "A001").collect();

			agent.query("covertest/test01").param("id", 1).collect();
			agent.query("covertest/test02").collect();

		}

		assertThat(Files.exists(path), is(true));

		ref.set(before);
		System.clearProperty("sql.coverage.file");
	}

}
