package jp.co.future.uroborosql.coverage.reports.html;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.AbstractAgent;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.coverage.CoverageHandler;

public class HtmlReportCoverageHandlerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig("jdbc:h2:mem:coberturatestdb;DB_CLOSE_DELAY=-1;", "test", "test");

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
		Path path = Paths.get("target", "coverage", "testsql");

		Files.deleteIfExists(path.resolve("example/select_test.html"));
		Files.deleteIfExists(path.resolve("covertest/test01.html"));
		Files.deleteIfExists(path.resolve("covertest/test02.html"));
		Files.deleteIfExists(path.resolve("covertest/test03.html"));

		//カバレッジ用インスタンスをクリア
		Field field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		AtomicReference<CoverageHandler> ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		CoverageHandler before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (SqlAgent agent = config.createAgent()) {
			agent.query("example/select_test").param("id", "A001").collect();

			agent.query("covertest/test01").param("id", 1).collect();
			agent.query("covertest/test02").collect();
			agent.query("covertest/test03").collect();

		}

		assertThat(Files.exists(path.resolve("example/select_test.html")), is(true));
		assertThat(Files.exists(path.resolve("covertest/test01.html")), is(true));
		assertThat(Files.exists(path.resolve("covertest/test02.html")), is(true));
		assertThat(Files.exists(path.resolve("covertest/test03.html")), is(true));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}

}
