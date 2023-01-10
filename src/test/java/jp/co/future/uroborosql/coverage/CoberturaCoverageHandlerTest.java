package jp.co.future.uroborosql.coverage;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.SqlAgentImpl;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.WrapContextEventSubscriber;

public class CoberturaCoverageHandlerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:CoberturaCoverageHandlerTest;DB_CLOSE_DELAY=-1", null, null).build();

		try (var agent = config.agent()) {
			agent.updateWith("create table if not exists test ( \n id VARCHAR, name  VARCHAR \n )").count();
			agent.commit();
		}
	}

	@AfterEach
	public void tearDown() {
	}

	@Test
	void testReport() throws Exception {
		var path = Paths.get("target", "coverage", "test-sql-cover.xml");
		Files.deleteIfExists(path);
		//カバレッジ用インスタンスをクリア
		var field = SqlAgentImpl.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		var ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE, "true");
		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".file", "target/coverage/test-sql-cover.xml");
		var before = ref.get();
		ref.set(new CoberturaCoverageHandler());
		try (var agent = config.agent()) {
			agent.query("example/select_test").param("id", "A001").collect();

			agent.query("covertest/test01").param("id", 1).collect();
			agent.query("covertest/test02").collect();
		}

		var filter = new WrapContextEventSubscriber("/* PREFIX */", "/* SUFFIX */",
				".*(FOR\\sUPDATE|\\.NEXTVAL).*");
		filter.initialize();
		config.getSqlFilterManager().addSqlFilter(filter);
		try (var agent = config.agent()) {
			agent.query("covertest/test01").param("id", 1).collect();

		}

		assertThat(Files.exists(path), is(true));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".file");
	}

}
