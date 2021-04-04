package jp.co.future.uroborosql.coverage.reports.html;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.atomic.AtomicReference;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractAgent;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.coverage.CoverageHandler;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;

public class HtmlReportCoverageHandlerTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:HtmlReportCoverageHandlerTest;DB_CLOSE_DELAY=-1", null, null).build();

		try (var agent = config.agent()) {
			agent.updateWith("create table if not exists test ( \n id VARCHAR, name  VARCHAR \n )").count();
			agent.commit();
		}
	}

	@AfterEach
	public void tearDown() {
	}

	@Test
	public void testReport() throws Exception {
		var path = Paths.get("target", "coverage", "HtmlReportCoverageHandlerTest", "testReport");

		Files.deleteIfExists(path.resolve("example/select_test.html"));
		Files.deleteIfExists(path.resolve("covertest/test01.html"));
		Files.deleteIfExists(path.resolve("covertest/test02.html"));
		Files.deleteIfExists(path.resolve("covertest/test03.html"));

		// カバレッジ用インスタンスをクリア
		var field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		var ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		var before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (var agent = config.agent()) {
			agent.query("example/select_test").param("id", "A001").collect();

			agent.query("covertest/test01").param("id", 1).collect();
			agent.query("covertest/test01").collect();
			agent.query("covertest/test02").collect();
			agent.query("covertest/test03").collect();
		}
		var filter = new WrapContextSqlFilter("/* PREFIX */", "/* SUFFIX */",
				".*(FOR\\sUPDATE|\\.NEXTVAL).*");
		filter.initialize();
		config.getSqlFilterManager().addSqlFilter(filter);
		try (var agent = config.agent()) {
			agent.query("covertest/test01").param("id", 1).collect();

		}

		assertThat(Files.exists(path.resolve("example/select_test.html")), is(true));

		assertThat(Files.readAllLines(path.resolve("covertest/test01.html")).size(), is(97));
		assertThat(Files.readAllLines(path.resolve("covertest/test01_hash_1.html")).size(), is(99));
		assertThat(Files.readAllLines(path.resolve("covertest/test02.html")).size(), is(97));
		assertThat(Files.readAllLines(path.resolve("covertest/test03.html")).size(), is(121));
		assertThat(Files.readAllLines(path.resolve("example/select_test.html")).size(), is(76));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}

	@Test
	public void testReportNoBranch() throws Exception {
		var path = Paths.get("target", "coverage", "HtmlReportCoverageHandlerTest", "testReportNoBranch");

		Files.deleteIfExists(path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportNoBranch.html"));

		// カバレッジ用インスタンスをクリア
		var field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		var ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		var before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (var agent = config.agent()) {
			agent.query("covertest/HtmlReportCoverageHandlerTest/testReportNoBranch").collect();
		}

		assertThat(Files.readAllLines(path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportNoBranch.html"))
				.size(), is(61));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}

	@Test
	public void testReportLastNoBranch() throws Exception {
		var path = Paths.get("target", "coverage", "HtmlReportCoverageHandlerTest", "testReportLastNoBranch");

		Files.deleteIfExists(path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch.html"));

		// カバレッジ用インスタンスをクリア
		var field = AbstractAgent.class.getDeclaredField("coverageHandlerRef");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		var ref = (AtomicReference<CoverageHandler>) field.get(null);

		System.setProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir", path.toString());
		var before = ref.get();
		ref.set(new HtmlReportCoverageHandler());
		try (var agent = config.agent()) {
			agent.query("covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch").collect();
		}

		assertThat(
				Files.readAllLines(path.resolve("covertest/HtmlReportCoverageHandlerTest/testReportLastNoBranch.html"))
						.size(),
				is(70));

		ref.set(before);
		System.clearProperty(SqlAgent.KEY_SQL_COVERAGE + ".dir");
	}
}
