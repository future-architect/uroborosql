package jp.co.future.uroborosql.coverage;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.ResultSet;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.AbstractAgent;
import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;

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
	public void tearDown() throws Exception {
	}

	@Test
	public void testReport() throws Exception {
		//カバレッジ用インスタンスをクリア
		Field field = AbstractAgent.class.getDeclaredField("coverageHandler");
		field.setAccessible(true);
		field.set(null, null);
		System.setProperty("sql.coverage", "true");
		try (SqlAgent agent = config.createAgent()) {
			try (ResultSet rs = agent.query("example/select_test").paramList("product_id", 0, 1, 2, 3).resultSet()) {

			}
		}

		//確実に書き込みをする
		CoberturaCoverageHandler coverageHandler = (CoberturaCoverageHandler) field.get(null);
		coverageHandler.write();

		assertThat(Files.exists(Paths.get("target", "coverage", "sql-clover.xml")), is(true));
	}

}
