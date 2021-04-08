package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.testlog.TestAppender;

@TestInstance(Lifecycle.PER_CLASS)
public class DebugEventSubscriberTest extends BaseEventSubscriberTest {

	private SqlConfig config;

	private SqlAgent agent;

	@BeforeAll
	public void beforeClass() throws SQLException {
		var subscriber = new DebugEventSubscriber();
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:DebugEventSubscriberTest"))
				.addSubscriber(subscriber)
				.build();
	}

	@BeforeEach
	public void setUp() throws IOException {
		agent = config.agent();
		createTables(agent);
	}

	@AfterEach
	public void tearDown() {
		agent.close();
	}

	@Test
	public void debugQuery() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/select_product")
					.param("product_id", new BigDecimal("0"))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEvent", "testExecuteQuery.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteUpdate() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/selectinsert_product")
					.setSqlId("222")
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR);
			agent.update(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEvent", "testExecuteUpdate.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteBatch() throws Exception {
		truncateTable(agent, "product");
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/insert_product")
					.setSqlId("333")
					.param("product_id", new BigDecimal(1))
					.param("product_name", "商品名1")
					.param("product_kana_name", "ショウヒンメイイチ")
					.param("jan_code", "1234567890123")
					.param("product_description", "1番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0))
					.addBatch()
					.param("product_id", new BigDecimal(2))
					.param("product_name", "商品名2")
					.param("product_kana_name", "ショウヒンメイニ")
					.param("jan_code", "1234567890124")
					.param("product_description", "2番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.addBatch();
			agent.batch(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEvent", "testExecuteBatch.txt"),
						StandardCharsets.UTF_8)));
	}
}
