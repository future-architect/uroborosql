package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.testlog.TestAppender;

public class AuditLogEventSubscriberTest extends BaseEventSubscriberTest {

	private SqlConfig config;

	private SqlAgent agent;

	private AuditLogEventSubscriber auditLog;

	@BeforeEach
	public void setUp() throws Exception {
		auditLog = new AuditLogEventSubscriber();
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:AuditLogEventSubscriberTest"))
				.addSubscriber(auditLog)
				.build();
		agent = config.agent();
		createTables(agent);
	}

	@AfterEach
	public void tearDown() {
		agent.close();
	}

	@Test
	public void testExecuteQuery() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/select_product")
					.setSqlId("111")
					.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogEvent", "testExecuteQuery.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testSetAuditLogKey() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		auditLog.setFuncIdKey("_customFuncId").setUserNameKey("_customUserName");

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/select_product")
					.setSqlId("111")
					.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName1")
					.param("_funcId", "testFunction1")
					.param("_customUserName", "testUserName2")
					.param("_customFuncId", "testFunction2");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogEvent",
						"testExecuteQueryCustomParam.txt"),
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
		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogEvent", "testExecuteUpdate.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteBatch() throws Exception {
		truncateTable(agent, "product");
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/insert_product")
					.setSqlId("333")
					// 1件目
					.param("product_id", new BigDecimal(1))
					.param("product_name", "商品名1")
					.param("product_kana_name", "ショウヒンメイイチ")
					.param("jan_code", "1234567890123")
					.param("product_description", "1番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0))
					.addBatch()
					// 2件目
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
		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogEvent", "testExecuteBatch.txt"),
				StandardCharsets.UTF_8)));
	}
}
