package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.testlog.TestAppender;

public class DebugEventSubscriberTest extends AbstractEventSubscriberTest {

	@BeforeEach
	public void setUpLocal() throws SQLException, IOException {
		config.getEventListenerHolder().addEventSubscriber(new DebugEventSubscriber());
	}

	@Test
	void testExecuteQueryEvent() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/select_product")
					.param("product_id", new BigDecimal("0"))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber",
								"testExecuteQueryEvent.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteTransactionEvent() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/select_product")
					.param("product_id", new BigDecimal("0"))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.requiresNew(() -> {
				try {
					agent.query(ctx);
				} catch (SQLException e) {
					fail();
				}
			});
		});

		// トランザクションイベントログではオブジェクトIDが出力されるため、固定値に置換する
		log = log.stream()
				.map(l -> l.replaceAll("@[\\w]{8}", "@XXXXXXXX"))
				.collect(Collectors.toList());

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber",
								"testExecuteQueryWithTxEvent.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteUpdateEvent() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/selectinsert_product")
					.setSqlId("222")
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR);
			agent.update(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber",
								"testExecuteUpdateEvent.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteBatchEvent() throws Exception {
		truncateTable("product");
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/insert_product")
					.setSqlId("333")
					.param("product_id", new BigDecimal(1))
					.param("product_name", "商品名1")
					.param("product_kana_name", "ショウヒンメイイチ")
					.param("jan_code", "1234567890123")
					.param("product_description", "1番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0)).addBatch()
					.param("product_id", new BigDecimal(2))
					.param("product_name", "商品名2")
					.param("product_kana_name", "ショウヒンメイニ")
					.param("jan_code", "1234567890124")
					.param("product_description", "2番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction").addBatch();
			agent.batch(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber",
								"testExecuteBatchEvent.txt"),
						StandardCharsets.UTF_8)));
	}

}
