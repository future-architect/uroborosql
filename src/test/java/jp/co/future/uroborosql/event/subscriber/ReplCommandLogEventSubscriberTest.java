package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.testlog.TestAppender;

public class ReplCommandLogEventSubscriberTest extends AbstractDbTest {
	private ReplCommandLogEventSubscriber eventSubscriber;

	@BeforeEach
	public void setUpLocal() throws SQLException, IOException {
		eventSubscriber = new ReplCommandLogEventSubscriber();
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
	}

	@AfterEach
	public void tearDownLocal() throws Exception {
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
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
						Paths.get("src/test/resources/data/expected/ReplCommandLogEventSubscriber",
								"testExecuteQueryEvent.txt"),
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
						Paths.get("src/test/resources/data/expected/ReplCommandLogEventSubscriber",
								"testExecuteUpdateEvent.txt"),
						StandardCharsets.UTF_8)));
	}

}
