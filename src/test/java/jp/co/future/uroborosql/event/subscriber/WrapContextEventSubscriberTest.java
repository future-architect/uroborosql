package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.SQLException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;

public class WrapContextEventSubscriberTest extends AbstractDbTest {
	private WrapContextEventSubscriber eventSubscriber;

	@BeforeEach
	public void setUpLocal() throws SQLException, IOException {
		eventSubscriber = new WrapContextEventSubscriber("select tmp_.* from (", ") tmp_ limit 10",
				".*(\\bcreate table|\\binsert|\\bupdate|\\btruncate|\\bfor update).*");
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
	}

	@AfterEach
	public void tearDownLocal() throws Exception {
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
	}

	@Test
	void testExecuteQueryEvent1() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", new BigDecimal("0"))
				.param("_userName", "testUserName")
				.param("_funcId", "testFunction")
				.setSqlId("111");

		agent.query(ctx);
		assertThat(ctx.getExecutableSql(), containsString("select tmp_"));
	}
}
