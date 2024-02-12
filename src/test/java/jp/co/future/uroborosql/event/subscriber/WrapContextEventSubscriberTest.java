package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.SQLException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class WrapContextEventSubscriberTest extends AbstractEventSubscriberTest {
	private WrapContextEventSubscriber subscriber;

	@BeforeEach
	public void setUpLocal() throws SQLException, IOException {
		subscriber = new WrapContextEventSubscriber("select tmp_.* from (", ") tmp_ limit 10",
				".*(\\bcreate table|\\binsert|\\bupdate|\\btruncate|\\bfor update).*");
		config.getEventListenerHolder().addEventSubscriber(subscriber);
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
