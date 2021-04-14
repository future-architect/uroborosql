package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.event.ResultEvent.QueryResultEvent;
import jp.co.future.uroborosql.testlog.TestAppender;

public class EventSubscriberRegisterTest extends BaseEventSubscriberTest {

	private static final Logger LOG = LoggerFactory.getLogger(EventSubscriberRegisterTest.class);

	private static final List<String> EXPECTED_LOG = List.of(
			"doQuery@subscriber1",
			"doQuery@subscriber2",
			"doQuery@subscriber3");

	private final EventSubscriber subscriber1 = new EventSubscriber() {
		@Override
		public ResultSet doQuery(final QueryResultEvent event) throws SQLException {
			LOG.debug("doQuery@subscriber1");
			return event.getResultSet();
		}
	};

	private final EventSubscriber subscriber2 = new EventSubscriber() {
		@Override
		public ResultSet doQuery(final QueryResultEvent event) throws SQLException {
			LOG.debug("doQuery@subscriber2");
			return event.getResultSet();
		}
	};

	@BeforeEach
	public void setUp() throws Exception {
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:EventSubscriberRegisterTest")).build();
		createTables(config.agent());
		cleanInsert(config.agent(), Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
	}

	@Test
	public void testEventRegisterOnBuilder() throws Exception {

		var log = TestAppender.getLogbackLogs(() -> {
			var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:EventSubscriberRegisterTest"))
					.addSubscriber(subscriber1)
					.addSubscriber(subscriber2)
					.addSubscriber(s -> s.doOnQuery(e -> {
						LOG.debug("doQuery@subscriber3");
						return e.getResultSet();
					}))
					.build();

			try (var agent = config.agent()) {
				agent.query("example/select_product")
						.param("product_id", new BigDecimal(0))
						.collect();
			}
		});
		assertThat(log, is(EXPECTED_LOG));

		log = TestAppender.getLogbackLogs(() -> {
			var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:EventSubscriberRegisterTest"))
					.addSubscriber(subscriber1)
					.clearSubscribers()
					.build();

			try (var agent = config.agent()) {
				agent.query("example/select_product")
						.param("product_id", new BigDecimal(0))
						.collect();
			}
		});
		assertThat(log, empty());
	}

	@Test
	public void testEventRegisterOnAgent() throws Exception {
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:EventSubscriberRegisterTest")).build();

		var log = TestAppender.getLogbackLogs(() -> {
			try (var agent = config.agent()) {
				agent.addSubscriber(subscriber1)
						.addSubscriber(subscriber2)
						.addSubscriber(s -> s.doOnQuery(e -> {
							LOG.debug("doQuery@subscriber3");
							return e.getResultSet();
						}))
						.query("example/select_product")
						.param("product_id", new BigDecimal(0))
						.collect();
			}
		});
		assertThat(log, is(EXPECTED_LOG));

		log = TestAppender.getLogbackLogs(() -> {
			try (var agent = config.agent()) {
				agent.clearSubscribers()
						.query("example/select_product")
						.param("product_id", new BigDecimal(0))
						.collect();
			}
		});
		assertThat(log, empty());
	}

	@Test
	public void testEventRegisterOnBoth() throws Exception {
		var config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:EventSubscriberRegisterTest"))
				.addSubscriber(subscriber1)
				.build();

		var log = TestAppender.getLogbackLogs(() -> {
			try (var agent = config.agent()) {
				agent.addSubscriber(subscriber2)
						.addSubscriber(s -> s.doOnQuery(e -> {
							LOG.debug("doQuery@subscriber3");
							return e.getResultSet();
						}))
						.query("example/select_product")
						.param("product_id", new BigDecimal(0))
						.collect();
			}
		});
		assertThat(log, is(EXPECTED_LOG));

		log = TestAppender.getLogbackLogs(() -> {
			try (var agent = config.agent()) {
				agent.clearSubscribers()
						.addSubscriber(subscriber2)
						.query("example/select_product")
						.param("product_id", new BigDecimal(0))
						.collect();
			}
		});
		assertThat(log, is(List.of("doQuery@subscriber2")));
	}
}
