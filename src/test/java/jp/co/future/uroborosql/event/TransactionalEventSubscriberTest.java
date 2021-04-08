package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.time.LocalDate;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.testlog.TestAppender;

public class TransactionalEventSubscriberTest extends BaseEventSubscriberTest {

	private static final Logger LOG = LoggerFactory.getLogger(TransactionalEventSubscriberTest.class);

	private SqlConfig config;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:TransactionalEventSubscriberTest"))
				.addSubscriber(s -> s.doBeforeTransaction(e -> LOG.debug("BEFORE TX"))
						.doAfterTransaction(e -> LOG.debug("AFTER TX"))
						.doBeforeCommit(e -> LOG.debug("BEFORE COMMIT"))
						.doAfterCommit(e -> LOG.debug("AFTER COMMIT"))
						.doBeforeRollback(e -> LOG.debug("BEFORE ROLLBACK"))
						.doAfterRollback(e -> LOG.debug("AFTER ROLLBACK"))
						.doOnQuery(e -> {
							LOG.debug("SQL:{} executed.", e.getExecutionContext().getSqlName());
							return e.getResultSet();
						})
						.doOnUpdate(e -> {
							LOG.debug("SQL:{} executed. Count:{} items.", e.getExecutionContext().getSqlName(),
									e.getResult());
							return e.getResult();
						}))
				.build();

		createTables(config.agent());
		truncateTable(config.agent(), "PRODUCT", "PRODUCT_REGIST_WORK");
	}

	@Test
	public void testTransactionalEvents() throws Exception {
		var log = TestAppender.getLogbackLogs(() -> {
			try (var agent = config.agent()) {
				agent.required(() -> {
					cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
					agent.required(() -> {
						agent.query("example/select_product")
								.param("product_id", new BigDecimal(0))
								.collect();
					});

					agent.requiresNew(() -> {
						agent.update("example/insert_product_regist_work")
								.param("product_name", "test1")
								.param("product_kana_name", "test_kana1")
								.param("jan_code", "1234567890123")
								.param("product_description", "")
								.param("ins_datetime", LocalDate.now())
								.count();

						agent.setRollbackOnly();
					});
					var count = agent.queryWith("select * from product_regist_work").collect();
					assertThat(count.size(), is(1));
				});
			}
		});
		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/TransactionalEvent", "testTransactionalEvents.txt"),
						StandardCharsets.UTF_8)));
	}
}
