package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.file.Paths;
import java.sql.SQLException;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SecretColumnEventSubscriberTest.Product;

public class CreatePreparedStatementEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterCreatePreparedStatementListener(this::afterCreatePreparedStatement);
			}

			void afterCreatePreparedStatement(final AfterCreatePreparedStatementEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getPreparedStatement(), not(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var products = agent.query(Product.class)
				.collect();
		assertThat(products.size(), is(2));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterCreatePreparedStatementListener(this::afterCreatePreparedStatement);
			}

			void afterCreatePreparedStatement(final AfterCreatePreparedStatementEvent event) {
				var sql = event.getExecutionContext().getExecutableSql();
				try {
					var stmt = event.getConnection().prepareStatement(sql + System.lineSeparator() + "limit 1");
					event.setPreparedStatement(stmt);
				} catch (SQLException ex) {
					fail();
				}
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var products = agent.query(Product.class)
				.collect();
		assertThat(products.size(), is(1));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
