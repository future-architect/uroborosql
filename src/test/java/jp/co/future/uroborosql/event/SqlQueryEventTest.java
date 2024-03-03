package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SecretColumnEventSubscriberTest.Product;

public class SqlQueryEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				sqlQueryListener(this::sqlQuery);
			}

			void sqlQuery(final SqlQueryEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
				assertThat(event.getResultSet(), not(nullValue()));
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
				sqlQueryListener(this::sqlQuery);
			}

			void sqlQuery(final SqlQueryEvent event) {
				event.setResultSet(event.getResultSet());
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var products = agent.query(Product.class)
				.collect();
		assertThat(products.size(), is(2));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
