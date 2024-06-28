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

public class SqlUpdateEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				sqlUpdateListener(this::sqlUpdate);
			}

			void sqlUpdate(final SqlUpdateEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
				assertThat(event.getCount(), is(1));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var count = agent.update(Product.class).set("productName", "商品名_new").equal("productId", 1).count();
		assertThat(count, is(1));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				sqlUpdateListener(this::sqlUpdate);
			}

			void sqlUpdate(final SqlUpdateEvent event) {
				event.setCount(10);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var count = agent.update(Product.class).set("productName", "商品名_new").equal("productId", 1).count();
		assertThat(count, is(10));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
