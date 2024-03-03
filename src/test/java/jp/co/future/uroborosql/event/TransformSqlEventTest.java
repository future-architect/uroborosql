package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SecretColumnEventSubscriberTest.Product;

public class TransformSqlEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterInitializeExecutionContextListener(this::afterInitializeExecutionContext);
				transformSqlListener(this::transformSql);
			}

			void afterInitializeExecutionContext(final AfterInitializeExecutionContextEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
			}

			void transformSql(final TransformSqlEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getSql(), containsString("PRODUCT"));
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
				transformSqlListener(this::transformSql);
			}

			void transformSql(final TransformSqlEvent event) {
				event.setSql(event.getSql() + System.lineSeparator() + "limit 1");
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var products = agent.query(Product.class)
				.collect();
		assertThat(products.size(), is(1));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
