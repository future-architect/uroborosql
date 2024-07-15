package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.model.Product;

public class EntityQueryEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityQueryListener(this::beforeEntityQuery);
				afterEntityQueryListener(this::afterEntityQuery);
			}

			void beforeEntityQuery(final BeforeEntityQueryEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), is(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityQuery(final AfterEntityQueryEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), is(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
				assertThat(event.getResults(), not(nullValue()));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var results = agent.query(Product.class)
				.asc("productId")
				.collect();

		assertThat(results.size(), is(2));
		assertThat(results.get(0).getProductId(), is(0));
		assertThat(results.get(1).getProductId(), is(1));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterEntityQueryListener(this::afterEntityQuery);
			}

			void afterEntityQuery(final AfterEntityQueryEvent event) {
				event.setResults(event.getResults()
						.map(res -> {
							var product = (Product) res;
							product.setProductId(product.getProductId() + 10);
							return product;
						}));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var results = agent.query(Product.class)
				.asc("productId")
				.collect();

		assertThat(results.size(), is(2));
		assertThat(results.get(0).getProductId(), is(10));
		assertThat(results.get(1).getProductId(), is(11));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}
}
