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

public class EntityDeleteEventTest extends AbstractDbTest {

	@Test
	void testValidateWithEntityEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityDeleteListener(this::beforeEntityDelete);
				afterEntityDeleteListener(this::afterEntityDelete);
			}

			void beforeEntityDelete(final BeforeEntityDeleteEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityDelete(final AfterEntityDeleteEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
				assertThat(event.getCount(), is(1));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var product = agent.query(Product.class)
				.equal("productId", 1)
				.first()
				.orElseThrow();

		var count = agent.delete(product);

		assertThat(count, is(1));

		assertThat(agent.query(Product.class).count(), is(0L));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateWithEntityClassEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityDeleteListener(this::beforeEntityDelete);
				afterEntityDeleteListener(this::afterEntityDelete);
			}

			void beforeEntityDelete(final BeforeEntityDeleteEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), is(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityDelete(final AfterEntityDeleteEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), is(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
				assertThat(event.getCount(), is(1));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var count = agent.delete(Product.class)
				.equal("productId", 1)
				.count();

		assertThat(count, is(1));

		assertThat(agent.query(Product.class).count(), is(0L));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyCountEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterEntityDeleteListener(this::afterEntityDelete);
			}

			void afterEntityDelete(final AfterEntityDeleteEvent event) {
				event.setCount(10);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var product = agent.query(Product.class)
				.equal("productId", 1)
				.first()
				.orElseThrow();

		var count = agent.delete(product);
		assertThat(count, is(10));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
