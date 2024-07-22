package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.model.Product;

public class EntityUpdateEventTest extends AbstractDbTest {

	@Test
	void testValidateWithEntityEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityUpdateListener(this::beforeEntityUpdate);
				afterEntityUpdateListener(this::afterEntityUpdate);
			}

			void beforeEntityUpdate(final BeforeEntityUpdateEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityUpdate(final AfterEntityUpdateEvent event) {
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
		product.setProductDescription(product.getProductDescription() + "更新");

		var count = agent.update(product);

		assertThat(count, is(1));

		assertThat(agent.query(Product.class)
				.select("productDescription", String.class)
				.findFirst().orElseThrow(), is("1番目の商品更新"));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testValidateWithEntityClassEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityUpdateListener(this::beforeEntityUpdate);
				afterEntityUpdateListener(this::afterEntityUpdate);
			}

			void beforeEntityUpdate(final BeforeEntityUpdateEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), is(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityUpdate(final AfterEntityUpdateEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), is(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
				assertThat(event.getCount(), is(1));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var count = agent.update(Product.class)
				.equal("productId", 1)
				.set("productDescription", "商品更新")
				.count();

		assertThat(count, is(1));

		assertThat(agent.query(Product.class)
				.select("productDescription", String.class)
				.findFirst().orElseThrow(), is("商品更新"));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityUpdateListener(this::beforeEntityUpdate);
			}

			void beforeEntityUpdate(final BeforeEntityUpdateEvent event) {
				var nowDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());

				var product = (Product) event.getEntity();
				product.setUpdDatetime(nowDate);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var product = agent.query(Product.class)
				.equal("productId", 1)
				.first()
				.orElseThrow();
		var updDateTime = product.getUpdDatetime();

		product.setProductDescription(product.getProductDescription() + "更新");

		var result = agent.updateAndReturn(product);

		assertThat(result.getInsDatetime(), is(product.getInsDatetime()));
		assertThat(result.getUpdDatetime().after(updDateTime), is(true));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyCountEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterEntityUpdateListener(this::afterEntityUpdate);
			}

			void afterEntityUpdate(final AfterEntityUpdateEvent event) {
				event.setCount(10);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var product = agent.query(Product.class)
				.equal("productId", 1)
				.first()
				.orElseThrow();

		product.setProductDescription(product.getProductDescription() + "更新");

		var count = agent.update(product);
		assertThat(count, is(10));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
