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

public class EntityInsertEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityInsertListener(this::beforeEntityInsert);
				afterEntityInsertListener(this::afterEntityInsert);
			}

			void beforeEntityInsert(final BeforeEntityInsertEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityInsert(final AfterEntityInsertEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
				assertThat(event.getCount(), is(1));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var fixedDate = Date.from(LocalDate.of(2023, 8, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
		var product = new Product(2, "追加商品", "ツイカショウヒン", "1234567890999", "追加した商品", fixedDate, fixedDate, 1);

		var count = agent.insert(product);

		assertThat(count, is(1));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityInsertListener(this::beforeEntityInsert);
			}

			void beforeEntityInsert(final BeforeEntityInsertEvent event) {
				var nowDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());

				var product = (Product) event.getEntity();
				product.setInsDatetime(nowDate);
				product.setUpdDatetime(nowDate);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var fixedDate = Date.from(LocalDate.of(2023, 8, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
		var product = new Product(2, "追加商品", "ツイカショウヒン", "1234567890999", "追加した商品", fixedDate, fixedDate, 1);

		var result = agent.insertAndReturn(product);

		assertThat(result.getInsDatetime().after(fixedDate), is(true));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyCountEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityInsertListener(this::beforeEntityInsert);
				afterEntityInsertListener(this::afterEntityInsert);
			}

			void beforeEntityInsert(final BeforeEntityInsertEvent event) {
				var nowDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());

				var product = (Product) event.getEntity();
				product.setInsDatetime(nowDate);
				product.setUpdDatetime(nowDate);
			}

			void afterEntityInsert(final AfterEntityInsertEvent event) {
				event.setCount(10);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var fixedDate = Date.from(LocalDate.of(2023, 8, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
		var product = new Product(2, "追加商品", "ツイカショウヒン", "1234567890999", "追加した商品", fixedDate, fixedDate, 1);

		var count = agent.insert(product);
		assertThat(count, is(10));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
