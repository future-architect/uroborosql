package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.model.Product;

public class EntityBatchUpdateEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityBatchUpdateListener(this::beforeEntityBatchUpdate);
				afterEntityBatchUpdateListener(this::afterEntityBatchUpdate);
			}

			void beforeEntityBatchUpdate(final BeforeEntityBatchUpdateEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntity(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
			}

			void afterEntityBatchUpdate(final AfterEntityBatchUpdateEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getEntityList(), not(nullValue()));
				assertThat(event.getEntityType(), is(Product.class));
				assertThat(event.getCounts().length, is(10));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var fixedDate = Date.from(LocalDate.of(2023, 8, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
		var results = agent.insertsAndReturn(IntStream.rangeClosed(11, 20)
				.mapToObj(idx -> new Product(idx, "商品" + idx, "ショウヒンカナ" + idx, "12345678901" + idx, "商品説明" + idx,
						fixedDate, fixedDate, 0)),
				InsertsType.BATCH);

		var count = agent.updates(results.map(product -> {
			product.setProductDescription(product.getProductDescription() + "更新");
			return product;
		}));

		assertThat(count, is(10));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				beforeEntityBatchUpdateListener(this::beforeEntityBatchUpdate);
			}

			void beforeEntityBatchUpdate(final BeforeEntityBatchUpdateEvent event) {
				var nowDate = Date.from(LocalDate.now().atStartOfDay(ZoneId.systemDefault()).toInstant());

				var product = (Product) event.getEntity();
				product.setUpdDatetime(nowDate);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var fixedDate = Date.from(LocalDate.of(2023, 8, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
		var results = agent.insertsAndReturn(IntStream.rangeClosed(11, 20)
				.mapToObj(idx -> new Product(idx, "商品" + idx, "ショウヒンカナ" + idx, "12345678901" + idx, "商品説明" + idx,
						fixedDate, fixedDate, 0)),
				InsertsType.BATCH);

		var updateResults = agent.updatesAndReturn(results.map(product -> {
			product.setProductDescription(product.getProductDescription() + "更新");
			return product;
		}));

		updateResults.forEach(result -> {
			assertThat(result.getProductDescription(), containsString("更新"));
			assertThat(result.getInsDatetime(), is(fixedDate));
			assertThat(result.getUpdDatetime().after(fixedDate), is(true));
		});

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyCountEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterEntityBatchUpdateListener(this::afterEntityBatchUpdate);
			}

			void afterEntityBatchUpdate(final AfterEntityBatchUpdateEvent event) {
				event.setCounts(new int[] { 1, 2, 3, 4 });
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		var fixedDate = Date.from(LocalDate.of(2023, 8, 1).atStartOfDay(ZoneId.systemDefault()).toInstant());
		var results = agent.insertsAndReturn(IntStream.rangeClosed(11, 20)
				.mapToObj(idx -> new Product(idx, "商品" + idx, "ショウヒンカナ" + idx, "12345678901" + idx, "商品説明" + idx,
						fixedDate, fixedDate, 0)),
				InsertsType.BATCH);

		var count = agent.updates(results.map(product -> {
			product.setProductDescription(product.getProductDescription() + "更新");
			return product;
		}));

		assertThat(count, is(10));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
