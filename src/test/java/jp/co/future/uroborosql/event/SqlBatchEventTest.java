package jp.co.future.uroborosql.event;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;

public class SqlBatchEventTest extends AbstractDbTest {

	@Test
	void testValidateEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		// 事前条件
		truncateTable("PRODUCT");
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterSqlBatchListener(this::sqlBatch);
			}

			void sqlBatch(final AfterSqlBatchEvent event) {
				assertThat(event.getExecutionContext(), not(nullValue()));
				assertThat(event.getConnection(), not(nullValue()));
				var counts = new int[100];
				Arrays.fill(counts, 1);
				assertThat(event.getCounts(), is(counts));
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var count = agent.batch("example/insert_product")
				.paramStream(input.stream())
				.count();

		assertThat(count, is(100));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

	@Test
	void testModifyEventObj() {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		// 事前条件
		truncateTable("PRODUCT");
		var es = new EventSubscriber() {

			@Override
			public void initialize() {
				afterSqlBatchListener(this::sqlBatch);
			}

			void sqlBatch(final AfterSqlBatchEvent event) {
				var counts = new int[200];
				Arrays.fill(counts, 1);
				event.setCounts(counts);
			}
		};

		config.getEventListenerHolder().addEventSubscriber(es);

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var count = agent.batch("example/insert_product")
				.paramStream(input.stream())
				.count();

		assertThat(count, is(200));

		config.getEventListenerHolder().removeEventSubscriber(es);
	}

}
