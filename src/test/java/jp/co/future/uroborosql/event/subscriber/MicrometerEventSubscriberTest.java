package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.enums.SqlKind;

public class MicrometerEventSubscriberTest extends AbstractDbTest {
	private MicrometerEventSubscriber eventSubscriber;
	private MeterRegistry meterRegistry;

	@BeforeEach
	public void setUpLocal() throws Exception {
		meterRegistry = new SimpleMeterRegistry();
	}

	@AfterEach
	public void tearDownLocal() throws Exception {
		if (eventSubscriber != null) {
			config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
		}
	}

	@Test
	void testExecuteQuery() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// Add subscriber after data setup to avoid counting setup operations
		eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		var ctx = agent.context().setSqlName("example/select_product")
				.setSqlId("111")
				.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")));
		ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

		agent.query(ctx);

		// 実行回数カウンターを確認
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.SELECT.name());
		assertThat(counter.count(), is(1.0));

		// 実行時間タイマーを確認
		var timer = meterRegistry.timer("uroborosql.sql.duration",
				"sql.kind", SqlKind.SELECT.name());
		assertThat(timer.count(), is(1L));
		assertThat(timer.totalTime(java.util.concurrent.TimeUnit.NANOSECONDS) > 0, is(true));

		// 処理行数を確認
		var summaries = meterRegistry.find("uroborosql.sql.rows")
				.tag("sql.kind", SqlKind.SELECT.name())
				.summaries();
		assertThat(summaries.size(), is(1));
		assertThat(summaries.iterator().next().count(), is(1L));
		assertThat(summaries.iterator().next().totalAmount() >= 0, is(true));
	}

	@Test
	void testExecuteUpdate() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		// Add subscriber after data setup to avoid counting setup operations
		eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		var ctx = agent.context().setSqlName("example/selectinsert_product")
				.setSqlId("222")
				.param("product_id", new BigDecimal("0"))
				.param("jan_code", "1234567890123");

		agent.update(ctx);

		// 実行回数カウンターを確認
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.UPDATE.name());
		assertThat(counter.count(), is(1.0));

		// 実行時間タイマーを確認
		var timer = meterRegistry.timer("uroborosql.sql.duration",
				"sql.kind", SqlKind.UPDATE.name());
		assertThat(timer.count(), is(1L));
		assertThat(timer.totalTime(java.util.concurrent.TimeUnit.NANOSECONDS) > 0, is(true));

		// 処理行数を確認
		var summaries = meterRegistry.find("uroborosql.sql.rows")
				.tag("sql.kind", SqlKind.UPDATE.name())
				.summaries();
		assertThat(summaries.size(), is(1));
		assertThat(summaries.iterator().next().totalAmount(), is(1.0));
	}

	@Test
	void testExecuteBatch() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

// Add subscriber after data setup to avoid counting setup operations
eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
		truncateTable("PRODUCT");

		var ctx = agent.context().setSqlName("example/insert_product");
		ctx.param("product_id", new BigDecimal("3")).param("product_name", "test3").param("product_kana_name", "test3")
				.param("jan_code", "1234567890124").param("product_description", "test").param("ins_datetime",
						java.sql.Timestamp.valueOf("2005-12-12 10:10:10.000000000"))
				.param("upd_datetime", java.sql.Timestamp.valueOf("2005-12-12 10:10:10.000000000"))
				.param("version_no", new BigDecimal("1")).addBatch();
		ctx.param("product_id", new BigDecimal("4")).param("product_name", "test4").param("product_kana_name", "test4")
				.param("jan_code", "1234567890125").param("product_description", "test").param("ins_datetime",
						java.sql.Timestamp.valueOf("2005-12-12 10:10:10.000000000"))
				.param("upd_datetime", java.sql.Timestamp.valueOf("2005-12-12 10:10:10.000000000"))
				.param("version_no", new BigDecimal("1")).addBatch();

		agent.batch(ctx);

		// 実行回数カウンターを確認
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.BATCH_INSERT.name());
		assertThat(counter.count(), is(1.0));

		// 実行時間タイマーを確認
		var timer = meterRegistry.timer("uroborosql.sql.duration",
				"sql.kind", SqlKind.BATCH_INSERT.name());
		assertThat(timer.count(), is(1L));
		assertThat(timer.totalTime(java.util.concurrent.TimeUnit.NANOSECONDS) > 0, is(true));

		// 処理行数を確認
		var summaries = meterRegistry.find("uroborosql.sql.rows")
				.tag("sql.kind", SqlKind.BATCH_INSERT.name())
				.summaries();
		assertThat(summaries.size(), is(1));
		assertThat(summaries.iterator().next().totalAmount(), is(2.0));
	}

	@Test
	void testWithSqlNameTag() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

// Add subscriber after data setup to avoid counting setup operations
eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		eventSubscriber.setIncludeSqlNameTag(true);

		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")));
		ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

		agent.query(ctx);

		// SQL名タグを含むカウンターを確認
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.SELECT.name(),
				"sql.name", "example/select_product");
		assertThat(counter.count(), is(1.0));
	}

	@Test
	void testWithSqlIdTag() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

// Add subscriber after data setup to avoid counting setup operations
eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		eventSubscriber.setIncludeSqlIdTag(true);

		var ctx = agent.context().setSqlName("example/select_product")
				.setSqlId("test_id_001")
				.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")));
		ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

		agent.query(ctx);

		// SQL-IDタグを含むカウンターを確認
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.SELECT.name(),
				"sql.id", "test_id_001");
		assertThat(counter.count(), is(1.0));
	}

	@Test
	void testWithRowCountDisabled() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

// Add subscriber after data setup to avoid counting setup operations
eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		eventSubscriber.setIncludeRowCount(false);

		var ctx = agent.context().setSqlName("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"))
				.param("jan_code", "1234567890123");

		agent.update(ctx);

		// 実行回数カウンターは確認できる
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.UPDATE.name());
		assertThat(counter.count(), is(1.0));

		// 処理行数のsummaryは存在しない
		var summaries = meterRegistry.find("uroborosql.sql.rows")
				.tag("sql.kind", SqlKind.UPDATE.name())
				.summaries();
		assertThat(summaries.size(), is(0));
	}

	@Test
	void testMultipleExecutions() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

// Add subscriber after data setup to avoid counting setup operations
eventSubscriber = new MicrometerEventSubscriber(meterRegistry);
config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		// 複数回実行
		for (var i = 0; i < 3; i++) {
			var ctx = agent.context().setSqlName("example/select_product")
					.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")));
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			agent.query(ctx);
		}

		// 実行回数カウンターを確認
		var counter = meterRegistry.counter("uroborosql.sql.executions",
				"sql.kind", SqlKind.SELECT.name());
		assertThat(counter.count(), is(3.0));

		// 実行時間タイマーを確認
		var timer = meterRegistry.timer("uroborosql.sql.duration",
				"sql.kind", SqlKind.SELECT.name());
		assertThat(timer.count(), is(3L));
	}
}
