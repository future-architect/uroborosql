package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.testlog.TestAppender;

public class DumpResultEventSubscriberTest extends AbstractDbTest {
	private DumpResultEventSubscriber eventSubscriber;

	@BeforeEach
	public void setUpLocal() throws Exception {
		eventSubscriber = new DumpResultEventSubscriber();
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);

		var builder = new StringBuilder();
		builder.append("create table if not exists many_column_table (").append(System.lineSeparator());
		for (var i = 1; i <= 100; i++) {
			builder.append("col").append(i).append(" VARCHAR(100)");
			if (i < 100) {
				builder.append(",");
			}
			builder.append(System.lineSeparator());
		}
		builder.append(");");
		agent.updateWith(builder.toString()).count();
		agent.commit();
	}

	@AfterEach
	public void tearDownLocal() throws Exception {
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
	}

	@Test
	void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/select_product")
					.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEventSubscriber",
								"testExecuteQueryEvent.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteQueryFilterManyColumn() throws Exception {
		// データのクリア
		agent.updateWith("truncate table many_column_table").count();

		// 結果の検証
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select * from many_column_table");

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEventSubscriber",
								"testExecuteQueryEventManyColumn.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteQueryFilterOneColumn() throws Exception {
		// データのクリア
		agent.updateWith("truncate table many_column_table").count();

		// 結果の検証
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select col1 from many_column_table");

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEventSubscriber",
								"testExecuteQueryEventOneColumn.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteQueryFilterManyColumnWithData() throws Exception {
		// データ投入
		var builder = new StringBuilder();
		builder.append("insert into many_column_table").append(System.lineSeparator())
				.append("(").append(System.lineSeparator())
				.append("\t").append("col1");
		for (var i = 2; i <= 100; i++) {
			builder.append("\t").append(", col").append(i).append(System.lineSeparator());
		}
		builder.append(") values (").append(System.lineSeparator())
				.append("\t").append("/*col1*/").append(System.lineSeparator());
		for (var i = 2; i <= 100; i++) {
			builder.append("\t").append(", /*col").append(i).append("*/''").append(System.lineSeparator());
		}
		builder.append(")").append(System.lineSeparator());

		List<Map<String, Object>> params = new ArrayList<>();
		for (var i = 1; i <= 10; i++) {
			Map<String, Object> values = new HashMap<>();
			for (var j = 1; j <= 100; j++) {
				values.put("col" + j, "value" + i * j);
			}
			params.add(values);
		}
		agent.batchWith(builder.toString())
				.paramStream(params.stream())
				.count();

		// select 結果の検証
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select * from many_column_table");

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEventSubscriber",
								"testExecuteQueryEventManyColumnWithData.txt"),
						StandardCharsets.UTF_8)));
	}

}
