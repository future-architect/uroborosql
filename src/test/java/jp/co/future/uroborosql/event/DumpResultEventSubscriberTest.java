package jp.co.future.uroborosql.event;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.testlog.TestAppender;

@TestInstance(Lifecycle.PER_CLASS)
public class DumpResultEventSubscriberTest extends BaseEventSubscriberTest {

	private SqlConfig config;

	private SqlAgent agent;

	@BeforeAll
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:DumpResultEventSubscriberTest"))
				.addSubscriber(new DumpResultEventSubscriber())
				.build();
		agent = config.agent();

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

		createTables(agent);
	}

	@AfterAll
	public void tearDown() {
		agent.close();
	}

	@Test
	public void testExecuteQuery() throws Exception {
		cleanInsert(agent, Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.contextFrom("example/select_product")
					.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName").param("_funcId", "testFunction").setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEvent",
								"testExecuteQuery.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteQueryManyColumn() throws Exception {
		// データのクリア
		agent.updateWith("truncate table many_column_table").count();

		// 結果の検証
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select * from many_column_table");

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEvent",
								"testExecuteQueryManyColumn.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteQueryOneColumn() throws Exception {
		// データのクリア
		agent.updateWith("truncate table many_column_table").count();

		// 結果の検証
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select col1 from many_column_table");

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEvent",
								"testExecuteQueryOneColumn.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteQueryManyColumnWithData() throws Exception {
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
		agent.batchWith(builder.toString()).paramStream(params.stream()).count();

		// select 結果の検証
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select * from many_column_table");

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultEvent",
								"testExecuteQueryManyColumnWithData.txt"),
						StandardCharsets.UTF_8)));
	}
}
