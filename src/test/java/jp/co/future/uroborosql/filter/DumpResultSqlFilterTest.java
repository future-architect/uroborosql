package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.testlog.TestAppender;
import jp.co.future.uroborosql.utils.StringUtils;

public class DumpResultSqlFilterTest {

	private SqlConfig config;

	private SqlAgent agent;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:DumpResultSqlFilterTest")).build();
		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new DumpResultSqlFilter());
		sqlFilterManager.initialize();

		agent = config.agent();

		String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}

		StringBuilder builder = new StringBuilder();
		builder.append("create table if not exists many_column_table (").append(System.lineSeparator());
		for (int i = 1; i <= 100; i++) {
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

	@After
	public void tearDown() throws Exception {
		agent.close();
	}

	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				String[] parts = line.split("\t");
				for (String part : parts) {
					String[] keyValue = part.split(":", 2);
					row.put(keyValue[0].toLowerCase(), StringUtils.isBlank(keyValue[1]) ? null : keyValue[1]);
				}
				ans.add(row);
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ans;
	}

	private void truncateTable(final Object... tables) {
		try {
			Arrays.asList(tables).stream().forEach(tbl -> {
				try {
					agent.updateWith("truncate table " + tbl.toString()).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					fail("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage());
				}
			});
		} catch (Exception ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	private void cleanInsert(final Path path) {
		List<Map<String, Object>> dataList = getDataFromFile(path);

		try {
			dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
					.forEach(tbl -> truncateTable(tbl));

			dataList.stream().forEach(map -> {
				try {
					agent.update(map.get("sql").toString()).paramMap(map).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					fail("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage());
				}
			});
		} catch (Exception ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@Test
	public void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<String> log = TestAppender.getLogbackLogs(() -> {
			SqlContext ctx = agent.contextFrom("example/select_product")
					.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName").param("_funcId", "testFunction").setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultSqlFilter", "testExecuteQueryFilter.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteQueryFilterManyColumn() throws Exception {
		// データのクリア
		agent.updateWith("truncate table many_column_table").count();

		// 結果の検証
		List<String> log = TestAppender.getLogbackLogs(() -> {
			SqlContext ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select * from many_column_table");

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultSqlFilter",
								"testExecuteQueryFilterManyColumn.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteQueryFilterOneColumn() throws Exception {
		// データのクリア
		agent.updateWith("truncate table many_column_table").count();

		// 結果の検証
		List<String> log = TestAppender.getLogbackLogs(() -> {
			SqlContext ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select col1 from many_column_table");

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultSqlFilter",
								"testExecuteQueryFilterOneColumn.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteQueryFilterManyColumnWithData() throws Exception {
		// データ投入
		StringBuilder builder = new StringBuilder();
		builder.append("insert into many_column_table").append(System.lineSeparator())
				.append("(").append(System.lineSeparator())
				.append("\t").append("col1");
		for (int i = 2; i <= 100; i++) {
			builder.append("\t").append(", col").append(i).append(System.lineSeparator());
		}
		builder.append(") values (").append(System.lineSeparator())
				.append("\t").append("/*col1*/").append(System.lineSeparator());
		for (int i = 2; i <= 100; i++) {
			builder.append("\t").append(", /*col").append(i).append("*/''").append(System.lineSeparator());
		}
		builder.append(")").append(System.lineSeparator());

		List<Map<String, Object>> params = new ArrayList<>();
		for (int i = 1; i <= 10; i++) {
			Map<String, Object> values = new HashMap<>();
			for (int j = 1; j <= 100; j++) {
				values.put("col" + j, "value" + i * j);
			}
			params.add(values);
		}
		agent.batchWith(builder.toString()).paramStream(params.stream()).count();

		// select 結果の検証
		List<String> log = TestAppender.getLogbackLogs(() -> {
			SqlContext ctx = config.context();
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
			ctx.setSql("select * from many_column_table");

			agent.query(ctx);
		});

		assertThat(
				log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultSqlFilter",
								"testExecuteQueryFilterManyColumnWithData.txt"),
						StandardCharsets.UTF_8)));
	}

	public void assertFile(final String expectedFilePath, final String actualFilePath) throws IOException {
		String expected = new String(Files.readAllBytes(Paths.get(expectedFilePath)), StandardCharsets.UTF_8);
		String actual = new String(Files.readAllBytes(Paths.get(actualFilePath)), StandardCharsets.UTF_8);

		assertEquals(expected, actual);
	}

}
