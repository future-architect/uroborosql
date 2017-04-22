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
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.testlog.TestAppender;

public class DumpResultSqlFilterTest {

	private SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:DumpResultSqlFilterTest"));
		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new DumpResultSqlFilter());
		sqlFilterManager.initialize();

		try (SqlAgent agent = config.createAgent()) {

			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		}
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
		try (SqlAgent agent = config.createAgent()) {
			Arrays.asList(tables).stream().forEach(tbl -> {
				try {
					agent.updateWith("truncate table " + tbl.toString()).count();
				} catch (SQLException ex) {
					ex.printStackTrace();
					fail("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage());
				}
			});
		} catch (SQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	private void cleanInsert(final Path path) {
		List<Map<String, Object>> dataList = getDataFromFile(path);

		try (SqlAgent agent = config.createAgent()) {
			dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
					.forEach(tbl -> truncateTable(tbl));

			dataList.stream().forEach(map -> {
				try {
					agent.update(map.get("sql").toString()).paramMap(map).count();
				} catch (SQLException ex) {
					ex.printStackTrace();
					fail("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage());
				}
			});
		} catch (SQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@Test
	public void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<String> log = TestAppender.getLogbackLogs(() -> {
			try (SqlAgent agent = config.createAgent()) {
				SqlContext ctx = agent.contextFrom("example/select_product")
						.paramList("product_id", new BigDecimal("0"), new BigDecimal("2"))
						.param("_userName", "testUserName").param("_funcId", "testFunction").setSqlId("111");
				ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

				agent.query(ctx);
			}
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DumpResultSqlFilter", "testExecuteQueryFilter.txt"),
						StandardCharsets.UTF_8)));
	}

	public void assertFile(final String expectedFilePath, final String actualFilePath) throws IOException {
		String expected = new String(Files.readAllBytes(Paths.get(expectedFilePath)), StandardCharsets.UTF_8);
		String actual = new String(Files.readAllBytes(Paths.get(actualFilePath)), StandardCharsets.UTF_8);

		assertEquals(expected, actual);
	}

}
