package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.DebugEventSubscriber;
import jp.co.future.uroborosql.testlog.TestAppender;
import jp.co.future.uroborosql.utils.StringUtils;

public class DebugSqlFilterTest {

	private SqlConfig config;

	private SqlAgent agent;

	@BeforeEach
	public void setUp() throws SQLException, IOException {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:DebugSqlFilterTest")).build();
		var sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new DebugEventSubscriber());
		sqlFilterManager.initialize();

		agent = config.agent();

		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (var sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
	}

	@AfterEach
	public void tearDown() throws Exception {
		agent.close();
	}

	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				var parts = line.split("\t");
				for (var part : parts) {
					var keyValue = part.split(":", 2);
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
		var dataList = getDataFromFile(path);

		try {
			dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
					.forEach(this::truncateTable);

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
	void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/select_product").param("product_id", new BigDecimal("0"))
					.param("_userName", "testUserName").param("_funcId", "testFunction").setSqlId("111");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber", "testExecuteQueryFilter.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteUpdateFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/selectinsert_product").setSqlId("222")
					.param("_userName", "testUserName").param("_funcId", "testFunction")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR);
			agent.update(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber", "testExecuteUpdateFilter.txt"),
						StandardCharsets.UTF_8)));
	}

	@Test
	void testExecuteBatchFilter() throws Exception {
		truncateTable("product");
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		var log = TestAppender.getLogbackLogs(() -> {
			var ctx = agent.context().setSqlName("example/insert_product").setSqlId("333")
					.param("product_id", new BigDecimal(1)).param("product_name", "商品名1")
					.param("product_kana_name", "ショウヒンメイイチ").param("jan_code", "1234567890123")
					.param("product_description", "1番目の商品").param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch()
					.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
					.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
					.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0))
					.param("_userName", "testUserName").param("_funcId", "testFunction").addBatch();
			agent.batch(ctx);
		});

		assertThat(log,
				is(Files.readAllLines(
						Paths.get("src/test/resources/data/expected/DebugEventSubscriber", "testExecuteBatchFilter.txt"),
						StandardCharsets.UTF_8)));
	}

	public void assertFile(final String expectedFilePath, final String actualFilePath) throws IOException {
		var expected = new String(Files.readAllBytes(Paths.get(expectedFilePath)), StandardCharsets.UTF_8);
		var actual = new String(Files.readAllBytes(Paths.get(actualFilePath)), StandardCharsets.UTF_8);

		assertEquals(expected, actual);
	}

}
