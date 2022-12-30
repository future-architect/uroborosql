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
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
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
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.testlog.TestAppender;
import jp.co.future.uroborosql.utils.StringUtils;

public class AuditLogSqlFilterTest {
	private SqlConfig config;

	private SqlAgent agent;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:AuditLogSqlFilterTest")).build();
		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
		sqlFilterManager.initialize();

		agent = config.agent();

		String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
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
			ExecutionContext ctx = agent.contextFrom("example/select_product").setSqlId("111")
					.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogSqlFilter", "testExecuteQueryFilter.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testSetAuditLogKey() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		AuditLogSqlFilter filter = (AuditLogSqlFilter) config.getSqlFilterManager().getFilters().get(0);
		filter.setFuncIdKey("_customFuncId").setUserNameKey("_customUserName");

		List<String> log = TestAppender.getLogbackLogs(() -> {
			ExecutionContext ctx = agent.contextFrom("example/select_product").setSqlId("111")
					.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("2")))
					.param("_userName", "testUserName1")
					.param("_funcId", "testFunction1")
					.param("_customUserName", "testUserName2")
					.param("_customFuncId", "testFunction2");
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			agent.query(ctx);
		});

		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogSqlFilter",
						"testExecuteQueryFilterCustomParam.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteUpdateFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			ExecutionContext ctx = agent.contextFrom("example/selectinsert_product").setSqlId("222")
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR);

			agent.update(ctx);
		});
		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogSqlFilter", "testExecuteUpdateFilter.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteBatchFilter() throws Exception {
		truncateTable("product");
		Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		List<String> log = TestAppender.getLogbackLogs(() -> {
			ExecutionContext ctx = agent.contextFrom("example/insert_product").setSqlId("333")
					// 1件目
					.param("product_id", new BigDecimal(1))
					.param("product_name", "商品名1")
					.param("product_kana_name", "ショウヒンメイイチ")
					.param("jan_code", "1234567890123")
					.param("product_description", "1番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0))
					.addBatch()
					// 2件目
					.param("product_id", new BigDecimal(2))
					.param("product_name", "商品名2")
					.param("product_kana_name", "ショウヒンメイニ")
					.param("jan_code", "1234567890124")
					.param("product_description", "2番目の商品")
					.param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0))
					.param("_userName", "testUserName")
					.param("_funcId", "testFunction")
					.addBatch();
			agent.batch(ctx);
		});
		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/data/expected/AuditLogSqlFilter", "testExecuteBatchFilter.txt"),
				StandardCharsets.UTF_8)));
	}

	public void assertFile(final String expectedFilePath, final String actualFilePath) throws IOException {
		String expected = new String(Files.readAllBytes(Paths.get(expectedFilePath)), StandardCharsets.UTF_8);
		String actual = new String(Files.readAllBytes(Paths.get(actualFilePath)), StandardCharsets.UTF_8);

		assertEquals(expected, actual);
	}
}
