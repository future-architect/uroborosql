package jp.co.future.uroborosql;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.utils.CaseFormat;
import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.*;
import java.util.*;
import java.util.Date;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

public class AbstractResultSetWrapperTest {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(AbstractResultSetWrapperTest.class);

	private SqlConfig config;

	private SqlAgent agent;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:AbstractResultSetWrapperTest", "sa", "sa").build();
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
		Arrays.asList(tables).stream().forEach(tbl -> {
			try {
				agent.updateWith("truncate table " + tbl.toString()).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				fail("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage());
			}
		});
	}

	private void cleanInsert(final Path path) {
		List<Map<String, Object>> dataList = getDataFromFile(path);

		dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
				.forEach(tbl -> truncateTable(tbl));

		dataList.forEach(map -> {
			try {
				agent.update(map.get("sql").toString()).paramMap(map).count();
			} catch (Exception ex) {
				ex.printStackTrace();
				fail("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage());
			}
		});
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryByResult() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product");
		ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

		ResultSet rs = agent.query(ctx);
		assertNotNull("ResultSetが取得できませんでした。", rs);
		assertTrue("結果が0件です。", rs.next());
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));

		AbstractResultSetWrapper wrapper = rs.unwrap(AbstractResultSetWrapper.class);
		assertEquals("検索件数が一致しません。", 1, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 1, wrapper.getRow());

		rs.next();
		assertEquals("1", rs.getString("PRODUCT_ID"));
		assertEquals("商品名1", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイイチ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890124", rs.getString("JAN_CODE"));
		assertEquals("1番目の商品", rs.getString("PRODUCT_DESCRIPTION"));

		assertEquals("検索件数が一致しません。", 2, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 2, wrapper.getRow());

		rs.next();

		assertEquals("検索件数が一致しません。", 2, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 0, wrapper.getRow());

		// カーソル操作
		rs.beforeFirst();
		assertEquals("検索件数が一致しません。", 0, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 0, wrapper.getRow());

		rs.first();
		assertEquals("検索件数が一致しません。", 1, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 1, wrapper.getRow());

		rs.last();
		assertEquals("検索件数が一致しません。", 2, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 2, wrapper.getRow());

		rs.afterLast();
		assertEquals("検索件数が一致しません。", 2, wrapper.getRowCount());
		assertEquals("検索件数が一致しません。", 0, wrapper.getRow());

	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (Stream<Map<String, Object>> stream = agent.query("example/select_product")
				.stream()) {
			stream.forEach((m) -> {
				assertTrue(m.containsKey("PRODUCT_ID"));
				assertTrue(m.containsKey("PRODUCT_NAME"));
				assertTrue(m.containsKey("PRODUCT_KANA_NAME"));
				assertTrue(m.containsKey("JAN_CODE"));
				assertTrue(m.containsKey("PRODUCT_DESCRIPTION"));
				assertTrue(m.containsKey("INS_DATETIME"));
				assertTrue(m.containsKey("UPD_DATETIME"));
				assertTrue(m.containsKey("VERSION_NO"));
			});
		}
	}
}