package jp.co.future.uroborosql;

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
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.ResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SqlAgentTest {
	private SqlConfig config;

	private SqlAgent agent;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SqlAgentTest"));
		agent = config.createAgent();
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

		dataList.stream().forEach(map -> {
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
	public void testQuery() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<BigDecimal> productIdList = new ArrayList<>();
		productIdList.add(new BigDecimal("0"));
		productIdList.add(new BigDecimal("2"));
		SqlContext ctx = agent.contextFrom("example/select_product").param("product_id", productIdList)
				.setSqlId("test_sql_id");

		ResultSet rs = agent.query(ctx);
		assertNotNull("ResultSetが取得できませんでした。", rs);
		assertTrue("結果が0件です。", rs.next());
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		assertFalse("結果が複数件です。", rs.next());
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryFilter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlFilterManager manager = config.getSqlFilterManager();
		WrapContextSqlFilter filter = new WrapContextSqlFilter("",
				"LIMIT /*$maxRowCount*/10 OFFSET /*$startRowIndex*/0", ".*(FOR\\sUPDATE|\\.NEXTVAL).*");
		filter.initialize();
		manager.addSqlFilter(filter);

		SqlContext ctx = agent.contextFrom("example/select_product")
				.paramList("product_id", new BigDecimal("0"), new BigDecimal("1")).param("startRowIndex", 0)
				.param("maxRowCount", 1);

		ResultSet rs = agent.query(ctx);
		assertNotNull("ResultSetが取得できませんでした。", rs);
		assertTrue("結果が0件です。", rs.next());
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		assertFalse("結果が複数件です。", rs.next());
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryArray() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product").paramList("product_id", 0, 1, 2, 3);

		ResultSet rs = agent.query(ctx);
		assertNotNull("ResultSetが取得できませんでした。", rs);
		assertTrue("結果が0件です。", rs.next());
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentArray() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		ResultSet rs = agent.query("example/select_product").paramList("product_id", 0, 1, 2, 3).resultSet();
		assertNotNull("ResultSetが取得できませんでした。", rs);
		assertTrue("結果が0件です。", rs.next());
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentCollect() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Map<String, Object>> ans = agent.query("example/select_product").paramList("product_id", 0, 1, 2, 3)
				.collect();
		assertEquals("結果の件数が一致しません。", 2, ans.size());
		Map<String, Object> map = ans.get(0);
		assertEquals(new BigDecimal("0"), map.get("PRODUCT_ID"));
		assertEquals("商品名0", map.get("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", map.get("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", map.get("JAN_CODE"));
		assertEquals("0番目の商品", map.get("PRODUCT_DESCRIPTION"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentCollectSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentFactory().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.createAgent();

		List<Map<String, Object>> ans = agent.query("example/select_product").paramList("product_id", 0, 1, 2, 3)
				.collect();
		assertEquals("結果の件数が一致しません。", 2, ans.size());
		Map<String, Object> map = ans.get(0);
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Map<String, Object> map = agent.query("example/select_product").paramList("product_id", 0, 1, 2, 3).first();
		assertEquals(new BigDecimal("0"), map.get("PRODUCT_ID"));
		assertEquals("商品名0", map.get("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", map.get("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", map.get("JAN_CODE"));
		assertEquals("0番目の商品", map.get("PRODUCT_DESCRIPTION"));
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFirstSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentFactory().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.createAgent();

		Map<String, Object> map = agent.query("example/select_product").paramList("product_id", 0, 1, 2, 3).first();
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.paramList("product_id", 0, 1, 2, 3).findFirst();
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("PRODUCT_ID"));
		assertEquals("商品名0", map.get("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", map.get("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", map.get("JAN_CODE"));
		assertEquals("0番目の商品", map.get("PRODUCT_DESCRIPTION"));

		optional = agent.query("example/select_product").paramList("product_id", 4).findFirst();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindFirstSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentFactory().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.createAgent();

		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.paramList("product_id", 0, 1, 2, 3).findFirst();
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));

		optional = agent.query("example/select_product").paramList("product_id", 4).findFirst();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFirstCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Map<String, Object> map = agent.query("example/select_product").paramList("product_id", 0, 1, 2, 3)
				.first(CaseFormat.LOWER_SNAKE_CASE);
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindFirstCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.paramList("product_id", 0, 1, 2, 3).findFirst(CaseFormat.PASCAL_CASE);
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("ProductId"));
		assertEquals("商品名0", map.get("ProductName"));
		assertEquals("ショウヒンメイゼロ", map.get("ProductKanaName"));
		assertEquals("1234567890123", map.get("JanCode"));
		assertEquals("0番目の商品", map.get("ProductDescription"));

		optional = agent.query("example/select_product").paramList("product_id", 4).findFirst();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFirstByClass() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Product product = agent.query("example/select_product")
				.paramList("product_id", 0, 1, 2, 3)
				.first(Product.class);

		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindFirstByClass() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Optional<Product> optional = agent.query("example/select_product")
				.paramList("product_id", 0, 1, 2, 3)
				.findFirst(Product.class);

		assertTrue(optional.isPresent());

		Product product = optional.get();

		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());

		optional = agent.query("example/select_product")
				.paramList("product_id", 4)
				.findFirst(Product.class);

		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product");
		ctx.paramList("product_id", 0, 1);

		agent.query(ctx, (rs) -> {
			ResultSetMetaData rsmd = rs.getMetaData();

			int columnCount = rsmd.getColumnCount();

			Map<String, String> record = new LinkedHashMap<>(columnCount);

			for (int i = 1; i <= columnCount; i++) {
				record.put(rsmd.getColumnLabel(i), rs.getString(i));
			}
			return record;
		}).forEach((m) -> {
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

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").paramList("product_id", 0, 1)
				.stream().forEach((m) -> {
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

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambdaSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentFactory().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.createAgent();

		agent.query("example/select_product").paramList("product_id", 0, 1)
				.stream().forEach((m) -> {
					assertTrue(m.containsKey("product_id"));
					assertTrue(m.containsKey("product_name"));
					assertTrue(m.containsKey("product_kana_name"));
					assertTrue(m.containsKey("jan_code"));
					assertTrue(m.containsKey("product_description"));
					assertTrue(m.containsKey("ins_datetime"));
					assertTrue(m.containsKey("upd_datetime"));
					assertTrue(m.containsKey("version_no"));
				});
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambdaWithParamBean() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		ProductSearchBean bean = new ProductSearchBean();
		bean.setProductIds(new int[] { 0, 1 });
		bean.setProductName("商品");

		agent.query("example/select_product_param_camel")
				.paramBean(bean)
				.stream().forEach((m) -> {
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

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentStreamEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product")
				.paramList("product_id", 0, 1)
				.stream(Product.class)
				.forEach(p -> {
					assertNotNull(p.getProductId());
					assertNotNull(p.getProductName());
					assertNotNull(p.getProductKanaName());
					assertNotNull(p.getJanCode());
					assertNotNull(p.getProductDescription());
					assertNotNull(p.getInsDatetime());
					assertNotNull(p.getUpdDatetime());
					assertNotNull(p.getVersionNo());
				});
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryMapResuletSetConverter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product");
		ctx.paramList("product_id", 0, 1);

		List<Map<String, Object>> ans = agent.query(ctx, CaseFormat.CAMEL_CASE);
		ans.forEach((m) -> {
			assertTrue(m.containsKey("productId"));
			assertTrue(m.containsKey("productName"));
			assertTrue(m.containsKey("productKanaName"));
			assertTrue(m.containsKey("janCode"));
			assertTrue(m.containsKey("productDescription"));
			assertTrue(m.containsKey("insDatetime"));
			assertTrue(m.containsKey("updDatetime"));
			assertTrue(m.containsKey("versionNo"));
		});

		SqlContext ctx2 = agent.contextFrom("example/select_product");
		ctx2.paramList("product_id", 0, 1);

		List<Map<String, Object>> ans2 = agent.query(ctx2, CaseFormat.UPPER_SNAKE_CASE);
		ans2.forEach((m) -> {
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

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambdaAndUpdate() throws Exception {
		// 事前条件
		truncateTable("product_regist_work");
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").paramList("product_id", 0, 1).stream(new CustomResultSetConverter())
				.forEach((m) -> {
					try {
						agent.update("example/insert_product_regist_work").paramMap(m).count();
					} catch (Exception e) {
						fail(e.getMessage());
					}
				});

		List<Map<String, Object>> collect = agent.queryWith("select * from product_regist_work").collect();
		assertEquals(2, collect.size());
	}

	/**
	 * DB更新処理のテストケース。
	 */
	@Test
	public void testExecuteUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		SqlContext ctx = agent.contextFrom("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR);

		int updateCount = agent.update(ctx);
		assertEquals("データの登録に失敗しました。", 1, updateCount);

		// 検証処理
		List<Map<String, Object>> expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.paramList("product_id", 0, 1).stream(new CustomResultSetConverter()).collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB更新処理のテストケース。(Fluent API)
	 */
	@Test
	public void testUpdateFluent() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		int updateCount = agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR).count();
		assertEquals("データの登録に失敗しました。", 1, updateCount);

		// 検証処理
		List<Map<String, Object>> expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.paramList("product_id", 0, 1).stream(new CustomResultSetConverter()).collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース。
	 */
	@Test
	public void testExecuteBatch() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

		SqlContext ctx = agent.contextFrom("example/insert_product").param("product_id", new BigDecimal(1))
				.param("product_name", "商品名1").param("product_kana_name", "ショウヒンメイイチ")
				.param("jan_code", "1234567890123").param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch().param("product_id", new BigDecimal(2))
				.param("product_name", "商品名2").param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890124").param("product_description", "2番目の商品")
				.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		int[] count = agent.batch(ctx);
		assertEquals("データの登録件数が不正です。", 2, count.length);
		assertEquals("1行目のデータの登録に失敗しました。", 1, count[0]);
		assertEquals("2行目のデータの登録に失敗しました。", 1, count[1]);

		// 検証処理
		List<Map<String, Object>> expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatch.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.paramList("product_id", 1, 2).stream(new CustomResultSetConverter()).collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース。(複数回)
	 */
	@Test
	public void testExecuteBatchRepeat() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

		SqlContext ctx = agent.contextFrom("example/insert_product").param("product_id", new BigDecimal(1))
				.param("product_name", "商品名1").param("product_kana_name", "ショウヒンメイイチ")
				.param("jan_code", "1234567890123").param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		int[] count = agent.batch(ctx);
		assertEquals("データの登録件数が不正です。", 1, count.length);
		assertEquals("1行目のデータの登録に失敗しました。", 1, count[0]);

		ctx.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch();

		count = agent.batch(ctx);
		assertEquals("データの登録件数が不正です。", 1, count.length);
		assertEquals("1行目のデータの登録に失敗しました。", 1, count[0]);

		// 検証処理
		List<Map<String, Object>> expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatch.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.paramList("product_id", 1, 2).stream(new CustomResultSetConverter()).collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理(Null挿入）のテストケース。
	 */
	@Test
	public void testExecuteBatchNull() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		SqlContext ctx = agent.contextFrom("example/insert_product").param("product_id", new BigDecimal(1))
				.param("product_name", null).param("product_kana_name", null).param("jan_code", "1234567890123")
				.param("product_description", "1番目の商品").param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch()
				.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch();

		int[] count = agent.batch(ctx);
		assertEquals("データの登録件数が不正です。", 2, count.length);
		assertEquals("1行目のデータの登録に失敗しました。", 1, count[0]);
		assertEquals("2行目のデータの登録に失敗しました。", 1, count[1]);

		List<Map<String, Object>> expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchNull.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.paramList("product_id", 1, 2).stream(new CustomResultSetConverter()).collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	@Test
	public void testExecuteBatchNoAddBatch() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		int[] count = agent.update("example/insert_product").batch();
		assertEquals("データの登録件数が不正です。", 0, count.length);
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	public void testNotFoundFile() throws Exception {
		try {
			SqlContext ctx = agent.contextFrom("file");
			agent.query(ctx);
			// 例外が発生しなかった場合
			fail();
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	private static class CustomResultSetConverter implements ResultSetConverter<Map<String, Object>> {
		@Override
		public Map<String, Object> createRecord(final ResultSet rs) throws SQLException {
			ResultSetMetaData rsmd = rs.getMetaData();
			int columnCount = rsmd.getColumnCount();
			Map<String, Object> record = new LinkedHashMap<>(columnCount);
			for (int i = 1; i <= columnCount; i++) {
				record.put(rsmd.getColumnLabel(i).toLowerCase(), rs.getObject(i));
			}
			return record;
		}
	}

	public static class Product {
		private int productId;
		private String productName;
		private String productKanaName;
		private String janCode;
		private String productDescription;
		private Date insDatetime;
		private Date updDatetime;
		private int versionNo;

		public Product() {
		}

		public int getProductId() {
			return productId;
		}

		public void setProductId(final int productId) {
			this.productId = productId;
		}

		public String getProductName() {
			return productName;
		}

		public void setProductName(final String productName) {
			this.productName = productName;
		}

		public String getProductKanaName() {
			return productKanaName;
		}

		public void setProductKanaName(final String productKanaName) {
			this.productKanaName = productKanaName;
		}

		public String getJanCode() {
			return janCode;
		}

		public void setJanCode(final String janCode) {
			this.janCode = janCode;
		}

		public String getProductDescription() {
			return productDescription;
		}

		public void setProductDescription(final String productDescription) {
			this.productDescription = productDescription;
		}

		public Date getInsDatetime() {
			return insDatetime;
		}

		public void setInsDatetime(final Date insDatetime) {
			this.insDatetime = insDatetime;
		}

		public Date getUpdDatetime() {
			return updDatetime;
		}

		public void setUpdDatetime(final Date updDatetime) {
			this.updDatetime = updDatetime;
		}

		public int getVersionNo() {
			return versionNo;
		}

		public void setVersionNo(final int versionNo) {
			this.versionNo = versionNo;
		}
	}

	public static class BaseProductSearchBean {
		private String productName;

		public String getProductName() {
			return this.productName;
		}

		public void setProductName(final String productName) {
			this.productName = productName;
		}
	}

	public static class ProductSearchBean extends BaseProductSearchBean {
		private int[] productIds;

		public int[] getProductIds() {
			return this.productIds;
		}

		public void setProductIds(final int[] productIds) {
			this.productIds = productIds;
		}
	}
}