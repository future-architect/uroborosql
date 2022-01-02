package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.IntStream;

import org.junit.Test;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.utils.CaseFormat;

/**
 * SqlQueryインタフェースのテストケース
 *
 * @author H.Sugimoto
 */
public class SqlQueryTest extends AbstractDbTest {
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
	@SuppressWarnings("deprecation")
	@Test
	public void testQueryParamList() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product")
				.paramList("product_id", new BigDecimal("0"), new BigDecimal("2"))
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
	public void testQueryParamListWithSet() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Set<BigDecimal> params = new HashSet<>();
		params.add(new BigDecimal("0"));
		params.add(new BigDecimal("2"));
		SqlContext ctx = agent.contextFrom("example/select_product")
				.param("product_id", params)
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
				.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("1"))).param("startRowIndex", 0)
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
	 * SQLのdoTransformフィルターのテストケース。
	 */
	@Test
	public void testQueryFilterQueryWith() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlFilterManager manager = config.getSqlFilterManager();
		manager.addSqlFilter(new AbstractSqlFilter() {
			@Override
			public String doTransformSql(final SqlContext sqlContext, final String sql) {
				String newSql = String.format("select * from (%s)", sql);
				return newSql;
			}
		});

		SqlQuery query = agent.queryWith("select product_id from product");
		List<Map<String, Object>> results = query.collect();
		assertThat(query.context().getSql(), is("select * from (select product_id from product)"));
		assertThat(results.size(), is(2));
	}

	/**
	 * LIKE句によるクエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryWithLikeClause() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Map<String, Object>> result = agent.queryWith(
				"select * from product where product_name like /*SF.contains(product_name)*/ escape /*#ESC_CHAR*/")
				.param("product_name", "商品")
				.collect();

		assertThat(result.size(), is(2));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryList() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3));

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

		ResultSet rs = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3)).resultSet();
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

		List<Map<String, Object>> ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
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
	public void testQueryFluentCollectWithPerformance() throws Exception {
		// 事前条件
		// 事前条件
		truncateTable("PRODUCT");
		int rowsize = 1000000;
		agent.required(() -> {
			agent.insertsAndReturn(IntStream.range(1, rowsize)
					.mapToObj(i -> new Product(i, "商品" + i, "ショウヒン" + i, "1111-" + i, "商品-" + i, new Date(), new Date(),
							1)));
		});

		Instant startTime = Instant.now();

		agent.query("example/select_product").collect();

		Instant finishTime = Instant.now();
		Duration elapsedTime = Duration.between(startTime, finishTime);
		System.out.println(rowsize + " records read time. time=" +
				elapsedTime.getSeconds() + "." + elapsedTime.getNano());
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentCollectCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Map<String, Object>> ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(CaseFormat.LOWER_SNAKE_CASE);
		assertEquals("結果の件数が一致しません。", 2, ans.size());
		Map<String, Object> map = ans.get(0);
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentCollectEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Product> ans = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(Product.class);
		assertEquals("結果の件数が一致しません。", 2, ans.size());
		Product product = ans.get(0);
		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentCollectEntityWithPerformance() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");
		int rowsize = 1000000;
		agent.required(() -> {
			agent.insertsAndReturn(IntStream.range(1, rowsize)
					.mapToObj(i -> new Product(i, "商品" + i, "ショウヒン" + i, "1111-" + i, "商品-" + i, new Date(), new Date(),
							1)));
		});

		Instant startTime = Instant.now();

		agent.query("example/select_product")
				.collect(Product.class);

		Instant finishTime = Instant.now();
		Duration elapsedTime = Duration.between(startTime, finishTime);
		System.out.println(rowsize + " records read time. time=" +
				elapsedTime.getSeconds() + "." + elapsedTime.getNano());
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
		agent = config.agent();

		List<Map<String, Object>> ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
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

		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).first();
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		Map<String, Object> map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.first();
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
	public void testQueryFluentOne() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).one();
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3)).one();
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Map<String, Object> map = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one();
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
		agent = config.agent();

		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).first();
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		Map<String, Object> map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.first();
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
	public void testQueryFluentOneSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentFactory().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();
		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).one();
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.one();
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Map<String, Object> map = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one();
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
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst();
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("PRODUCT_ID"));
		assertEquals("商品名0", map.get("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", map.get("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", map.get("JAN_CODE"));
		assertEquals("0番目の商品", map.get("PRODUCT_DESCRIPTION"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findFirst();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindOne() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3)).findOne();
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}

		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0)).findOne();
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("PRODUCT_ID"));
		assertEquals("商品名0", map.get("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", map.get("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", map.get("JAN_CODE"));
		assertEquals("0番目の商品", map.get("PRODUCT_DESCRIPTION"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findOne();
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
		agent = config.agent();

		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst();
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findFirst();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindOneSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentFactory().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.findOne();
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne();
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("product_id"));
		assertEquals("商品名0", map.get("product_name"));
		assertEquals("ショウヒンメイゼロ", map.get("product_kana_name"));
		assertEquals("1234567890123", map.get("jan_code"));
		assertEquals("0番目の商品", map.get("product_description"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findOne();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFirstCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(10))
					.first(CaseFormat.LOWER_SNAKE_CASE);
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		Map<String, Object> map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.first(CaseFormat.LOWER_SNAKE_CASE);
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
	public void testQueryFluentOneCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(10))
					.one(CaseFormat.LOWER_SNAKE_CASE);
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.one(CaseFormat.LOWER_SNAKE_CASE);
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Map<String, Object> map = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one(CaseFormat.LOWER_SNAKE_CASE);
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
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst(CaseFormat.PASCAL_CASE);
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("ProductId"));
		assertEquals("商品名0", map.get("ProductName"));
		assertEquals("ショウヒンメイゼロ", map.get("ProductKanaName"));
		assertEquals("1234567890123", map.get("JanCode"));
		assertEquals("0番目の商品", map.get("ProductDescription"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findFirst();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFindOneCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.findOne(CaseFormat.PASCAL_CASE);
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Optional<Map<String, Object>> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne(CaseFormat.PASCAL_CASE);
		assertTrue(optional.isPresent());

		Map<String, Object> map = optional.get();
		assertEquals(new BigDecimal("0"), map.get("ProductId"));
		assertEquals("商品名0", map.get("ProductName"));
		assertEquals("ショウヒンメイゼロ", map.get("ProductKanaName"));
		assertEquals("1234567890123", map.get("JanCode"));
		assertEquals("0番目の商品", map.get("ProductDescription"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findOne();
		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentFirstByClass() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(10))
					.first(Product.class);
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		Product product = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
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
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.findFirst(Product.class);

		assertTrue(optional.isPresent());

		Product product = optional.get();

		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());

		optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(4))
				.findFirst(Product.class);

		assertFalse(optional.isPresent());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentOneByClass() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(10))
					.first(Product.class);
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.one(Product.class);
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Product product = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one(Product.class);

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
	public void testQueryFluentFindOneByClass() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.findOne(Product.class);
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		Optional<Product> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne(Product.class);
		assertTrue(optional.isPresent());

		Product product = optional.get();

		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());

		optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(4))
				.findOne(Product.class);

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
		ctx.param("product_id", Arrays.asList(0, 1));

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

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
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
		assertThat(agent.query("example/select_product").param("product_id", Arrays.asList(0, 1)).stream().count(),
				is(2L));

	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambdaCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream(CaseFormat.LOWER_SNAKE_CASE).forEach((m) -> {
					assertTrue(m.containsKey("product_id"));
					assertTrue(m.containsKey("product_name"));
					assertTrue(m.containsKey("product_kana_name"));
					assertTrue(m.containsKey("jan_code"));
					assertTrue(m.containsKey("product_description"));
					assertTrue(m.containsKey("ins_datetime"));
					assertTrue(m.containsKey("upd_datetime"));
					assertTrue(m.containsKey("version_no"));
				});
		assertThat(agent.query("example/select_product").param("product_id", Arrays.asList(0, 1)).stream().count(),
				is(2L));
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
		agent = config.agent();

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
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
		assertThat(agent.query("example/select_product").param("product_id", Arrays.asList(0, 1)).stream().count(),
				is(2L));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLambdaWithParamBean() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		ProductSearchBean bean = new ProductSearchBean();
		bean.setProductIds(Arrays.asList(0, 1));
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

		assertThat(agent.query("example/select_product_param_camel").paramBean(bean).stream().count(), is(2L));

	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentStreamEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1))
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
		assertThat(agent.query("example/select_product").param("product_id", Arrays.asList(0, 1)).stream().count(),
				is(2L));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryMapResultSetConverter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		SqlContext ctx = agent.contextFrom("example/select_product");
		ctx.param("product_id", Arrays.asList(0, 1));

		List<Map<String, Object>> ans = agent.query(ctx, CaseFormat.CAMEL_CASE);
		assertThat(ans.size(), is(2));

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
		ctx2.param("product_id", Arrays.asList(0, 1));

		List<Map<String, Object>> ans2 = agent.query(ctx2, CaseFormat.UPPER_SNAKE_CASE);
		assertThat(ans2.size(), is(2));
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

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.forEach((m) -> {
					try {
						agent.update("example/insert_product_regist_work").paramMap(m).count();
					} catch (Exception e) {
						fail(e.getMessage());
					}
				});

		List<Map<String, Object>> collect = agent.queryWith("select * from product_regist_work").collect();
		assertThat(collect.size(), is(2));
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

}