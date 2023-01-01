package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Paths;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.NClob;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.DayOfWeek;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.MonthDay;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.OptionalLong;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import jp.co.future.uroborosql.context.ExecutionContext;

import org.junit.jupiter.api.Test;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;
import jp.co.future.uroborosql.mapping.annotations.Domain;
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
	void testQuery() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<BigDecimal> productIdList = new ArrayList<>();
		productIdList.add(new BigDecimal("0"));
		productIdList.add(new BigDecimal("2"));
		var ctx = agent.contextFrom("example/select_product").param("product_id", productIdList)
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertNotNull(rs, "ResultSetが取得できませんでした。");
		assertTrue(rs.next(), "結果が0件です。");
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		assertFalse(rs.next(), "結果が複数件です。");
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryParamListWithSet() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Set<BigDecimal> params = new HashSet<>();
		params.add(new BigDecimal("0"));
		params.add(new BigDecimal("2"));
		var ctx = agent.contextFrom("example/select_product")
				.param("product_id", params)
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertNotNull(rs, "ResultSetが取得できませんでした。");
		assertTrue(rs.next(), "結果が0件です。");
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		assertFalse(rs.next(), "結果が複数件です。");
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryFilter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var manager = config.getSqlFilterManager();
		var filter = new WrapContextSqlFilter("",
				"LIMIT /*$maxRowCount*/10 OFFSET /*$startRowIndex*/0", ".*(FOR\\sUPDATE|\\.NEXTVAL).*");
		filter.initialize();
		manager.addSqlFilter(filter);

		var ctx = agent.contextFrom("example/select_product")
				.param("product_id", Arrays.asList(new BigDecimal("0"), new BigDecimal("1"))).param("startRowIndex", 0)
				.param("maxRowCount", 1);

		var rs = agent.query(ctx);
		assertNotNull(rs, "ResultSetが取得できませんでした。");
		assertTrue(rs.next(), "結果が0件です。");
		assertEquals("0", rs.getString("PRODUCT_ID"));
		assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
		assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
		assertEquals("1234567890123", rs.getString("JAN_CODE"));
		assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		assertFalse(rs.next(), "結果が複数件です。");
	}

	/**
	 * SQLのdoTransformフィルターのテストケース。
	 */
	@Test
	void testQueryFilterQueryWith() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var manager = config.getSqlFilterManager();
		manager.addSqlFilter(new AbstractSqlFilter() {
			@Override
			public String doTransformSql(final ExecutionContext ExecutionContext, final String sql) {
				return String.format("select * from (%s)", sql);
			}
		});

		var query = agent.queryWith("select product_id from product");
		var results = query.collect();
		assertThat(query.context().getSql(), is("select * from (select product_id from product)"));
		assertThat(results.size(), is(2));
	}

	/**
	 * LIKE句によるクエリ実行処理のテストケース。
	 */
	@Test
	void testQueryWithLikeClause() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var result = agent.queryWith(
				"select * from product where product_name like /*SF.contains(product_name)*/ escape /*#ESC_CHAR*/")
				.param("product_name", "商品")
				.collect();

		assertThat(result.size(), is(2));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryList() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product").param("product_id",
				Arrays.asList(0, 1, 2, 3));

		var rs = agent.query(ctx);
		assertNotNull(rs, "ResultSetが取得できませんでした。");
		assertTrue(rs.next(), "結果が0件です。");
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
	void testQueryFluentArray() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var rs = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3)).resultSet();
		assertNotNull(rs, "ResultSetが取得できませんでした。");
		assertTrue(rs.next(), "結果が0件です。");
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
	void testQueryFluentCollect() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect();
		assertEquals(2, ans.size(), "結果の件数が一致しません。");
		var map = ans.get(0);
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
	void testQueryFluentCollectWithPerformance() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");
		var rowsize = 500000;
		agent.required(() -> {
			agent.insertsAndReturn(IntStream.range(1, rowsize)
					.mapToObj(i -> new Product(i, "商品" + i, "ショウヒン" + i, "1111-" + i, "商品-" + i, new Date(), new Date(),
							1)));
		});

		var startTime = Instant.now();

		agent.query("example/select_product").collect();

		var finishTime = Instant.now();
		var elapsedTime = Duration.between(startTime, finishTime);
		System.out.println(rowsize + " records read time. time=" +
				elapsedTime.getSeconds() + "." + elapsedTime.getNano());
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentCollectCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(CaseFormat.LOWER_SNAKE_CASE);
		assertEquals(2, ans.size(), "結果の件数が一致しません。");
		var map = ans.get(0);
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
	void testQueryFluentCollectEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Product> ans = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(Product.class);
		assertEquals(2, ans.size(), "結果の件数が一致しません。");
		var product = ans.get(0);
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
	void testQueryFluentCollectSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Integer> ans = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(Integer.class);
		assertEquals(2, ans.size(), "結果の件数が一致しません。");
		assertThat(ans.get(0), is(0));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentCollectEntityWithPerformance() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");
		var rowsize = 500000;
		agent.required(() -> {
			agent.insertsAndReturn(IntStream.range(1, rowsize)
					.mapToObj(i -> new Product(i, "商品" + i, "ショウヒン" + i, "1111-" + i, "商品-" + i, new Date(), new Date(),
							1)));
		});

		var startTime = Instant.now();

		agent.query("example/select_product")
				.collect(Product.class);

		var finishTime = Instant.now();
		var elapsedTime = Duration.between(startTime, finishTime);
		System.out.println(rowsize + " records read time. time=" +
				elapsedTime.getSeconds() + "." + elapsedTime.getNano());
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentCollectSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		var ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect();
		assertEquals(2, ans.size(), "結果の件数が一致しません。");
		var map = ans.get(0);
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
	void testQueryFluentFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).first();
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
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
	void testQueryFluentOne() throws Exception {
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
		var map = agent.query("example/select_product")
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
	void testQueryFluentFirstSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).first();
			assertTrue(false);
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
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
	void testQueryFluentOneSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
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
		var map = agent.query("example/select_product")
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
	void testQueryFluentFindFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst();
		assertTrue(optional.isPresent());

		var map = optional.get();
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
	void testQueryFluentFindOne() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3)).findOne();
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0)).findOne();
		assertTrue(optional.isPresent());

		var map = optional.get();
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
	void testQueryFluentFindFirstSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst();
		assertTrue(optional.isPresent());

		var map = optional.get();
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
	void testQueryFluentFindOneSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.findOne();
			assertTrue(false);
		} catch (DataNonUniqueException e) {
			// OK
		}
		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne();
		assertTrue(optional.isPresent());

		var map = optional.get();
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
	void testQueryFluentFirstCaseFormat() throws Exception {
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
		var map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
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
	void testQueryFluentOneCaseFormat() throws Exception {
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
		var map = agent.query("example/select_product")
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
	void testQueryFluentFindFirstCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst(CaseFormat.PASCAL_CASE);
		assertTrue(optional.isPresent());

		var map = optional.get();
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
	void testQueryFluentFindOneCaseFormat() throws Exception {
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
		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne(CaseFormat.PASCAL_CASE);
		assertTrue(optional.isPresent());

		var map = optional.get();
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
	void testQueryFluentFirstByClass() throws Exception {
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
		var product = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.first(Product.class);

		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFirstSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ans = agent.query("example/select_product")
				.first(Integer.class);
		assertThat(ans, is(0));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindFirstByClass() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Optional<Product> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.findFirst(Product.class);

		assertTrue(optional.isPresent());

		var product = optional.get();

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
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindFirstSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Optional<Integer> ans = agent.query("example/select_product")
				.findFirst(Integer.class);
		assertThat(ans.orElse(null), is(0));
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentOneByClass() throws Exception {
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
		var product = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one(Product.class);

		assertEquals(0, product.getProductId());
		assertEquals("商品名0", product.getProductName());
		assertEquals("ショウヒンメイゼロ", product.getProductKanaName());
		assertEquals("1234567890123", product.getJanCode());
		assertEquals("0番目の商品", product.getProductDescription());
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentOneByClassSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var productId = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one(Integer.class);

		assertThat(productId, is(0));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindOneByClass() throws Exception {
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

		var product = optional.get();

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
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindOneByClassSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		Optional<Integer> productId = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne(Integer.class);

		assertThat(productId.orElse(null), is(0));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product");
		ctx.param("product_id", Arrays.asList(0, 1));

		agent.query(ctx, rs -> {
			var rsmd = rs.getMetaData();

			var columnCount = rsmd.getColumnCount();

			Map<String, String> record = new LinkedHashMap<>(columnCount);

			for (var i = 1; i <= columnCount; i++) {
				record.put(rsmd.getColumnLabel(i), rs.getString(i));
			}
			return record;
		}).forEach(m -> {
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
	void testQueryFluentLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream().forEach(m -> {
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
	void testQueryFluentLambdaCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream(CaseFormat.LOWER_SNAKE_CASE).forEach(m -> {
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
	void testQueryFluentLambdaSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream().forEach(m -> {
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
	void testQueryFluentLambdaWithParamBean() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var bean = new ProductSearchBean();
		bean.setProductIds(Arrays.asList(0, 1));
		bean.setProductName("商品");

		agent.query("example/select_product_param_camel")
				.paramBean(bean)
				.stream().forEach(m -> {
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
	void testQueryFluentStreamEntity() throws Exception {
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

		try {
			agent.query("example/select_product").stream((Class<?>) null);
			assertTrue(false);
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("Argument 'type' is required."));
		} catch (Exception ex) {
			assertTrue(false);
		}
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentStreamSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Integer> results = agent.query("example/select_product")
				.stream(Integer.class)
				.collect(Collectors.toList());
		assertThat(results.size(), is(2));
		assertThat(results.get(0), is(0));
		assertThat(results.get(1), is(1));

		List<String> stringResults = agent.query("example/select_product")
				.stream(String.class)
				.collect(Collectors.toList());
		assertThat(stringResults.size(), is(2));
	}

	/**
	 * クエリ実行（１カラム）処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentSelect() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 先頭カラムの取得
		List<Integer> idResults = agent.query("example/select_product")
				.select(Integer.class)
				.collect(Collectors.toList());
		assertThat(idResults.size(), is(2));
		assertThat(idResults.get(0), is(0));
		assertThat(idResults.get(1), is(1));

		// カラム指定の取得
		List<String> nameResults = agent.query("example/select_product")
				.select("productName", String.class)
				.collect(Collectors.toList());
		assertThat(nameResults.size(), is(2));
		assertThat(nameResults.get(0), is("商品名0"));
		assertThat(nameResults.get(1), is("商品名1"));

		try {
			// 存在しないカラムの指定
			agent.query("example/select_product")
					.select("productNameNothing", String.class)
					.collect(Collectors.toList());
			assertTrue(false);
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("PRODUCT_NAME_NOTHING not found in query result."));
		} catch (Exception ex) {
			assertTrue(false);
		}
	}

	/**
	 * クエリ実行（１カラム）処理のテストケース(Fluent API)。
	 */
	@SuppressWarnings("unchecked")
	@Test
	void testQueryFluentSelectByType() throws Exception {
		// 基本の型
		assertThat(agent.queryWith("select 'abc'").select(String.class).findFirst().orElse(null), is("abc"));
		assertThat(agent.queryWith("select true").select(boolean.class).findFirst().orElse(null), is(true));
		assertThat(agent.queryWith("select false").select(boolean.class).findFirst().orElse(null), is(false));
		assertThat(agent.queryWith("select true").select(Boolean.class).findFirst().orElse(null), is(true));
		assertThat(agent.queryWith("select false").select(Boolean.class).findFirst().orElse(null), is(false));
		assertThat(agent.queryWith("select X'61'").select(byte.class).findFirst().orElse(null), is("a".getBytes()[0]));
		assertThat(agent.queryWith("select X'61'").select(Byte.class).findFirst().orElse(null), is("a".getBytes()[0]));
		assertThat(agent.queryWith("select 1").select(short.class).findFirst().orElse(null), is((short) 1));
		assertThat(agent.queryWith("select 1").select(Short.class).findFirst().orElse(null), is((short) 1));
		assertThat(agent.queryWith("select 1").select(int.class).findFirst().orElse(null), is(1));
		assertThat(agent.queryWith("select 1").select(Integer.class).findFirst().orElse(null), is(1));
		assertThat(agent.queryWith("select 10000000000").select(long.class).findFirst().orElse(null), is(10000000000L));
		assertThat(agent.queryWith("select 10000000000").select(Long.class).findFirst().orElse(null), is(10000000000L));
		assertThat(agent.queryWith("select 1000.123").select(float.class).findFirst().orElse(null), is(1000.123f));
		assertThat(agent.queryWith("select 1000.123").select(Float.class).findFirst().orElse(null), is(1000.123f));
		assertThat(agent.queryWith("select 10000000000.123").select(double.class).findFirst().orElse(null),
				is(10000000000.123d));
		assertThat(agent.queryWith("select 10000000000.123").select(Double.class).findFirst().orElse(null),
				is(10000000000.123d));
		assertThat(agent.queryWith("select 1000000000000").select(BigInteger.class).findFirst().orElse(null),
				is(new BigInteger("1000000000000")));
		assertThat(agent.queryWith("select 10000000000.123").select(BigDecimal.class).findFirst().orElse(null),
				is(new BigDecimal("10000000000.123")));

		// 日付型
		assertThat(
				agent.queryWith("select CURRENT_DATE").select(java.sql.Date.class).findFirst().orElse(null).toString(),
				is(new java.sql.Date(Calendar.getInstance().getTimeInMillis()).toString()));
		assertThat(
				agent.queryWith("select CURRENT_DATE").select(Date.class).findFirst().orElse(null),
				instanceOf(Date.class));
		assertThat(
				agent.queryWith("select CURRENT_TIME").select(Time.class).findFirst().orElse(null),
				instanceOf(Time.class));
		assertThat(agent.queryWith("select CURRENT_TIMESTAMP").select(Timestamp.class).findFirst().orElse(null),
				instanceOf(Timestamp.class));

		// java.time API
		assertThat(agent.queryWith("select CURRENT_DATE").select(LocalDate.class).findFirst().orElse(null),
				is(LocalDate.now()));
		assertThat(agent.queryWith("select CURRENT_TIME").select(LocalTime.class).findFirst().orElse(null),
				instanceOf(LocalTime.class));
		assertThat(agent.queryWith("select CURRENT_TIME").select(OffsetTime.class).findFirst().orElse(null),
				instanceOf(OffsetTime.class));
		assertThat(agent.queryWith("select CURRENT_TIMESTAMP").select(LocalDateTime.class).findFirst().orElse(null),
				instanceOf(LocalDateTime.class));
		assertThat(agent.queryWith("select CURRENT_TIMESTAMP").select(OffsetDateTime.class).findFirst().orElse(null),
				instanceOf(OffsetDateTime.class));
		assertThat(agent.queryWith("select CURRENT_TIMESTAMP").select(ZonedDateTime.class).findFirst().orElse(null),
				instanceOf(ZonedDateTime.class));
		assertThat(agent.queryWith("select YEAR(CURRENT_DATE)").select(Year.class).findFirst().orElse(null),
				is(Year.now()));
		assertThat(agent.queryWith("select 202208").select(YearMonth.class).findFirst().orElse(null),
				is(YearMonth.of(2022, 8)));
		assertThat(agent.queryWith("select 823").select(MonthDay.class).findFirst().orElse(null),
				is(MonthDay.of(8, 23)));
		assertThat(agent.queryWith("select MONTH(CURRENT_DATE)").select(Month.class).findFirst().orElse(null),
				is(Month.from(LocalDate.now())));
		assertThat(
				agent.queryWith("select ISO_DAY_OF_WEEK(CURRENT_DATE)").select(DayOfWeek.class).findFirst()
						.orElse(null),
				is(DayOfWeek.from(LocalDate.now())));

		// 配列型
		assertThat(agent.queryWith("select ARRAY[1, 2]").select(Object[].class).findFirst().orElse(null)[1], is(2));
		assertThat(agent.queryWith("select X'616263'").select(byte[].class).findFirst().orElse(null),
				is("abc".getBytes()));
		assertThat(agent.queryWith("select ARRAY[1, 2]").select(Array.class).findFirst().orElse(null).getArray(),
				is(new int[]{1, 2}));

		// java.sqlの型
		assertThat(agent.queryWith("select CAST('abc' as CLOB)").select(Clob.class).findFirst().orElse(null)
				.getSubString(1, 3), is("abc"));
		assertThat(agent.queryWith("select CAST('abc' as NCLOB)").select(NClob.class).findFirst().orElse(null)
				.getSubString(1, 3), is("abc"));
		assertThat(agent.queryWith("select CAST(X'616263' as BLOB)").select(Blob.class).findFirst().orElse(null)
						.getBytes(1, 3),
				is("abc".getBytes()));
		// TODO java.sql.REF, java.sql.SQLXML は H2DBが対応していないためテストできていない

		// Optional型
		assertThat(agent.queryWith("select 'abc'").select(Optional.class).findFirst().orElse(null).orElse(null),
				is("abc"));
		assertThat(agent.queryWith("select 1").select(OptionalInt.class).findFirst().orElse(null).orElse(0), is(1));
		assertThat(agent.queryWith("select 10000000000").select(OptionalLong.class).findFirst().orElse(null).orElse(0L),
				is(10000000000L));
		assertThat(
				agent.queryWith("select 10000000000.123").select(OptionalDouble.class).findFirst().orElse(null)
						.orElse(0d),
				is(10000000000.123d));

		// Domain型
		assertThat(agent.queryWith("select 'abc'").select(NameDomain.class).findFirst().orElse(null).getName(),
				is("abc"));
		// Enum型
		assertThat(agent.queryWith("select 'NAME1'").select(NameEnum.class).findFirst().orElse(null),
				is(NameEnum.NAME1));

		// 例外
		try {
			agent.queryWith("select 'abc'").select(null);
			assertTrue(false);
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("Argument 'type' is required."));
		} catch (Exception ex) {
			assertTrue(false);
		}

		try {
			agent.queryWith("select 'abc'").select(Object.class);
			assertTrue(false);
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("java.lang.Object is not supported."));
		} catch (Exception ex) {
			assertTrue(false);
		}
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryMapResultSetConverter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product");
		ctx.param("product_id", Arrays.asList(0, 1));

		var ans = agent.query(ctx, CaseFormat.CAMEL_CASE);
		assertThat(ans.size(), is(2));

		ans.forEach(m -> {
			assertTrue(m.containsKey("productId"));
			assertTrue(m.containsKey("productName"));
			assertTrue(m.containsKey("productKanaName"));
			assertTrue(m.containsKey("janCode"));
			assertTrue(m.containsKey("productDescription"));
			assertTrue(m.containsKey("insDatetime"));
			assertTrue(m.containsKey("updDatetime"));
			assertTrue(m.containsKey("versionNo"));
		});

		var ctx2 = agent.contextFrom("example/select_product");
		ctx2.param("product_id", Arrays.asList(0, 1));

		var ans2 = agent.query(ctx2, CaseFormat.UPPER_SNAKE_CASE);
		assertThat(ans2.size(), is(2));
		ans2.forEach(m -> {
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
	void testQueryFluentLambdaAndUpdate() throws Exception {
		// 事前条件
		truncateTable("product_regist_work");
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.forEach(m -> {
					try {
						agent.update("example/insert_product_regist_work").paramMap(m).count();
					} catch (Exception e) {
						fail(e.getMessage());
					}
				});

		var collect = agent.queryWith("select * from product_regist_work").collect();
		assertThat(collect.size(), is(2));
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	void testNotFoundFile() throws Exception {
		try {
			var ctx = agent.contextFrom("file");
			agent.query(ctx);
			// 例外が発生しなかった場合
			fail();
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	//Enumを定義
	public enum NameEnum {
		NAME1, NAME2, NAME3;
	}

	// Doaminを定義
	@Domain(valueType = String.class, toJdbcMethod = "getName")
	public static class NameDomain {
		private final String name;

		public NameDomain(final String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}

		@Override
		public int hashCode() {
			return Objects.hash(name);
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			var other = (NameDomain) obj;
			if (!Objects.equals(name, other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "NameDomain [name=" + name + "]";
		}

	}

}