package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Paths;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.NClob;
import java.sql.SQLException;
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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.bean.ProductSearchBean;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.event.subscriber.WrapContextEventSubscriber;
import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.mapping.annotations.Domain;
import jp.co.future.uroborosql.model.Product;
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
		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", productIdList)
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
		assertThat("結果が0件です。", rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat("結果が複数件です。", rs.next(), is(false));
	}

	/**
	 * クエリ実行処理のテストケース。（ログ抑止
	 */
	@Test
	void testQueryWithSuppressLog() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			agent.suppressLogging();
			try (var rs = agent.query("example/select_product")
					.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")))
					.resultSet()) {
				assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getString("PRODUCT_ID"), is("0"));
				assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
				assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
				assertThat("結果が複数件です。", rs.next(), is(false));
			} catch (SQLException ex) {
				fail(ex);
			}

			agent.releaseLogging();
			try (var rs = agent.query("example/select_product")
					.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")))
					.resultSet()) {
				assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getString("PRODUCT_ID"), is("0"));
				assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
				assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
				assertThat("結果が複数件です。", rs.next(), is(false));
			} catch (SQLException ex) {
				fail(ex);
			}
		});
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
		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", params)
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
		assertThat("結果が0件です。", rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat("結果が複数件です。", rs.next(), is(false));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryFilter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		config.getEventListenerHolder().addEventSubscriber(new WrapContextEventSubscriber("",
				"LIMIT /*$maxRowCount*/10 OFFSET /*$startRowIndex*/0", ".*(FOR\\sUPDATE|\\.NEXTVAL).*"));

		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("1")))
				.param("startRowIndex", 0)
				.param("maxRowCount", 1);

		var rs = agent.query(ctx);
		assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
		assertThat("結果が0件です。", rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat("結果が複数件です。", rs.next(), is(false));
	}

	/**
	 * SQLのdoTransformイベントリスナーのテストケース。
	 */
	@Test
	void testQueryFilterQueryWith() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		config.getEventListenerHolder()
				.addBeforeTransformSqlListener(evt -> evt.setSql(String.format("select * from (%s)", evt.getSql())));

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
	 * クエリ実行処理のテストケース（SQLがNULLの場合）。
	 */
	@Test
	void testQueryWithSqlNull() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.queryWith(null);
		});
	}

	/**
	 * クエリ実行処理のテストケース（SQLがEmptyの場合）。
	 */
	@Test
	void testQueryWithSqlEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.queryWith("");
		});
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryList() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id",
						List.of(0, 1, 2, 3));

		var rs = agent.query(ctx);
		assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
		assertThat("結果が0件です。", rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentArray() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var rs = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.resultSet();
		assertThat("ResultSetが取得できませんでした。", rs, is(not(nullValue())));
		assertThat("結果が0件です。", rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentCollect() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ans = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.collect();
		assertThat("結果の件数が一致しません。", ans.size(), is(2));
		var map = ans.get(0);
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));
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

		agent.query("example/select_product")
				.collect();

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
				.param("product_id", List.of(0, 1, 2, 3))
				.collect(CaseFormat.LOWER_SNAKE_CASE);
		assertThat("結果の件数が一致しません。", ans.size(), is(2));
		var map = ans.get(0);
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentCollectEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Product> ans = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.collect(Product.class);
		assertThat("結果の件数が一致しません。", ans.size(), is(2));
		var product = ans.get(0);
		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentCollectSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Integer> ans = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.collect(Integer.class);
		assertThat("結果の件数が一致しません。", ans.size(), is(2));
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
				.param("product_id", List.of(0, 1, 2, 3))
				.collect();
		assertThat("結果の件数が一致しません。", ans.size(), is(2));
		var map = ans.get(0);
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", List.of(10))
					.first();
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.first();
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentOne() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		try {
			agent.query("example/select_product")
					.param("product_id", List.of(10))
					.one();
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", List.of(0, 1, 2, 3))
					.one();
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.one();
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));
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
			agent.query("example/select_product")
					.param("product_id", List.of(10))
					.first();
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.first();
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));
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
			agent.query("example/select_product")
					.param("product_id", List.of(10))
					.one();
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", List.of(0, 1, 2, 3))
					.one();
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.one();
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var optional = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3)).findFirst();
		assertThat(optional.isPresent(), is(true));

		var map = optional.orElseThrow();
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4)).findFirst();
		assertThat(optional.isPresent(), is(false));
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
					.param("product_id", List.of(0, 1, 2, 3)).findOne();
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}

		var optional = agent.query("example/select_product")
				.param("product_id", List.of(0)).findOne();
		assertThat(optional.isPresent(), is(true));

		var map = optional.orElseThrow();
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4)).findOne();
		assertThat(optional.isPresent(), is(false));
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
				.param("product_id", List.of(0, 1, 2, 3)).findFirst();
		assertThat(optional.isPresent(), is(true));

		var map = optional.orElseThrow();
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4)).findFirst();
		assertThat(optional.isPresent(), is(false));
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
					.param("product_id", List.of(0, 1, 2, 3))
					.findOne();
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		var optional = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.findOne();
		assertThat(optional.isPresent(), is(true));

		var map = optional.orElseThrow();
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4)).findOne();
		assertThat(optional.isPresent(), is(false));
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
					.param("product_id", List.of(10))
					.first(CaseFormat.LOWER_SNAKE_CASE);
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.first(CaseFormat.LOWER_SNAKE_CASE);
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));
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
					.param("product_id", List.of(10))
					.one(CaseFormat.LOWER_SNAKE_CASE);
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", List.of(0, 1, 2, 3))
					.one(CaseFormat.LOWER_SNAKE_CASE);
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.one(CaseFormat.LOWER_SNAKE_CASE);
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindFirstCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var optional = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3)).findFirst(CaseFormat.PASCAL_CASE);
		assertThat(optional.isPresent(), is(true));

		var map = optional.orElseThrow();
		assertThat(map.get("ProductId"), is(new BigDecimal("0")));
		assertThat(map.get("ProductName"), is("商品名0"));
		assertThat(map.get("ProductKanaName"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JanCode"), is("1234567890123"));
		assertThat(map.get("ProductDescription"), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4)).findFirst();
		assertThat(optional.isPresent(), is(false));
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
					.param("product_id", List.of(0, 1, 2, 3))
					.findOne(CaseFormat.PASCAL_CASE);
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		var optional = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.findOne(CaseFormat.PASCAL_CASE);
		assertThat(optional.isPresent(), is(true));

		var map = optional.orElseThrow();
		assertThat(map.get("ProductId"), is(new BigDecimal("0")));
		assertThat(map.get("ProductName"), is("商品名0"));
		assertThat(map.get("ProductKanaName"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JanCode"), is("1234567890123"));
		assertThat(map.get("ProductDescription"), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4)).findOne();
		assertThat(optional.isPresent(), is(false));
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
					.param("product_id", List.of(10))
					.first(Product.class);
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		var product = agent.query("example/select_product")
				.param("product_id", List.of(0, 1, 2, 3))
				.first(Product.class);

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));
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
				.param("product_id", List.of(0, 1, 2, 3))
				.findFirst(Product.class);

		assertThat(optional.isPresent(), is(true));

		var product = optional.orElseThrow();

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4))
				.findFirst(Product.class);

		assertThat(optional.isPresent(), is(false));
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
					.param("product_id", List.of(10))
					.first(Product.class);
			fail();
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", List.of(0, 1, 2, 3))
					.one(Product.class);
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		var product = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.one(Product.class);

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));
	}

	/**
	 * クエリ実行処理(1件取得)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentOneByClassSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		var productId = agent.query("example/select_product")
				.param("product_id", List.of(0))
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
					.param("product_id", List.of(0, 1, 2, 3))
					.findOne(Product.class);
			fail();
		} catch (DataNonUniqueException e) {
			// OK
		}
		Optional<Product> optional = agent.query("example/select_product")
				.param("product_id", List.of(0))
				.findOne(Product.class);
		assertThat(optional.isPresent(), is(true));

		var product = optional.orElseThrow();

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", List.of(4))
				.findOne(Product.class);

		assertThat(optional.isPresent(), is(false));
	}

	/**
	 * クエリ実行処理(1件取得:Optional)のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentFindOneByClassSingleType() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		Optional<Integer> productId = agent.query("example/select_product")
				.param("product_id", List.of(0))
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

		var ctx = agent.context().setSqlName("example/select_product");
		ctx.param("product_id", List.of(0, 1));

		agent.query(ctx, rs -> {
			var rsmd = rs.getMetaData();

			var columnCount = rsmd.getColumnCount();

			Map<String, String> record = new LinkedHashMap<>(columnCount);

			for (var i = 1; i <= columnCount; i++) {
				record.put(rsmd.getColumnLabel(i), rs.getString(i));
			}
			return record;
		}).forEach(m -> {
			assertThat(m.containsKey("PRODUCT_ID"), is(true));
			assertThat(m.containsKey("PRODUCT_NAME"), is(true));
			assertThat(m.containsKey("PRODUCT_KANA_NAME"), is(true));
			assertThat(m.containsKey("JAN_CODE"), is(true));
			assertThat(m.containsKey("PRODUCT_DESCRIPTION"), is(true));
			assertThat(m.containsKey("INS_DATETIME"), is(true));
			assertThat(m.containsKey("UPD_DATETIME"), is(true));
			assertThat(m.containsKey("VERSION_NO"), is(true));
		});
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream().forEach(m -> {
					assertThat(m.containsKey("PRODUCT_ID"), is(true));
					assertThat(m.containsKey("PRODUCT_NAME"), is(true));
					assertThat(m.containsKey("PRODUCT_KANA_NAME"), is(true));
					assertThat(m.containsKey("JAN_CODE"), is(true));
					assertThat(m.containsKey("PRODUCT_DESCRIPTION"), is(true));
					assertThat(m.containsKey("INS_DATETIME"), is(true));
					assertThat(m.containsKey("UPD_DATETIME"), is(true));
					assertThat(m.containsKey("VERSION_NO"), is(true));
				});
		assertThat(agent.query("example/select_product")
				.param("product_id", List.of(0, 1)).stream().count(),
				is(2L));

	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	void testQueryFluentLambdaCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream(CaseFormat.LOWER_SNAKE_CASE).forEach(m -> {
					assertThat(m.containsKey("product_id"), is(true));
					assertThat(m.containsKey("product_name"), is(true));
					assertThat(m.containsKey("product_kana_name"), is(true));
					assertThat(m.containsKey("jan_code"), is(true));
					assertThat(m.containsKey("product_description"), is(true));
					assertThat(m.containsKey("ins_datetime"), is(true));
					assertThat(m.containsKey("upd_datetime"), is(true));
					assertThat(m.containsKey("version_no"), is(true));
				});
		assertThat(agent.query("example/select_product")
				.param("product_id", List.of(0, 1)).stream().count(),
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

		agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream().forEach(m -> {
					assertThat(m.containsKey("product_id"), is(true));
					assertThat(m.containsKey("product_name"), is(true));
					assertThat(m.containsKey("product_kana_name"), is(true));
					assertThat(m.containsKey("jan_code"), is(true));
					assertThat(m.containsKey("product_description"), is(true));
					assertThat(m.containsKey("ins_datetime"), is(true));
					assertThat(m.containsKey("upd_datetime"), is(true));
					assertThat(m.containsKey("version_no"), is(true));
				});
		assertThat(agent.query("example/select_product")
				.param("product_id", List.of(0, 1)).stream().count(),
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
		bean.setProductIds(List.of(0, 1));
		bean.setProductName("商品");

		agent.query("example/select_product_param_camel")
				.paramBean(bean)
				.stream().forEach(m -> {
					assertThat(m.containsKey("PRODUCT_ID"), is(true));
					assertThat(m.containsKey("PRODUCT_NAME"), is(true));
					assertThat(m.containsKey("PRODUCT_KANA_NAME"), is(true));
					assertThat(m.containsKey("JAN_CODE"), is(true));
					assertThat(m.containsKey("PRODUCT_DESCRIPTION"), is(true));
					assertThat(m.containsKey("INS_DATETIME"), is(true));
					assertThat(m.containsKey("UPD_DATETIME"), is(true));
					assertThat(m.containsKey("VERSION_NO"), is(true));
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
				.param("product_id", List.of(0, 1))
				.stream(Product.class)
				.forEach(p -> {
					assertThat(p.getProductId(), is(not(nullValue())));
					assertThat(p.getProductName(), is(not(nullValue())));
					assertThat(p.getProductKanaName(), is(not(nullValue())));
					assertThat(p.getJanCode(), is(not(nullValue())));
					assertThat(p.getProductDescription(), is(not(nullValue())));
					assertThat(p.getInsDatetime(), is(not(nullValue())));
					assertThat(p.getUpdDatetime(), is(not(nullValue())));
					assertThat(p.getVersionNo(), is(not(nullValue())));
				});
		assertThat(agent.query("example/select_product")
				.param("product_id", List.of(0, 1)).stream().count(),
				is(2L));

		try {
			agent.query("example/select_product").stream((Class<?>) null);
			fail();
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("Argument 'type' is required."));
		} catch (Exception ex) {
			fail();
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
			fail();
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("PRODUCT_NAME_NOTHING not found in query result."));
		} catch (Exception ex) {
			fail();
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
				is(new int[] { 1, 2 }));

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
			fail();
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("Argument 'type' is required."));
		} catch (Exception ex) {
			fail();
		}

		try {
			agent.queryWith("select 'abc'").select(Object.class);
			fail();
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("java.lang.Object is not supported."));
		} catch (Exception ex) {
			fail();
		}
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	void testQueryMapResultSetConverter() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.context().setSqlName("example/select_product");
		ctx.param("product_id", List.of(0, 1));

		var ans = agent.query(ctx, CaseFormat.CAMEL_CASE);
		assertThat(ans.size(), is(2));

		ans.forEach(m -> {
			assertThat(m.containsKey("productId"), is(true));
			assertThat(m.containsKey("productName"), is(true));
			assertThat(m.containsKey("productKanaName"), is(true));
			assertThat(m.containsKey("janCode"), is(true));
			assertThat(m.containsKey("productDescription"), is(true));
			assertThat(m.containsKey("insDatetime"), is(true));
			assertThat(m.containsKey("updDatetime"), is(true));
			assertThat(m.containsKey("versionNo"), is(true));
		});

		var ctx2 = agent.context().setSqlName("example/select_product");
		ctx2.param("product_id", List.of(0, 1));

		var ans2 = agent.query(ctx2, CaseFormat.UPPER_SNAKE_CASE);
		assertThat(ans2.size(), is(2));
		ans2.forEach(m -> {
			assertThat(m.containsKey("PRODUCT_ID"), is(true));
			assertThat(m.containsKey("PRODUCT_NAME"), is(true));
			assertThat(m.containsKey("PRODUCT_KANA_NAME"), is(true));
			assertThat(m.containsKey("JAN_CODE"), is(true));
			assertThat(m.containsKey("PRODUCT_DESCRIPTION"), is(true));
			assertThat(m.containsKey("INS_DATETIME"), is(true));
			assertThat(m.containsKey("UPD_DATETIME"), is(true));
			assertThat(m.containsKey("VERSION_NO"), is(true));
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

		agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.forEach(m -> {
					try {
						agent.update("example/insert_product_regist_work").paramMap(m).count();
					} catch (Exception e) {
						fail(e.getMessage());
					}
				});

		var collect = agent.queryWith("select * from product_regist_work")
				.collect();
		assertThat(collect.size(), is(2));
	}

	/**
	 * SqlAgentのsetter/getterのテスト.
	 */
	@Test
	void testSetterGetter() throws Exception {
		agent.setFetchSize(100);
		assertThat(agent.getFetchSize(), is(100));
		agent.setFetchSize(200);
		assertThat(agent.getFetchSize(), is(200));

		agent.setInsertsType(InsertsType.BULK);
		assertThat(agent.getInsertsType(), is(InsertsType.BULK));
		agent.setInsertsType(InsertsType.BATCH);
		assertThat(agent.getInsertsType(), is(InsertsType.BATCH));

		agent.setMapKeyCaseFormat(CaseFormat.PASCAL_CASE);
		assertThat(agent.getMapKeyCaseFormat(), is(CaseFormat.PASCAL_CASE));
		agent.setMapKeyCaseFormat(CaseFormat.CAMEL_CASE);
		assertThat(agent.getMapKeyCaseFormat(), is(CaseFormat.CAMEL_CASE));

		agent.setQueryTimeout(100);
		assertThat(agent.getQueryTimeout(), is(100));
		agent.setQueryTimeout(0);
		assertThat(agent.getQueryTimeout(), is(0));

		((SqlAgentImpl) agent).setMaxRetryCount(10);
		assertThat(((SqlAgentImpl) agent).getMaxRetryCount(), is(10));
		((SqlAgentImpl) agent).setMaxRetryCount(0);
		assertThat(((SqlAgentImpl) agent).getMaxRetryCount(), is(0));

		((SqlAgentImpl) agent).setRetryWaitTime(10);
		assertThat(((SqlAgentImpl) agent).getRetryWaitTime(), is(10));
		((SqlAgentImpl) agent).setRetryWaitTime(0);
		assertThat(((SqlAgentImpl) agent).getRetryWaitTime(), is(0));

		((SqlAgentImpl) agent).setSqlRetryCodes(List.of("60"));
		assertThat(((SqlAgentImpl) agent).getSqlRetryCodes(), containsInAnyOrder("60"));
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	void testNotFoundFile() throws Exception {
		Assertions.assertThrowsExactly(UroborosqlRuntimeException.class, () -> {
			var ctx = agent.context().setSqlName("file");
			agent.query(ctx);
		});
	}

	/**
	 * SQLファイルが空文字の場合のテストケース。
	 */
	@Test
	void testSqlNameEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.query("");
		});
	}

	/**
	 * クエリ実行処理のテストケース(SupplierによるSQLファイル決定)。
	 */
	@Test
	void testQuerySupplier() throws Exception {
		// 事前条件
		truncateTable("product_regist_work");
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var productId = List.of(0, 1);
		var updDatetime = LocalDateTime.of(2005, 12, 12, 10, 10, 10);
		var result = agent
				.query(() -> updDatetime != null ? "example/select_product_where_upd_datetime"
						: "example/select_product")
				.param("product_id", productId)
				.param("upd_datetime", updDatetime)
				.collect();
		assertThat(result.size(), is(2));
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