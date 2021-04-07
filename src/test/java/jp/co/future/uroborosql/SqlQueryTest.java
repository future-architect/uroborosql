package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.DataNotFoundException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;
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
		var ctx = agent.contextFrom("example/select_product").param("product_id", productIdList)
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertThat(rs, not(nullValue()));
		assertThat(rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat(rs.next(), is(false));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryParamList() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product")
				.param("product_id", List.of(new BigDecimal("0"), new BigDecimal("2")))
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertThat(rs, not(nullValue()));
		assertThat(rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat(rs.next(), is(false));
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
		var ctx = agent.contextFrom("example/select_product")
				.param("product_id", params)
				.setSqlId("test_sql_id");

		var rs = agent.query(ctx);
		assertThat(rs, not(nullValue()));
		assertThat(rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat(rs.next(), is(false));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryFilter() throws Exception {
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
		assertThat(rs, not(nullValue()));
		assertThat(rs.next(), is(true));
		assertThat(rs.getString("PRODUCT_ID"), is("0"));
		assertThat(rs.getString("PRODUCT_NAME"), is("商品名0"));
		assertThat(rs.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(rs.getString("JAN_CODE"), is("1234567890123"));
		assertThat(rs.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
		assertThat(rs.next(), is(false));
	}

	/**
	 * SQLのdoTransformフィルターのテストケース。
	 */
	@Test
	public void testQueryFilterQueryWith() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var manager = config.getSqlFilterManager();
		manager.addSqlFilter(new AbstractSqlFilter() {
			@Override
			public String doTransformSql(final ExecutionContext executionContext, final String sql) {
				var newSql = String.format("select * from (%s)", sql);
				return newSql;
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
	public void testQueryWithLikeClause() throws Exception {
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
	public void testQueryList() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.contextFrom("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3));

		var rs = agent.query(ctx);
		assertThat(rs, not(nullValue()));
		assertThat(rs.next(), is(true));
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
	public void testQueryFluentArray() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var rs = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3)).resultSet();
		assertThat(rs, not(nullValue()));
		assertThat(rs.next(), is(true));
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
	public void testQueryFluentCollect() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect();
		assertThat(ans.size(), is(2));
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
	public void testQueryFluentCollectCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ans = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(CaseFormat.LOWER_SNAKE_CASE);
		assertThat(ans.size(), is(2));
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
	public void testQueryFluentCollectEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Product> ans = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
				.collect(Product.class);
		assertThat(ans.size(), is(2));
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
	public void testQueryFluentCollectSetDefaultCaseFormat() throws Exception {
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
		assertThat(ans.size(), is(2));
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
	public void testQueryFluentFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).first();
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
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
	public void testQueryFluentOne() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));
		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).one();
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3)).one();
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
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
	public void testQueryFluentFirstSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).first();
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
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
	public void testQueryFluentOneSetDefaultCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// agentの設定を変更するため、setupで生成したagentをいったんクローズする
		agent.close();

		// defaultMapKeyCaseFormatの設定
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();
		try {
			agent.query("example/select_product").param("product_id", Arrays.asList(10)).one();
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.one();
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
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
	public void testQueryFluentFindFirst() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst();
		assertThat(optional.isPresent(), is(true));

		var map = optional.get();
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findFirst();
		assertThat(optional.isPresent(), is(false));
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
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0)).findOne();
		assertThat(optional.isPresent(), is(true));

		var map = optional.get();
		assertThat(map.get("PRODUCT_ID"), is(new BigDecimal("0")));
		assertThat(map.get("PRODUCT_NAME"), is("商品名0"));
		assertThat(map.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JAN_CODE"), is("1234567890123"));
		assertThat(map.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findOne();
		assertThat(optional.isPresent(), is(false));
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
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst();
		assertThat(optional.isPresent(), is(true));

		var map = optional.get();
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findFirst();
		assertThat(optional.isPresent(), is(false));
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
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.findOne();
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne();
		assertThat(optional.isPresent(), is(true));

		var map = optional.get();
		assertThat(map.get("product_id"), is(new BigDecimal("0")));
		assertThat(map.get("product_name"), is("商品名0"));
		assertThat(map.get("product_kana_name"), is("ショウヒンメイゼロ"));
		assertThat(map.get("jan_code"), is("1234567890123"));
		assertThat(map.get("product_description"), is("0番目の商品"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findOne();
		assertThat(optional.isPresent(), is(false));
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
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		var map = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1, 2, 3))
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
	public void testQueryFluentOneCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(10))
					.one(CaseFormat.LOWER_SNAKE_CASE);
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.one(CaseFormat.LOWER_SNAKE_CASE);
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		var map = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
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
	public void testQueryFluentFindFirstCaseFormat() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3)).findFirst(CaseFormat.PASCAL_CASE);
		assertThat(optional.isPresent(), is(true));

		var map = optional.get();
		assertThat(map.get("ProductId"), is(new BigDecimal("0")));
		assertThat(map.get("ProductName"), is("商品名0"));
		assertThat(map.get("ProductKanaName"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JanCode"), is("1234567890123"));
		assertThat(map.get("ProductDescription"), is("0番目の商品"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findFirst();
		assertThat(optional.isPresent(), is(false));
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
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		var optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne(CaseFormat.PASCAL_CASE);
		assertThat(optional.isPresent(), is(true));

		var map = optional.get();
		assertThat(map.get("ProductId"), is(new BigDecimal("0")));
		assertThat(map.get("ProductName"), is("商品名0"));
		assertThat(map.get("ProductKanaName"), is("ショウヒンメイゼロ"));
		assertThat(map.get("JanCode"), is("1234567890123"));
		assertThat(map.get("ProductDescription"), is("0番目の商品"));

		optional = agent.query("example/select_product").param("product_id", Arrays.asList(4)).findOne();
		assertThat(optional.isPresent(), is(false));
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
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		var product = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1, 2, 3))
				.first(Product.class);

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));
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

		assertThat(optional.isPresent(), is(true));

		var product = optional.get();

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(4))
				.findFirst(Product.class);

		assertThat(optional.isPresent(), is(false));
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
			assertThat(false, is(true));
		} catch (DataNotFoundException e) {
			// OK
		}
		try {
			agent.query("example/select_product")
					.param("product_id", Arrays.asList(0, 1, 2, 3))
					.one(Product.class);
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		var product = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.one(Product.class);

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));
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
			assertThat(false, is(true));
		} catch (DataNonUniqueException e) {
			// OK
		}
		Optional<Product> optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(0))
				.findOne(Product.class);
		assertThat(optional.isPresent(), is(true));

		var product = optional.get();

		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名0"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイゼロ"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("0番目の商品"));

		optional = agent.query("example/select_product")
				.param("product_id", Arrays.asList(4))
				.findOne(Product.class);

		assertThat(optional.isPresent(), is(false));
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryLambda() throws Exception {
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
	public void testQueryFluentLambda() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
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
		config.getSqlAgentProvider().setDefaultMapKeyCaseFormat(CaseFormat.LOWER_SNAKE_CASE);
		agent = config.agent();

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
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

		var bean = new ProductSearchBean();
		bean.setProductIds(Arrays.asList(0, 1));
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
	public void testQueryFluentStreamEntity() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product")
				.param("product_id", Arrays.asList(0, 1))
				.stream(Product.class)
				.forEach(p -> {
					assertThat(p.getProductId(), not(nullValue()));
					assertThat(p.getProductName(), not(nullValue()));
					assertThat(p.getProductKanaName(), not(nullValue()));
					assertThat(p.getJanCode(), not(nullValue()));
					assertThat(p.getProductDescription(), not(nullValue()));
					assertThat(p.getInsDatetime(), not(nullValue()));
					assertThat(p.getUpdDatetime(), not(nullValue()));
					assertThat(p.getVersionNo(), not(nullValue()));
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

		var ctx = agent.contextFrom("example/select_product");
		ctx.param("product_id", Arrays.asList(0, 1));

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

		var ctx2 = agent.contextFrom("example/select_product");
		ctx2.param("product_id", Arrays.asList(0, 1));

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
	public void testQueryFluentLambdaAndUpdate() throws Exception {
		// 事前条件
		truncateTable("product_regist_work");
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.forEach(m -> {
					try {
						agent.update("example/insert_product_regist_work").paramMap(m).count();
					} catch (Exception e) {
						assertThat(e.getMessage(), false);
					}
				});

		var collect = agent.queryWith("select * from product_regist_work").collect();
		assertThat(collect.size(), is(2));
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	public void testNotFoundFile() throws Exception {
		try {
			var ctx = agent.contextFrom("file");
			agent.query(ctx);
			// 例外が発生しなかった場合
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception e) {
			assertThat(e.getMessage(), false);
		}
	}

}