package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.JDBCType;
import java.sql.Types;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Test;

import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.utils.CaseFormat;

public class SqlUpdateTest extends AbstractDbTest {
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
				.param("product_id", Arrays.asList(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

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
				.param("product_id", Arrays.asList(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB更新処理のテストケース(NULLに設定)。
	 */
	@Test
	public void testExecuteUpdateToNull() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		int updateCount = agent
				.updateWith("update product set jan_code = /*jan_code*/ where product_id = /*product_id*/")
				.param("product_id", 1, JDBCType.INTEGER)
				.param("jan_code", null, Types.CHAR)
				.count();
		assertEquals("データの登録に失敗しました。", 1, updateCount);

		assertNull(agent.queryWith("select jan_code from product where product_id = /*product_id*/")
				.param("product_id", 1, JDBCType.INTEGER)
				.one()
				.get("JAN_CODE"));
	}

	/**
	 * DB複数更新処理(SQLファイル1件)のテストケース。(Fluent API)
	 */
	@Test
	public void testUpdatesSingleFileFluent() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		int updateCount = agent.updateChained("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"))
				.param("jan_code", "1234567890123").count();
		assertEquals("データの登録に失敗しました。", 1, updateCount);

		// 検証処理
		List<Map<String, Object>> expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Integer> productIds = new ArrayList<>();
		productIds.add(0);
		productIds.add(1);
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", productIds)
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB複数更新処理(SQLファイルn件)のテストケース.
	 */
	@Test
	public void testUpdateChained() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		int updateCount = agent.updateChained("example/selectinsert_product",
				"example/insert_product_regist_work",
				"example/update_product")
				.param("product_id", new BigDecimal("2"))
				.param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890123")
				.param("product_description", "2番目の商品")
				.param("ins_datetime", ZonedDateTime.now())
				.count();
		assertEquals("データの登録に失敗しました。", 1, updateCount);

		// 検証処理
		if (!agent.find(Product.class, 2).isPresent()) {
			fail();
		}
		List<Map<String, Object>> productRegistWorks = agent.queryWith("select * from product_regist_work")
				.collect();

		assertEquals(productRegistWorks.size(), 2);

		List<Product> products = agent.query(Product.class)
				.asc("product_id")
				.collect();
		assertEquals(products.get(0).getProductName(), "商品名1_updated");
		assertEquals(products.get(1).getProductName(), "商品名0_updated");
	}

	/**
	 * DB複数更新処理(SQLファイルn件)のテストケース。(Fluent API)<br/>
	 * 最初の更新SQLが複数件を更新する場合
	 */
	@Test
	public void testUpdateChainedFirstSQLMultiRowUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("2"))
				.param("jan_code", "1234567890123")
				.count();

		int updateCount = agent.updateChained(
				"example/update_product",
				"example/selectinsert_product")
				.param("product_id", new BigDecimal("3"))
				.param("jan_code", "1234567890123")
				.count();
		assertEquals("データの登録に失敗しました。", 2, updateCount);

		// 検証処理
		List<Product> products = agent.query(Product.class)
				.asc("product_id")
				.collect();
		assertEquals(products.size(), 3);
		assertEquals(products.get(0).getProductName(), "商品名1_updated");
		assertEquals(products.get(1).getProductName(), "商品名0_updated");
		assertEquals(products.get(2).getProductName(), "商品名0");
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	public void testNotFoundFile() throws Exception {
		try {
			SqlContext ctx = agent.contextFrom("file");
			agent.update(ctx);
			// 例外が発生しなかった場合
			fail();
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}

	/**
	 * updateDelegateが指定された場合のテストケース。
	 */
	@Test
	public void testUpdateDelegate() throws Exception {
		SqlUpdate update = agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR);
		SqlContext ctx = update.context();
		ctx.setUpdateDelegate(context -> 2);

		assertThat(update.count(), is(2));
	}

	/**
	 * 複数SQL更新実行処理のテストケース（引数がEmptyの場合）。
	 */
	@Test
	public void testUpdatesSqlNameEmpty() throws Exception {
		try {
			agent.updateChained((String[]) null);
			fail();
		} catch (IllegalArgumentException ex) {
			// OK
		} catch (Exception ex) {
			fail();
		}
	}

	/**
	 * 複数SQL更新実行処理のテストケース（引数で指定したSQLファイルのいずれかが存在しない場合）。
	 */
	@Test
	public void testUpdatesNotFoundFile() throws Exception {
		try {
			agent.updateChained("example/selectinsert_product", "not_exists_file");
			fail();
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception ex) {
			fail();
		}
	}
}