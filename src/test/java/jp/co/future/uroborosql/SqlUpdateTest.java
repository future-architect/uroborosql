package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.JDBCType;
import java.sql.Types;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.model.Product;
import jp.co.future.uroborosql.utils.CaseFormat;

public class SqlUpdateTest extends AbstractDbTest {
	/**
	 * DB更新処理のテストケース。
	 */
	@Test
	void testExecuteUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var ctx = agent.context().setSqlName("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR);

		var updateCount = agent.update(ctx);
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB更新処理のテストケース。(Fluent API)
	 */
	@Test
	void testUpdateFluent() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var updateCount = agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR).count();
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB更新処理のテストケース。(SupplierでSQLNameを決定)
	 */
	@Test
	void testUpdateSupplier() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		String productName = null;
		var updateCount = agent
				.update(() -> productName == null ? "example/selectinsert_product" : "example/selectinsert_product2")
				.param("product_id", new BigDecimal("0"))
				.param("product_name", productName)
				.param("jan_code", "1234567890123")
				.count();
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB複数更新処理(SQLファイル1件)のテストケース。(Fluent API)
	 */
	@Test
	void testUpdatesSingleFileFluent() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var updateCount = agent.updateChained("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"))
				.param("jan_code", "1234567890123").count();
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteUpdate.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * DB複数更新処理(SQLファイルn件)のテストケース.
	 */
	@Test
	void testUpdateChained() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var updateCount = agent.updateChained("example/selectinsert_product",
				"example/insert_product_regist_work",
				"example/update_product")
				.param("product_id", new BigDecimal("2"))
				.param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890123")
				.param("product_description", "2番目の商品")
				.param("ins_datetime", ZonedDateTime.now())
				.count();
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		// 検証処理
		if (agent.find(Product.class, 2).isEmpty()) {
			Assert.fail();
		}
		var productRegistWorks = agent.queryWith("select * from product_regist_work")
				.collect();

		assertEquals(productRegistWorks.size(), 2);

		var products = agent.query(Product.class)
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
	void testUpdateChainedFirstSQLMultiRowUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));
		agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("2"))
				.param("jan_code", "1234567890123")
				.count();

		var updateCount = agent.updateChained(
				"example/update_product",
				"example/selectinsert_product")
				.param("product_id", new BigDecimal("3"))
				.param("jan_code", "1234567890123")
				.count();
		assertEquals(2, updateCount, "データの登録に失敗しました。");

		// 検証処理
		var products = agent.query(Product.class)
				.asc("product_id")
				.collect();
		assertEquals(products.size(), 3);
		assertEquals(products.get(0).getProductName(), "商品名1_updated");
		assertEquals(products.get(1).getProductName(), "商品名0_updated");
		assertEquals(products.get(2).getProductName(), "商品名0");
	}

	/**
	 * DB更新処理のテストケース(NULLに設定)。
	 */
	@Test
	void testExecuteUpdateToNull() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var updateCount = agent
				.updateWith("update product set jan_code = /*jan_code*/ where product_id = /*product_id*/")
				.param("product_id", 1, JDBCType.INTEGER)
				.param("jan_code", null, Types.CHAR)
				.count();
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		assertNull(agent.queryWith("select jan_code from product where product_id = /*product_id*/")
				.param("product_id", 1, JDBCType.INTEGER)
				.one()
				.get("JAN_CODE"));
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	void testNotFoundFile() throws Exception {
		Assertions.assertThrowsExactly(UroborosqlRuntimeException.class, () -> {
			var ctx = agent.context().setSqlName("file");
			agent.update(ctx);
		});
	}

	/**
	 * SQLファイルが空文字の場合のテストケース。
	 */
	@Test
	void testSqlNameEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.update("");
		});
	}

	/**
	 * 更新実行処理のテストケース（SQLがNULLの場合）。
	 */
	@Test
	void testUpdateWithSqlNull() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.updateWith(null);
		});
	}

	/**
	 * 更新実行処理のテストケース（SQLがEmptyの場合）。
	 */
	@Test
	void testUpdateWithSqlEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.updateWith("");
		});
	}

	/**
	 * 複数SQL更新実行処理のテストケース（引数がEmptyの場合）。
	 */
	@Test
	void testUpdatesSqlNameEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.updateChained((String[]) null);
		});
	}

	/**
	 * 複数SQL更新実行処理のテストケース（引数で指定したSQLファイルのいずれかが存在しない場合）。
	 */
	@Test
	void testUpdatesNotFoundFile() throws Exception {
		Assertions.assertThrowsExactly(UroborosqlRuntimeException.class, () -> {
			agent.updateChained("example/selectinsert_product", "not_exists_file");
		});
	}

	/**
	 * updateDelegateが指定された場合のテストケース。
	 */
	@Test
	void testUpdateDelegate() throws Exception {
		var update = agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR);
		var ctx = update.context();
		ctx.setUpdateDelegate(context -> 2);

		assertThat(update.count(), is(2));
	}

}