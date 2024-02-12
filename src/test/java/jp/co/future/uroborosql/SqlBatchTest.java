package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.CaseFormat;

public class SqlBatchTest extends AbstractDbTest {
	/**
	 * バッチ処理のテストケース。
	 */
	@Test
	void testExecuteBatch() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

		var ctx = agent.context().setSqlName("example/insert_product")
				.param("product_id", new BigDecimal(1))
				.param("product_name", "商品名1")
				.param("product_kana_name", "ショウヒンメイイチ")
				.param("jan_code", "1234567890123")
				.param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch()
				.param("product_id", new BigDecimal(2))
				.param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品")
				.param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		var count = agent.batch(ctx);
		assertEquals(2, count.length, "データの登録件数が不正です。");
		assertEquals(1, count[0], "1行目のデータの登録に失敗しました。");
		assertEquals(1, count[1], "2行目のデータの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatch.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(1, 2))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース。(複数回)
	 */
	@Test
	void testExecuteBatchRepeat() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

		var ctx = agent.context().setSqlName("example/insert_product")
				.param("product_id", new BigDecimal(1))
				.param("product_name", "商品名1")
				.param("product_kana_name", "ショウヒンメイイチ")
				.param("jan_code", "1234567890123")
				.param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		var count = agent.batch(ctx);
		assertEquals(1, count.length, "データの登録件数が不正です。");
		assertEquals(1, count[0], "1行目のデータの登録に失敗しました。");

		ctx.param("product_id", new BigDecimal(2))
				.param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品")
				.param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		count = agent.batch(ctx);
		assertEquals(1, count.length, "データの登録件数が不正です。");
		assertEquals(1, count[0], "1行目のデータの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatch.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(1, 2))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理(Null挿入）のテストケース。
	 */
	@Test
	void testExecuteBatchNull() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		var ctx = agent.context().setSqlName("example/insert_product")
				.param("product_id", new BigDecimal(1))
				.param("product_name", null)
				.param("product_kana_name", null)
				.param("jan_code", "1234567890123")
				.param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch()
				.param("product_id", new BigDecimal(2))
				.param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品")
				.param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		var count = agent.batch(ctx);
		assertEquals(2, count.length, "データの登録件数が不正です。");
		assertEquals(1, count[0], "1行目のデータの登録に失敗しました。");
		assertEquals(1, count[1], "2行目のデータの登録に失敗しました。");

		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchNull.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", List.of(1, 2))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース(Stream)。
	 */
	@Test
	void testExecuteBatchStream() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var count = agent.batch("example/insert_product").paramStream(input.stream()).count();

		assertEquals(100, count, "データの登録件数が不正です。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchStream.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース(Bean Stream)。
	 */
	@Test
	void testExecuteBatchBeanStream() {
		// 事前条件
		truncateTable("PRODUCT");

		// Entity生成のために一旦登録してSelect
		var data = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		agent.batch("example/insert_product").paramStream(data.stream()).count();
		// 取得
		var input = agent.query(Product.class)
				.collect();
		// 再度削除
		truncateTable("PRODUCT");

		// 処理実行
		var count = agent.batch("example/insert_product_for_bean").paramStream(input.stream()).count();

		assertEquals(100, count, "データの登録件数が不正です。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchStream.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース(Stream)。
	 */
	@Test
	void testExecuteBatchStreamWithBatchWhen() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var count = agent.batch("example/insert_product").paramStream(input.stream())
				.by((ctx, row) -> {
					var current = row.get("ins_datetime");
					var pre = ctx.contextAttrs().put("prevRowValue", current);
					return pre != null && !current.equals(pre);
				}).count();

		assertEquals(100, count, "データの登録件数が不正です。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchStream.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	void testExecuteBatchStreamWithQuery() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var productCount = agent.batch("example/insert_product").paramStream(input.stream()).count();

		assertEquals(100, productCount, "データの登録件数が不正です。");

		agent.commit();

		var workCount = agent.batch("example/insert_product_regist_work")
				.paramStream(agent.query("example/select_product")
						.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE)))
				.by((ctx, row) -> ctx.batchCount() == 7)
				.count();

		assertEquals(100, workCount, "データの登録件数が不正です。");
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	void testExecuteBatchStreamMultiStream() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var productCount = agent.batch("example/insert_product").paramStream(input.stream()).count();

		assertEquals(100, productCount, "データの登録件数が不正です。");

		agent.commit();

		List<Map<String, Object>> paramList = new ArrayList<>();
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		for (var i = 101; i <= 150; i++) {
			Map<String, Object> row = new HashMap<>();
			row.put("product_name", "商品名" + i);
			row.put("product_kana_name", "ショウヒンメイ" + i);
			row.put("jan_code", "1234567890124");
			row.put("product_description", i + "番目の商品");
			row.put("ins_datetime", currentDatetime);
			paramList.add(row);
		}

		// 処理実行
		var workCount = agent.batch("example/insert_product_regist_work")
				.paramStream(agent.query("example/select_product")
						.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE)))
				.paramStream(paramList.stream())
				.by((ctx, row) -> ctx.batchCount() == 10)
				.count();

		assertEquals(150, workCount, "データの登録件数が不正です。");
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	void testExecuteBatchStreamCommitTiming() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		List<Map<String, Object>> paramList = new ArrayList<>();
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		for (var i = 1; i <= 100; i++) {
			Map<String, Object> row = new HashMap<>();
			row.put("product_name", "商品名" + i);
			row.put("product_kana_name", "ショウヒンメイ" + i);
			row.put("jan_code", i % 25 != 0 ? "1234567890124" : "12345678901234"); // 20で割り切れるとき、桁あふれエラーを発生させる
			row.put("product_description", i + "番目の商品");
			row.put("ins_datetime", currentDatetime);
			paramList.add(row);
		}

		// 処理実行
		var count = agent.batch("example/insert_product_regist_work")
				.paramStream(paramList.stream())
				.by((ctx, row) -> ctx.batchCount() == 10)
				.batchWhen((agent, ctx) -> agent.commit())
				.errorWhen((agent, ctx, ex) -> {
					// do noting
				})
				.count();

		assertEquals(60, count, "データの登録件数が不正です。");
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	void testExecuteManyInsert() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		for (var i = 1; i <= 1000; i++) {
			Map<String, Object> row = new HashMap<>();
			row.put("product_id", i);
			row.put("product_name", "商品名" + i);
			row.put("product_kana_name", "ショウヒンメイ" + i);
			row.put("jan_code", "1234567890124");
			row.put("product_description", i + "番目の商品");
			row.put("ins_datetime", currentDatetime);
			row.put("upd_datetime", currentDatetime);
			row.put("version_no", 1);
			// 処理実行
			var count = agent.update("example/insert_product")
					.paramMap(row).count();
			assertEquals(1, count, "データの登録件数が不正です。");
		}
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	void testNotFoundFile() throws Exception {
		Assertions.assertThrowsExactly(UroborosqlRuntimeException.class, () -> {
			var ctx = agent.context().setSqlName("file");
			agent.batch(ctx);
		});
	}

	/**
	 * SQLファイルが空文字の場合のテストケース。
	 */
	@Test
	void testSqlNameEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.batch("");
		});
	}

	/**
	 * バッチ実行処理のテストケース（SQLがNULLの場合）。
	 */
	@Test
	void testBatchWithSqlNull() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.batchWith(null);
		});
	}

	/**
	 * バッチ実行処理のテストケース（SQLがEmptyの場合）。
	 */
	@Test
	void testBatchWithSqlEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			agent.batchWith("");
		});
	}

	/**
	 * updateDelegateが指定された場合のテストケース。
	 */
	@Test
	void testUpdateDelegate() throws Exception {
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		List<Map<String, Object>> rows = new ArrayList<>();
		for (var i = 1; i <= 1000; i++) {
			Map<String, Object> row = new HashMap<>();
			row.put("product_id", i);
			row.put("product_name", "商品名" + i);
			row.put("product_kana_name", "ショウヒンメイ" + i);
			row.put("jan_code", "1234567890124");
			row.put("product_description", i + "番目の商品");
			row.put("ins_datetime", currentDatetime);
			row.put("upd_datetime", currentDatetime);
			row.put("version_no", 1);
			rows.add(row);
		}
		// 処理実行
		var batch = agent.batch("example/insert_product")
				.paramStream(rows.stream());
		var ctx = batch.context();
		ctx.setUpdateDelegate(context -> 2);

		assertThat(batch.count(), is(4));
	}

}