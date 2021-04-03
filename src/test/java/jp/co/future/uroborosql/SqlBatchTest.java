package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.CaseFormat;

public class SqlBatchTest extends AbstractDbTest {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(SqlBatchTest.class);

	/**
	 * バッチ処理のテストケース。
	 */
	@Test
	public void testExecuteBatch() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

		var ctx = agent.contextFrom("example/insert_product").param("product_id", new BigDecimal(1))
				.param("product_name", "商品名1").param("product_kana_name", "ショウヒンメイイチ")
				.param("jan_code", "1234567890123").param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch().param("product_id", new BigDecimal(2))
				.param("product_name", "商品名2").param("product_kana_name", "ショウヒンメイニ")
				.param("jan_code", "1234567890124").param("product_description", "2番目の商品")
				.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		var count = agent.batch(ctx);
		assertThat(count.length, is(2));
		assertThat(count[0], is(1));
		assertThat(count[1], is(1));

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatch.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", Arrays.asList(1, 2))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertThat(actualDataList.toString(), is(expectedDataList.toString()));
	}

	/**
	 * バッチ処理のテストケース。(複数回)
	 */
	@Test
	public void testExecuteBatchRepeat() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

		var ctx = agent.contextFrom("example/insert_product").param("product_id", new BigDecimal(1))
				.param("product_name", "商品名1").param("product_kana_name", "ショウヒンメイイチ")
				.param("jan_code", "1234567890123").param("product_description", "1番目の商品")
				.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
				.param("version_no", new BigDecimal(0)).addBatch();

		var count = agent.batch(ctx);
		assertThat(count.length, is(1));
		assertThat(count[0], is(1));

		ctx.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch();

		count = agent.batch(ctx);
		assertThat(count.length, is(1));
		assertThat(count[0], is(1));

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatch.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", Arrays.asList(1, 2))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertThat(actualDataList.toString(), is(expectedDataList.toString()));
	}

	/**
	 * バッチ処理(Null挿入）のテストケース。
	 */
	@Test
	public void testExecuteBatchNull() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		var ctx = agent.contextFrom("example/insert_product").param("product_id", new BigDecimal(1))
				.param("product_name", null).param("product_kana_name", null).param("jan_code", "1234567890123")
				.param("product_description", "1番目の商品").param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch()
				.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
				.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
				.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
				.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch();

		var count = agent.batch(ctx);
		assertThat(count.length, is(2));
		assertThat(count[0], is(1));
		assertThat(count[1], is(1));

		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchNull.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.param("product_id", Arrays.asList(1, 2))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertThat(actualDataList.toString(), is(expectedDataList.toString()));
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testExecuteBatchNoAddBatch() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var count = agent.update("example/insert_product").batch();
		assertThat(count.length, is(0));
	}

	/**
	 * バッチ処理のテストケース(Stream)。
	 */
	@Test
	public void testExecuteBatchStream() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var count = agent.batch("example/insert_product").paramStream(input.stream()).count();

		assertThat(count, is(100));

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchStream.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertThat(actualDataList.toString(), is(expectedDataList.toString()));
	}

	/**
	 * バッチ処理のテストケース(Bean Stream)。
	 */
	@Test
	public void testExecuteBatchBeanStream() {
		// 事前条件
		truncateTable("PRODUCT");

		// Entity生成のために一旦登録してSelect
		var data = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		agent.batch("example/insert_product").paramStream(data.stream()).count();
		// 取得
		var input = agent.query(Product.class).collect();
		// 再度削除
		truncateTable("PRODUCT");

		// 処理実行
		var count = agent.batch("example/insert_product_for_bean").paramStream(input.stream()).count();

		assertThat(count, is(100));

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchStream.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertThat(actualDataList.toString(), is(expectedDataList.toString()));
	}

	/**
	 * バッチ処理のテストケース(Stream)。
	 */
	@Test
	public void testExecuteBatchStreamWithBatchWhen() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var count = agent.batch("example/insert_product").paramStream(input.stream())
				.by((ctx, row) -> {
					Object current = row.get("ins_datetime");
					Object pre = ctx.contextAttrs().put("prevRowValue", current);
					return pre != null && !current.equals(pre);
				}).count();

		assertThat(count, is(100));

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
				"src/test/resources/data/expected/SqlAgent", "testExecuteBatchStream.ltsv"));
		List<Map<String, Object>> actualDataList = agent.query("example/select_product")
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertThat(actualDataList.toString(), is(expectedDataList.toString()));
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	public void testExecuteBatchStreamWithQuery() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		// 処理実行
		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var productCount = agent.batch("example/insert_product").paramStream(input.stream()).count();

		assertThat(productCount, is(100));

		agent.commit();

		var workCount = agent.batch("example/insert_product_regist_work")
				.paramStream(agent.query("example/select_product")
						.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE)))
				.by((ctx, row) -> ctx.batchCount() == 7)
				.count();

		assertThat(workCount, is(100));
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	public void testExecuteBatchStreamMultiStream() throws Exception {
		// 事前条件
		truncateTable("PRODUCT");

		var input = getDataFromFile(Paths.get("src/test/resources/data/expected/SqlAgent",
				"testExecuteBatchStream.ltsv"));
		var productCount = agent.batch("example/insert_product").paramStream(input.stream()).count();

		assertThat(productCount, is(100));

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

		assertThat(workCount, is(150));
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	public void testExecuteBatchStreamCommitTiming() throws Exception {
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
					log.error("error occurred. ex:{}", ex.getMessage());
				})
				.count();

		assertThat(count, is(60));
	}

	/**
	 * バッチ処理のテストケース(Query結果のStream）。
	 */
	@Test
	public void testExecuteManyInsert() throws Exception {
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
			assertThat(count, is(1));
		}
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	public void testNotFoundFile() throws Exception {
		try {
			var ctx = agent.contextFrom("file");
			agent.batch(ctx);
			// 例外が発生しなかった場合
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception e) {
			assertThat(e.getMessage(), false);
		}
	}

}