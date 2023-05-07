package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigDecimal;
import java.nio.file.Paths;
import java.sql.JDBCType;
import java.sql.Types;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.converter.MapResultSetConverter;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.utils.CaseFormat;

public class SqlUpdateTest extends AbstractDbTest {
	/**
	 * DB更新処理のテストケース。
	 */
	@Test
	void testExecuteUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteUpdate.ltsv"));

		var ctx = agent.contextFrom("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR);

		var updateCount = agent.update(ctx);
		assertEquals(1, updateCount, "データの登録に失敗しました。");

		// 検証処理
		var expectedDataList = getDataFromFile(Paths.get(
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
				.param("product_id", Arrays.asList(0, 1))
				.stream(new MapResultSetConverter(agent.getSqlConfig(), CaseFormat.LOWER_SNAKE_CASE))
				.collect(Collectors.toList());

		assertEquals(expectedDataList.toString(), actualDataList.toString());
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
		try {
			var ctx = agent.contextFrom("file");
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
	void testUpdateDelegate() throws Exception {
		var update = agent.update("example/selectinsert_product")
				.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
				.param("jan_code", "1234567890123", Types.CHAR);
		var ctx = update.context();
		ctx.setUpdateDelegate(context -> 2);

		assertThat(update.count(), is(2));
	}

}