package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;

public class UpdateDelegateTest {
	protected SqlConfig config;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.build();
		config.getEventListenerHolder().addAfterSetDaoUpdateParameterListener(
				evt -> evt.getExecutionContext().setUpdateDelegate(context -> 2));
	}

	/**
	 * updateDelegateが指定されたupdateメソッドのテストケース。
	 */
	@Test
	void testUpdateDelegate() throws Exception {
		try (var agent = config.agent()) {
			assertThat(agent.update("example/selectinsert_product")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR).count(), is(2));
		}
	}

	/**
	 * updateDelegateが指定されたbatchメソッドのテストケース。
	 */
	@Test
	void testBatchUpdateDelegate() throws Exception {
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

		try (var agent = config.agent()) {
			// 処理実行
			assertThat(agent.batch("example/insert_product")
					.paramStream(rows.stream())
					.count(), is(4));
		}
	}

}