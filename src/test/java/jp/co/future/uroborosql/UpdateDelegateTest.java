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

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;

public class UpdateDelegateTest {
	protected SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName()))
				.setSqlContextFactory(new SqlContextFactoryImpl()
						.addUpdateAutoParameterBinder(ctx -> ctx.setUpdateDelegate(context -> 2)))
				.build();
	}

	/**
	 * updateDelegateが指定されたupdateメソッドのテストケース。
	 */
	@Test
	public void testUpdateDelegate() throws Exception {
		try (SqlAgent agent = config.agent()) {
			assertThat(agent.update("example/selectinsert_product")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR).count(), is(2));
		}
	}

	/**
	 * updateDelegateが指定されたbatchメソッドのテストケース。
	 */
	@Test
	public void testBatchUpdateDelegate() throws Exception {
		Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		List<Map<String, Object>> rows = new ArrayList<>();
		for (int i = 1; i <= 1000; i++) {
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

		try (SqlAgent agent = config.agent()) {
			// 処理実行
			assertThat(agent.batch("example/insert_product")
					.paramStream(rows.stream())
					.count(), is(4));
		}
	}

}