package jp.co.future.uroborosql.matrix;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.model.Product;

class SqlApiTest extends AbstractMatrixTest {

	/**
	 * PRODUCTテーブルのクエリ実行処理のテストケース。
	 */
	@Test
	void testProductQuery() throws Exception {
		truncateTable("PRODUCT");
		var products = agent.query("example/select_product")
				.param("product_id", List.of(1))
				.collect(Product.class);

		assertThat(products, is(empty()));

		var updateCount = agent.update("example/insert_product")
				.param("product_id", 1)
				.param("product_name", "商品１")
				.param("product_kana_name", "ショウヒン１")
				.param("jan_code", "1234567890123")
				.param("product_description", "商品１説明")
				.param("ins_datetime", Optional.empty())
				.param("upd_datetime", Optional.empty())
				.param("version_no", 1)
				.count();
		assertThat(updateCount, is(1));

		var newProducts = agent.query("example/select_product")
				.param("product_id", List.of(1))
				.collect(Product.class);

		assertThat(newProducts, is(not(empty())));
		assertThat(newProducts.size(), is(1));
		var product = newProducts.get(0);
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品１"));
		assertThat(product.getProductKanaName(), is("ショウヒン１"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("商品１説明"));
		assertThat(product.getInsDatetime(), is(nullValue()));
		assertThat(product.getUpdDatetime(), is(nullValue()));
		assertThat(product.getVersionNo(), is(1));
	}

	/**
	 * COLUMN_TYPE_TESTテーブルのクエリ実行処理のテストケース。
	 */
	@Test
	void testColumnType() throws Exception {
		truncateTable("COLUMN_TYPE_TEST");
		agent.update("example/insert_column_type_test")
				.param("col_varchar", "abc")
				.param("col_char", "X")
				.param("col_numeric", 12.34)
				.param("col_boolean", Optional.empty())
				.param("col_timestamp", LocalDateTime.now())
				.param("col_date", LocalDate.now())
				.param("col_time", LocalTime.now())
				.count();

		var columnTypeTest = agent.query("example/select_column_type_test")
				.findFirst()
				.orElseThrow();

		assertThat(columnTypeTest.get("COL_VARCHAR"), is("abc"));
		assertThat(Objects.toString(columnTypeTest.get("COL_CHAR"), "").trim(), is("X"));
		assertThat(columnTypeTest.get("COL_NUMERIC"), is(new BigDecimal("12.34")));
		assertThat(columnTypeTest.get("COL_TIMESTAMP"), is(instanceOf(Timestamp.class)));
		assertThat(columnTypeTest.get("COL_DATE"), is(not(nullValue())));
		assertThat(columnTypeTest.get("COL_TIME"), is(not(nullValue())));

	}

	/**
	 * PRODUCTテーブルの複数SQL一括更新のテストケース。
	 */
	@Test
	void testProductUpdateChained() throws Exception {
		if (!config.getDialect().supportsUpdateChained()) {
			// updateChainedがサポートされていない場合は何もしない
			return;
		}

		// 事前準備
		truncateTable("PRODUCT");
		agent.inserts(IntStream.rangeClosed(1, 10)
				.mapToObj(idx -> {
					var product = new Product();
					product.setProductId(idx);
					product.setProductName("商品" + idx);
					product.setProductKanaName("ショウヒン" + idx);
					product.setJanCode(String.format("12345678901%02d", idx));
					product.setProductDescription("商品" + idx + "説明");
					product.setInsDatetime(new Date());
					product.setUpdDatetime(new Date());
					product.setVersionNo(0);
					return product;
				}));

		// 検証対象メソッド
		var updateCount = agent.updateChained("example/update_product",
				"example/insert_product")
				.param("product_ids", List.of(1, 2, 3))
				.param("product_id", 11)
				.param("product_name", "商品11")
				.param("product_kana_name", "ショウヒン11")
				.param("jan_code", "1234567890111")
				.param("product_description", "商品11説明")
				.param("ins_datetime", new Date())
				.param("upd_datetime", new Date())
				.param("version_no", 1)
				.count();

		if (Objects.equals(getTestDbType(), "sqlserver")) {
			assertThat(updateCount, is(1)); // 最後の更新SQLの更新件数
		} else {
			assertThat(updateCount, is(3)); // 最初の更新SQLの更新件数
		}

		var newProducts = agent.query(Product.class)
				.asc("product_id")
				.collect();

		assertThat(newProducts, is(not(empty())));
		assertThat(newProducts.size(), is(11));
		var product = newProducts.get(0);
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品1_updated"));
		product = newProducts.get(10);
		assertThat(product.getProductId(), is(11));
		assertThat(product.getProductName(), is("商品11"));
	}

	/**
	 * PRODUCTテーブルの複数SQL一括更新がサポートされていないDBの場合のテストケース。
	 */
	@Test
	void testProductUpdateChainedNotSupported() throws Exception {
		if (config.getDialect().supportsUpdateChained()) {
			// updateChainedがサポートされている場合は何もしない
			return;
		}

		Assertions.assertThrowsExactly(UroborosqlRuntimeException.class, () -> {
			agent.updateChained("example/update_product",
					"example/insert_product")
					.param("product_ids", List.of(1, 2, 3))
					.param("product_id", 11)
					.param("product_name", "商品11")
					.param("product_kana_name", "ショウヒン11")
					.param("jan_code", "1234567890111")
					.param("product_description", "商品11説明")
					.param("ins_datetime", new Date())
					.param("upd_datetime", new Date())
					.param("version_no", 1)
					.count();
		});
	}

}
