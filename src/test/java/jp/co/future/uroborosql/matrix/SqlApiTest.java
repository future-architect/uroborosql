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
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.model.Product;

class SqlApiTest extends AbstractMatrixTest {

	/**
	 * PRODUCTテーブルのクエリ実行処理のテストケース。
	 */
	@Test
	void testProductQuery() throws Exception {
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
		assertThat(columnTypeTest.get("COL_DATE"), is(instanceOf(ZonedDateTime.class)));
		assertThat(columnTypeTest.get("COL_TIME"), is(instanceOf(LocalTime.class)));

	}
}
