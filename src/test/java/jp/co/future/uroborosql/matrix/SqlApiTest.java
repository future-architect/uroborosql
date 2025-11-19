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
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.model.Product;

class SqlApiTest extends AbstractMatrixTest {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(SqlApiTest.class);

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
	 * PRODUCTテーブルのクエリ実行処理のテストケース（CacheSchema=true）
	 */
	@Test
	void testProductQueryWithCacheSchema() throws Exception {
		config.getConnectionSupplier().setDefaultCacheSchema(true);

		truncateTable("PRODUCT");
		var products = agent.query(Product.class)
				.collect();

		assertThat(products, is(empty()));

		var newProduct = new Product();
		newProduct.setProductName("商品１");
		newProduct.setProductKanaName("ショウヒン１");
		newProduct.setJanCode("1234567890123");
		newProduct.setProductDescription("商品１説明");
		newProduct.setInsDatetime(null);
		newProduct.setUpdDatetime(null);
		newProduct.setVersionNo(1);

		var updateCount = agent.insert(newProduct);
		assertThat(updateCount, is(1));

		var newProducts = agent.query(Product.class)
				.collect();

		assertThat(newProducts, is(not(empty())));
		assertThat(newProducts.size(), is(1));
		var product = newProducts.get(0);
		assertThat(product.getProductId(), is(not(nullValue())));
		assertThat(product.getProductName(), is("商品１"));
		assertThat(product.getProductKanaName(), is("ショウヒン１"));
		assertThat(product.getJanCode(), is("1234567890123"));
		assertThat(product.getProductDescription(), is("商品１説明"));
		assertThat(product.getInsDatetime(), is(nullValue()));
		assertThat(product.getUpdDatetime(), is(nullValue()));
		assertThat(product.getVersionNo(), is(1));
	}

	/**
	 * PRODUCTテーブルのクエリ実行処理のテストケース（CacheSchema=true）
	 */
	@Test
	void testProductQueryWithFixSchema() throws Exception {
		config.getConnectionSupplier().setDefaultFixSchema(true);

		truncateTable("PRODUCT");
		var products = agent.query(Product.class)
				.collect();

		assertThat(products, is(empty()));

		var newProduct = new Product();
		newProduct.setProductName("商品１");
		newProduct.setProductKanaName("ショウヒン１");
		newProduct.setJanCode("1234567890123");
		newProduct.setProductDescription("商品１説明");
		newProduct.setInsDatetime(null);
		newProduct.setUpdDatetime(null);
		newProduct.setVersionNo(1);

		var updateCount = agent.insert(newProduct);
		assertThat(updateCount, is(1));

		var newProducts = agent.query(Product.class)
				.collect();

		assertThat(newProducts, is(not(empty())));
		assertThat(newProducts.size(), is(1));
		var product = newProducts.get(0);
		assertThat(product.getProductId(), is(not(nullValue())));
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

		if (Objects.equals(getTestDbType(), "oracle")) {
			agent.updateWith("ALTER TABLE PRODUCT MODIFY (PRODUCT_ID GENERATED BY DEFAULT AS IDENTITY (START WITH 1))")
					.count();
		} else if (Objects.equals(getTestDbType(), "postgresql")) {
			agent.updateWith("TRUNCATE TABLE PRODUCT RESTART IDENTITY")
					.count();
		}

		agent.inserts(IntStream.rangeClosed(1, 10)
				.mapToObj(idx -> {
					var product = new Product();
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

	/**
	 * バッチインサートのテストケース（IDENTITY列対応確認）
	 * SQL Serverなど、executeBatch()でgetGeneratedKeys()をサポートしないDBでも
	 * バッチインサート自体は成功することを確認
	 */
	@Test
	void testBatchInsertWithAutoGeneratedKeys() throws Exception {
		truncateTable("PRODUCT");

		// テスト用のProductエンティティを作成
		// バッチインサート実行
		agent.inserts(IntStream.rangeClosed(1, 3)
				.mapToObj(i -> {
					var product = new Product();
					product.setProductName("商品" + i);
					product.setProductKanaName("ショウヒン" + i);
					product.setJanCode(String.format("123456789%04d", i));
					product.setProductDescription("商品" + i + "説明");
					product.setVersionNo(1);
					return product;
				}));

		// データが正しく挿入されたことを確認
		var insertedProducts = agent.query(Product.class)
				.asc("productId")
				.collect();

		assertThat(insertedProducts.size(), is(3));

		// SQL Serverの場合、自動生成されたIDは設定されない（これは期待される動作）
		// 他のDBでは自動生成されたIDが設定される
		if (!config.getDialect().supportsBatchGeneratedKeys()) {
			// SQL Serverなどバッチでの自動生成キー取得をサポートしないDBでは
			// バッチインサート自体は成功するが、エンティティにIDは設定されない
			log.atInfo()
					.setMessage("Batch insert succeeded on {} without generated key population")
					.addArgument(config.getDialect().getDatabaseName())
					.log();
		}
	}

	/**
	 * insertsAndReturn()のテストケース（IDENTITY列対応確認）
	 * SQL Serverなど、executeBatch()でgetGeneratedKeys()をサポートしないDBでも
	 * insertsAndReturn()は成功し、エンティティのStreamが返されることを確認
	 */
	@Test
	void testBatchInsertAndReturnWithAutoGeneratedKeys() throws Exception {
		truncateTable("PRODUCT");

		// テスト用のProductエンティティを作成
		// バッチインサート実行してエンティティを返却
		var returnedProducts = agent.insertsAndReturn(IntStream.rangeClosed(1, 3)
				.mapToObj(i -> {
					var product = new Product();
					product.setProductName("商品" + i);
					product.setProductKanaName("ショウヒン" + i);
					product.setJanCode(String.format("123456789%04d", i));
					product.setProductDescription("商品" + i + "説明");
					product.setVersionNo(1);
					return product;
				}))
				.collect(Collectors.toList());
		assertThat(returnedProducts.size(), is(3));

		// データが正しく挿入されたことを確認
		var insertedProducts = agent.query(Product.class)
				.asc("productId")
				.collect();

		assertThat(insertedProducts.size(), is(3));

		// バッチでの自動生成キー取得をサポートしている場合、returnedProductsとinsertedProductsのIDが一致することを確認
		if (config.getDialect().supportsBatchGeneratedKeys()) {
			for (var i = 0; i < 3; i++) {
				var returnedProduct = returnedProducts.get(i);
				var insertedProduct = insertedProducts.get(i);
				assertThat(returnedProduct.getProductId(), is(insertedProduct.getProductId()));
			}
		} else {
			// SQL Serverの場合、返されたエンティティに自動生成されたIDは設定されない（これは期待される動作）
			for (var product : returnedProducts) {
				assertThat(product.getProductId(), is(nullValue()));
			}
			log.atInfo()
					.setMessage("Batch insert and return succeeded on {} without generated key population")
					.addArgument(config.getDialect().getDatabaseName())
					.log();
		}
	}

}
