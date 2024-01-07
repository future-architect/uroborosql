package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;

public class SqlEntityQueryTest extends AbstractDbTest {

	@Test
	void testCollect() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var products = agent.query(Product.class)
				.in("product_id", 0, 1)
				.collect();

		products.forEach(p -> {
			assertNotNull(p.getProductId());
			assertNotNull(p.getProductName());
			assertNotNull(p.getProductKanaName());
			assertNotNull(p.getJanCode());
			assertNotNull(p.getProductDescription());
			assertNotNull(p.getInsDatetime());
			assertNotNull(p.getUpdDatetime());
			assertNotNull(p.getVersionNo());
		});
		assertThat(products.size(), is(2));
	}

	@Test
	void testStream() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.query(Product.class)
				.in("product_id", 0, 1)
				.stream().forEach(p -> {
					assertNotNull(p.getProductId());
					assertNotNull(p.getProductName());
					assertNotNull(p.getProductKanaName());
					assertNotNull(p.getJanCode());
					assertNotNull(p.getProductDescription());
					assertNotNull(p.getInsDatetime());
					assertNotNull(p.getUpdDatetime());
					assertNotNull(p.getVersionNo());
				});
		assertThat(agent.query(Product.class).in("product_id", 0, 1).count(), is(2L));
	}

	@Test
	void testFirst() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var product = agent.query(Product.class)
				.in("product_id", 0, 1)
				.first();

		assertThat(product.isPresent(), is(true));

		product.ifPresent(p -> {
			assertNotNull(p.getProductId());
			assertNotNull(p.getProductName());
			assertNotNull(p.getProductKanaName());
			assertNotNull(p.getJanCode());
			assertNotNull(p.getProductDescription());
			assertNotNull(p.getInsDatetime());
			assertNotNull(p.getUpdDatetime());
			assertNotNull(p.getVersionNo());
		});

		var empty = agent.query(Product.class)
				.in("product_id", 10)
				.first();

		assertThat(empty.isPresent(), is(false));
	}

	@Test
	void testOne() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query(Product.class).in("product_id", 0, 1)
					.one();
			fail();
		} catch (DataNonUniqueException ex) {
			// OK
		}

		var product = agent.query(Product.class)
				.in("product_id", 0)
				.one();

		assertThat(product.isPresent(), is(true));

		product.ifPresent(p -> {
			assertNotNull(p.getProductId());
			assertNotNull(p.getProductName());
			assertNotNull(p.getProductKanaName());
			assertNotNull(p.getJanCode());
			assertNotNull(p.getProductDescription());
			assertNotNull(p.getInsDatetime());
			assertNotNull(p.getUpdDatetime());
			assertNotNull(p.getVersionNo());
		});

		var empty = agent.query(Product.class)
				.in("product_id", 10)
				.one();

		assertThat(empty.isPresent(), is(false));
	}

	@Test
	void testCollectParam() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var products = agent.query(Product.class)
				.equal("productId", 0)
				.collect();

		products.forEach(p -> {
			assertNotNull(p.getProductId());
			assertNotNull(p.getProductName());
			assertNotNull(p.getProductKanaName());
			assertNotNull(p.getJanCode());
			assertNotNull(p.getProductDescription());
			assertNotNull(p.getInsDatetime());
			assertNotNull(p.getUpdDatetime());
			assertNotNull(p.getVersionNo());
		});
		assertThat(products.size(), is(1));
	}

	@Test
	void testSelect() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// camel case
		var productName = agent.query(Product.class)
				.equal("product_id", 1)
				.select("productName", String.class).findFirst().get();
		assertEquals(productName, "商品名1");

		// snake case
		var janCode = agent.query(Product.class)
				.equal("product_id", 1)
				.select("jan_code", String.class).findFirst().get();
		assertEquals(janCode, "1234567890124");

		// multiple case
		List<String> productNames = agent.query(Product.class)
				.select("productName", String.class)
				.collect(Collectors.toList());
		assertEquals(productNames.size(), 2);
		assertEquals(productNames.get(0), "商品名0");
		assertEquals(productNames.get(1), "商品名1");

		// exception case
		try {
			agent.query(Product.class)
					.equal("product_id", 1)
					.select("noMatchField", String.class);
			fail();
		} catch (UroborosqlRuntimeException ex) {
			assertEquals("field:noMatchField not found in Product.", ex.getMessage());
		} catch (Exception ex) {
			fail();
		}
	}

	@Test
	void testIncludeColumns() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 通常の検索
		var product = agent.query(Product.class)
				.equal("product_id", 1)
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// 引数なし　（カラムの絞り込みなし）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.includeColumns()
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		//　null指定　（カラムの絞り込みなし）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.includeColumns((String[]) null)
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// productIdのみ
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.includeColumns("productId")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is(nullValue()));
		assertThat(product.getProductKanaName(), is(nullValue()));
		assertThat(product.getJanCode(), nullValue());
		assertThat(product.getProductDescription(), nullValue());
		assertThat(product.getInsDatetime(), nullValue());
		assertThat(product.getUpdDatetime(), nullValue());
		assertThat(product.getVersionNo(), is(0));

		// productId, productName
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.includeColumns("productId", "productName")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is(nullValue()));
		assertThat(product.getJanCode(), nullValue());
		assertThat(product.getProductDescription(), nullValue());
		assertThat(product.getInsDatetime(), nullValue());
		assertThat(product.getUpdDatetime(), nullValue());
		assertThat(product.getVersionNo(), is(0));

		// productName (先頭カラムを含まない)
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.includeColumns("productName")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is(nullValue()));
		assertThat(product.getJanCode(), nullValue());
		assertThat(product.getProductDescription(), nullValue());
		assertThat(product.getInsDatetime(), nullValue());
		assertThat(product.getUpdDatetime(), nullValue());
		assertThat(product.getVersionNo(), is(0));

		// 片方存在しないカラムの指定（存在するカラムのみで絞り込み）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.includeColumns("productId1", "productName")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is(nullValue()));
		assertThat(product.getJanCode(), nullValue());
		assertThat(product.getProductDescription(), nullValue());
		assertThat(product.getInsDatetime(), nullValue());
		assertThat(product.getUpdDatetime(), nullValue());
		assertThat(product.getVersionNo(), is(0));

		try {
			// すべて存在しないカラムの指定
			product = agent.query(Product.class)
					.equal("product_id", 1)
					.includeColumns("productId1", "productName1") // 存在しないカラム名
					.first()
					.orElseThrow(UroborosqlSQLException::new);
			fail();
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("None of the includeColumns matches the column name."));
		}
	}

	@Test
	void testExcludeColumns() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 通常の検索
		var product = agent.query(Product.class)
				.equal("product_id", 1)
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// 引数なし（絞り込み無し）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns()
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// null指定（絞り込み無し）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns((String[]) null)
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// janCodeのみ
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns("janCode")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is(nullValue()));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// productId, productName
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns("productId", "productName")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is(nullValue()));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// productId (先頭カラム)
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns("productId")
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(0));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// 片方存在しないカラムを指定　（存在するカラムで絞り込み）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns("productId1", "productName") // 存在しないカラム
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is(nullValue()));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));

		// すべて存在しないカラムを指定　（全カラム取得される）
		product = agent.query(Product.class)
				.equal("product_id", 1)
				.excludeColumns("productId1", "productName1") // 存在しないカラム
				.first()
				.orElseThrow(UroborosqlSQLException::new);

		assertThat(product, not(nullValue()));
		assertThat(product.getProductId(), is(1));
		assertThat(product.getProductName(), is("商品名1"));
		assertThat(product.getProductKanaName(), is("ショウヒンメイイチ"));
		assertThat(product.getJanCode(), is("1234567890124"));
		assertThat(product.getProductDescription(), is("1番目の商品"));
		assertThat(product.getInsDatetime(), not(nullValue()));
		assertThat(product.getUpdDatetime(), not(nullValue()));
		assertThat(product.getVersionNo(), is(0));
	}

	@Test
	void testOptimizerHints() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var products = agent.query(Product.class)
				.hint("IX_PRODUCT")
				.collect();
		assertThat(products.size(), is(2));
	}

}
