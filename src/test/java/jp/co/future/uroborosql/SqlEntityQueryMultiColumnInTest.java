/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.model.Product;

/**
 * SqlEntityQuery Multi-Column IN Test
 */
public class SqlEntityQueryMultiColumnInTest extends AbstractDbTest {

	/**
	 * Multi-column IN条件のテスト用Bean
	 */
	public static class ProductInParam {
		private Integer productId;
		private String productName;

		public ProductInParam(final Integer productId, final String productName) {
			this.productId = productId;
			this.productName = productName;
		}

		public Integer getProductId() {
			return productId;
		}

		public void setProductId(final Integer productId) {
			this.productId = productId;
		}

		public String getProductName() {
			return productName;
		}

		public void setProductName(final String productName) {
			this.productName = productName;
		}
	}

	@Test
	void testMultiColumnIn() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// テスト用のBeanを作成
		var inParams = List.of(
				new ProductInParam(0, "商品名0"),
				new ProductInParam(1, "商品名1"));

		var products = agent.query(Product.class)
				.in(inParams)
				.collect();

		assertThat(products.size(), is(2));
		assertThat(products.get(0).getProductId(), is(0));
		assertThat(products.get(1).getProductId(), is(1));
	}

	@Test
	void testMultiColumnInWithEmptyList() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 空のリストでテスト
		var inParams = List.<ProductInParam>of();

		var products = agent.query(Product.class)
				.in(inParams)
				.collect();

		assertThat(products.size(), is(0));
	}

	@Test
	void testMultiColumnInWithSingleRow() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 1行だけのリストでテスト
		var inParams = List.of(new ProductInParam(0, "商品名0"));

		var products = agent.query(Product.class)
				.in(inParams)
				.collect();

		assertThat(products.size(), is(1));
		assertThat(products.get(0).getProductId(), is(0));
		assertThat(products.get(0).getProductName(), is("商品名0"));
	}

	@Test
	void testMultiColumnInCount() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// count()と組み合わせ
		var inParams = List.of(
				new ProductInParam(0, "商品名0"),
				new ProductInParam(1, "商品名1"));

		var count = agent.query(Product.class)
				.in(inParams)
				.count();

		assertThat(count, is(2L));
	}

	@Test
	void testMultiColumnInFirst() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// first()と組み合わせ
		var inParams = List.of(
				new ProductInParam(0, "商品名0"),
				new ProductInParam(1, "商品名1"));

		var product = agent.query(Product.class)
				.in(inParams)
				.first();

		assertThat(product.isPresent(), is(true));
		assertThat(product.get().getProductId(), is(0));
	}

	@Test
	void testMultiColumnInWithOtherConditions() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 他の条件と組み合わせ
		var inParams = List.of(
				new ProductInParam(0, "商品名0"),
				new ProductInParam(1, "商品名1"));

		var products = agent.query(Product.class)
				.in(inParams)
				.greaterEqual("productId", 0)
				.collect();

		assertThat(products.size(), is(2));
	}
}
