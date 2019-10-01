package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Test;

import jp.co.future.uroborosql.exception.OptimisticLockException;

public class SqlEntityUpdateTest extends AbstractDbTest {

	@Test
	public void testCountByClass() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(agent.update(Product.class).set("productName", "商品名_new").equal("productId", 1).count(), is(1));
			assertThat(agent.query(Product.class).equal("productId", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	@Test
	public void testCountByClass2() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(agent.update(Product.class).set("productName", "商品名_new").greaterEqual("productId", 0).count(),
					is(2));
			assertThat(agent.query(Product.class).equal("productId", 0).one().get().getProductName(), is("商品名_new"));
			assertThat(agent.query(Product.class).equal("productId", 1).one().get().getProductName(), is("商品名_new"));
		});
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。
	 */
	@Test
	public void testEntityUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {
			Product product = new Product();
			product.setProductId(1);
			product.setProductName("商品名_new");
			assertThat(product.getVersionNo(), is(0));

			assertThat(agent.update(product), is(1));
			assertThat(product.getVersionNo(), is(1));
			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名_new"));
			assertThat(agent.find(Product.class, 2).get().getVersionNo(), is(0));
		});
	}

	/**
	 * Entityを使ったDB更新処理のテストケース。(楽観ロックエラー）
	 */
	@Test(expected = OptimisticLockException.class)
	public void testEntityUpdateThrowException() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {
			Product product = new Product();
			product.setProductId(2);
			product.setProductName("商品名_new");
			product.setVersionNo(1);
			agent.update(product);
		});
	}

	/**
	 * Entityを使った一括更新処理のテストケース。
	 */
	@Test
	public void testEntityBatchUpdate() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			}).collect(Collectors.toList());
			assertThat(agent.updates(products.stream()), is(2));

			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 1).get().getProductKanaName(), nullValue());
			assertThat(agent.find(Product.class, 2).get().getProductName(), is("商品名2_new"));
			assertThat(agent.find(Product.class, 2).get().getProductKanaName(), nullValue());
		});
	}

	/**
	 * Entityを使った一括更新処理のテストケース。
	 */
	@Test
	public void testEntityBatchUpdateWithCondition() throws Exception {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteBatch.ltsv"));

		agent.required(() -> {

			List<Product> products = agent.query(Product.class).stream().map(p -> {
				p.setProductName(p.getProductName() + "_new");
				p.setProductKanaName(null);
				return p;
			}).collect(Collectors.toList());
			assertThat(agent.updates(products.stream(), (ctx, count, e) -> count == 1), is(2));

			assertThat(agent.find(Product.class, 1).get().getProductName(), is("商品名1_new"));
			assertThat(agent.find(Product.class, 1).get().getProductKanaName(), nullValue());
			assertThat(agent.find(Product.class, 2).get().getProductName(), is("商品名2_new"));
			assertThat(agent.find(Product.class, 2).get().getProductKanaName(), nullValue());
		});
	}

}
