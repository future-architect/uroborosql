package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.file.Paths;
import java.util.List;
import java.util.Optional;

import org.junit.Test;

import jp.co.future.uroborosql.exception.DataNonUniqueException;

public class SqlEntityQueryTest extends AbstractDbTest {

	@Test
	public void testCollect() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		List<Product> products = agent.query(Product.class)
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
	public void testStream() {
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
	public void testFirst() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		Optional<Product> product = agent.query(Product.class)
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

		Optional<Product> empty = agent.query(Product.class)
				.in("product_id", 10)
				.first();

		assertThat(empty.isPresent(), is(false));
	}

	@Test
	public void testOne() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try {
			agent.query(Product.class).in("product_id", 0, 1).one();
			fail();
		} catch (DataNonUniqueException ex) {
			// OK
		}

		Optional<Product> product = agent.query(Product.class)
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

		Optional<Product> empty = agent.query(Product.class)
				.in("product_id", 10)
				.one();

		assertThat(empty.isPresent(), is(false));
	}

	@Test
	public void testCollectParam() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		@SuppressWarnings("deprecation")
		List<Product> products = agent.query(Product.class)
				.param("productId", 0)
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

}
