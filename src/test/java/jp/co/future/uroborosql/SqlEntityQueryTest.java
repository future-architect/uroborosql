package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.DataNonUniqueException;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class SqlEntityQueryTest extends AbstractDbTest {

	@Test
	public void testCollect() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var products = agent.query(Product.class)
				.in("product_id", 0, 1)
				.collect();

		products.forEach(p -> {
			assertThat(p.getProductId(), not(nullValue()));
			assertThat(p.getProductName(), not(nullValue()));
			assertThat(p.getProductKanaName(), not(nullValue()));
			assertThat(p.getJanCode(), not(nullValue()));
			assertThat(p.getProductDescription(), not(nullValue()));
			assertThat(p.getInsDatetime(), not(nullValue()));
			assertThat(p.getUpdDatetime(), not(nullValue()));
			assertThat(p.getVersionNo(), not(nullValue()));
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
					assertThat(p.getProductId(), not(nullValue()));
					assertThat(p.getProductName(), not(nullValue()));
					assertThat(p.getProductKanaName(), not(nullValue()));
					assertThat(p.getJanCode(), not(nullValue()));
					assertThat(p.getProductDescription(), not(nullValue()));
					assertThat(p.getInsDatetime(), not(nullValue()));
					assertThat(p.getUpdDatetime(), not(nullValue()));
					assertThat(p.getVersionNo(), not(nullValue()));
				});
		assertThat(agent.query(Product.class).in("product_id", 0, 1).count(), is(2L));
	}

	@Test
	public void testFirst() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var product = agent.query(Product.class)
				.in("product_id", 0, 1)
				.first();

		assertThat(product.isPresent(), is(true));

		product.ifPresent(p -> {
			assertThat(p.getProductId(), not(nullValue()));
			assertThat(p.getProductName(), not(nullValue()));
			assertThat(p.getProductKanaName(), not(nullValue()));
			assertThat(p.getJanCode(), not(nullValue()));
			assertThat(p.getProductDescription(), not(nullValue()));
			assertThat(p.getInsDatetime(), not(nullValue()));
			assertThat(p.getUpdDatetime(), not(nullValue()));
			assertThat(p.getVersionNo(), not(nullValue()));
		});

		var empty = agent.query(Product.class)
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
			assertThat("Fail here.", false);
		} catch (DataNonUniqueException ex) {
			// OK
		}

		var product = agent.query(Product.class)
				.in("product_id", 0)
				.one();

		assertThat(product.isPresent(), is(true));

		product.ifPresent(p -> {
			assertThat(p.getProductId(), not(nullValue()));
			assertThat(p.getProductName(), not(nullValue()));
			assertThat(p.getProductKanaName(), not(nullValue()));
			assertThat(p.getJanCode(), not(nullValue()));
			assertThat(p.getProductDescription(), not(nullValue()));
			assertThat(p.getInsDatetime(), not(nullValue()));
			assertThat(p.getUpdDatetime(), not(nullValue()));
			assertThat(p.getVersionNo(), not(nullValue()));
		});

		var empty = agent.query(Product.class)
				.in("product_id", 10)
				.one();

		assertThat(empty.isPresent(), is(false));
	}

	@Test
	public void testCollectParam() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var products = agent.query(Product.class)
				.equal("productId", 0)
				.collect();

		products.forEach(p -> {
			assertThat(p.getProductId(), not(nullValue()));
			assertThat(p.getProductName(), not(nullValue()));
			assertThat(p.getProductKanaName(), not(nullValue()));
			assertThat(p.getJanCode(), not(nullValue()));
			assertThat(p.getProductDescription(), not(nullValue()));
			assertThat(p.getInsDatetime(), not(nullValue()));
			assertThat(p.getUpdDatetime(), not(nullValue()));
			assertThat(p.getVersionNo(), not(nullValue()));
		});
		assertThat(products.size(), is(1));
	}

	@Test
	public void testSelect() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// camel case
		var productName = agent.query(Product.class)
				.equal("product_id", 1)
				.select("productName", String.class).findFirst().get();
		assertThat(productName, is("商品名1"));

		// snake case
		var janCode = agent.query(Product.class)
				.equal("product_id", 1)
				.select("jan_code", String.class).findFirst().get();
		assertThat(janCode, is("1234567890124"));

		// multiple case
		List<String> productNames = agent.query(Product.class)
				.select("productName", String.class)
				.collect(Collectors.toList());
		assertThat(productNames.size(), is(2));
		assertThat(productNames.get(0), is("商品名0"));
		assertThat(productNames.get(1), is("商品名1"));

		// exception case
		try {
			agent.query(Product.class)
					.equal("product_id", 1)
					.select("noMatchField", String.class);
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("field:noMatchField not found in Product."));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
	}

	@Test
	public void testOptimizerHints() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var products = agent.query(Product.class)
				.hint("IX_PRODUCT")
				.collect();
		assertThat(products.size(), is(2));
	}

}
