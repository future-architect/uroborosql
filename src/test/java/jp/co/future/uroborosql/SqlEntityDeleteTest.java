package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.file.Paths;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.model.Product;

public class SqlEntityDeleteTest extends AbstractDbTest {

	@Test
	void testCountByInstance() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			var product = new Product();
			product.setProductId(0);
			assertThat(agent.delete(product), is(1));
		});
	}

	@Test
	void testCountByClass() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(agent.delete(Product.class).equal("productId", 1).count(), is(1));
		});
	}

	@Test
	void testDeleteAndReturn() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			var product = new Product();
			product.setProductId(1);
			assertThat(agent.deleteAndReturn(product), is(product));
		});
	}

	@Test
	void testDeleteThrowException() {
		assertThrows(IllegalArgumentException.class, () -> {
			// 事前条件
			cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

			agent.required(() -> {
				var product = new Product();
				product.setProductId(1);
				agent.delete(Stream.of(product));
			});
		});
	}

	@Test
	void testContextAttrs() {
		var delete = agent.delete(Product.class);
		delete.contextAttrs().put("dummyValue", Integer.MAX_VALUE);
		assertThat(delete.contextAttrs().get("dummyValue"), is(Integer.MAX_VALUE));
	}

}
