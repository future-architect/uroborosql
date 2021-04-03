package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Paths;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

public class SqlEntityDeleteTest extends AbstractDbTest {

	@Test
	public void testCountByInstance() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			Product product = new Product();
			product.setProductId(0);
			assertThat(agent.delete(product), is(1));
		});
	}

	@Test
	public void testCountByClass() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			assertThat(agent.delete(Product.class).equal("productId", 1).count(), is(1));
		});
	}

	@Test
	public void testDeleteAndReturn() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			Product product = new Product();
			product.setProductId(1);
			assertThat(agent.deleteAndReturn(product), is(product));
		});
	}

	@Test
	public void testDeleteThrowException() {
		// 事前条件
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		agent.required(() -> {
			Product product = new Product();
			product.setProductId(1);
			assertThrows(IllegalArgumentException.class, () -> agent.delete(Stream.of(product)));
		});
	}

}
