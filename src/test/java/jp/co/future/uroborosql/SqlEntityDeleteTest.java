package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.file.Paths;

import org.junit.Test;

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

}
