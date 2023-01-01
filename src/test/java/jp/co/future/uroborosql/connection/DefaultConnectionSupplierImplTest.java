package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.DriverManager;

import org.junit.jupiter.api.Test;

/**
 * Testcase of {@link DefaultConnectionSupplierImpl}
 *
 * @author H.Sugimoto
 */
public class DefaultConnectionSupplierImplTest {

	@Test
	void testDefaultConnectionSupplierImpl() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn);
		assertThat(supplier.getConnection(), instanceOf(CloseIgnoringConnectionWrapper.class));
	}

	@Test
	void testGetConnectionWithProps() throws Exception {
		assertThrows(UnsupportedOperationException.class, () -> {
			var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
			var user = "";
			var password = "";
			var conn = DriverManager.getConnection(url, user, password);

			var supplier = new DefaultConnectionSupplierImpl(conn);
			supplier.getConnection(null);
		});
	}

}
