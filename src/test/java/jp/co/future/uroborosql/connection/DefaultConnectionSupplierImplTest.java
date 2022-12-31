package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertThat;

import java.sql.DriverManager;

import org.junit.Test;

/**
 * Testcase of {@link DefaultConnectionSupplierImpl}
 *
 * @author H.Sugimoto
 */
public class DefaultConnectionSupplierImplTest {

	@Test
	public void testDefaultConnectionSupplierImpl() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn);
		assertThat(supplier.getConnection(), instanceOf(CloseIgnoringConnectionWrapper.class));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGetConnectionWithProps() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn);
		supplier.getConnection(null);
	}

}
