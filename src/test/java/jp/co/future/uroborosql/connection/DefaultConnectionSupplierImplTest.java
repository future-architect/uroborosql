package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.Connection;
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
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		Connection conn = DriverManager.getConnection(url, user, password);

		DefaultConnectionSupplierImpl supplier = new DefaultConnectionSupplierImpl(conn);
		assertThat(supplier.getConnection(), instanceOf(CloseIgnoringConnectionWrapper.class));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGetConnectionWithProps() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		Connection conn = DriverManager.getConnection(url, user, password);

		DefaultConnectionSupplierImpl supplier = new DefaultConnectionSupplierImpl(conn);
		supplier.getConnection(null);
	}

}
