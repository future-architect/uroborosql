package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.Connection;
import java.sql.DriverManager;

import org.junit.jupiter.api.Test;

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

	@Test
	public void testGetConnectionWithProps() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		Connection conn = DriverManager.getConnection(url, user, password);

		DefaultConnectionSupplierImpl supplier = new DefaultConnectionSupplierImpl(conn);
		assertThrows(UnsupportedOperationException.class, () -> supplier.getConnection(null));
	}

}
