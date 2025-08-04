package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

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
		assertThat(supplier.getConnection().isWrapperFor(CloseIgnoringConnectionWrapper.class), is(true));
		assertThat(supplier.getConnection().unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(false));
	}

	@Test
	public void testDefaultConnectionSupplierImplWithCache() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		Connection conn = DriverManager.getConnection(url, user, password);

		DefaultConnectionSupplierImpl supplier = new DefaultConnectionSupplierImpl(conn, true);
		assertThat(supplier.getConnection().isWrapperFor(CloseIgnoringConnectionWrapper.class), is(true));
		assertThat(supplier.getConnection().unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(true));
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
