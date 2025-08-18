package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.DriverManager;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl.SchemaOption;

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
		assertThat(supplier.getConnection().isWrapperFor(CloseIgnoringConnectionWrapper.class), is(true));
		assertThat(supplier.getConnection().unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(false));
	}

	@Test
	public void testDefaultConnectionSupplierImplWithCache() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn, SchemaOption.CACHE);
		assertThat(supplier.getConnection().isWrapperFor(CloseIgnoringConnectionWrapper.class), is(true));
		assertThat(supplier.getConnection().unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(true));
	}

	@Test
	public void testDefaultConnectionSupplierImplWithFix() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn, SchemaOption.FIX);
		assertThat(supplier.getConnection().isWrapperFor(CloseIgnoringConnectionWrapper.class), is(true));
		assertThat(supplier.getConnection().isWrapperFor(SchemaFixedConnectionWrapper.class), is(true));
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
