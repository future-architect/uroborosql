package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

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
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn);
		assertThat(supplier.getConnection(), instanceOf(CloseIgnoringConnectionWrapper.class));
	}

	@Test
	public void testGetConnectionWithProps() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var conn = DriverManager.getConnection(url, user, password);

		var supplier = new DefaultConnectionSupplierImpl(conn);
		assertThrows(UnsupportedOperationException.class, () -> supplier.getConnection(null));
	}

}
