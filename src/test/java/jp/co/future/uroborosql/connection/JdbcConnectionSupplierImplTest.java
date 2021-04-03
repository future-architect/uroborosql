package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import org.junit.jupiter.api.Test;

/**
 * Testcase for {@link JdbcConnectionSupplierImpl}
 *
 * @author H.Sugimoto
 */
public class JdbcConnectionSupplierImplTest {

	@Test
	public void testJdbcConnection() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";

		@SuppressWarnings("deprecation")
		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(url, user, password);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testJdbcConnectionWithProps() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";

		String checkSql = "select current timestamp from SYSIBM.SYSDUMMY1";

		@SuppressWarnings("deprecation")
		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(url, user, password);
		try (Connection conn = supplier.getConnection()) {
			conn.prepareStatement(checkSql);
			fail();
		} catch (SQLException ex) {
			// OK
		}

		supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder
				.jdbc("jdbc:h2:mem:db2?user=sa&password&sa")
				.user("sa")
				.password("sa")
				.set("MODE", "DB2"));
		try (Connection conn = supplier.getConnection()) {
			PreparedStatement stmt = conn.prepareStatement(checkSql);
			assertThat(stmt.executeQuery(), not(nullValue()));
		}
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testJdbcConnectionNull() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> new JdbcConnectionSupplierImpl(null, null, null));
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testNotInstanceOfJdbcConnectionContext() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";

		ConnectionContext ctx = ConnectionContextBuilder.dataSource();
		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(url, user, password);
		assertThrows(IllegalArgumentException.class, () -> supplier.getConnection(ctx));
	}

	@Test
	public void testJdbcConnectionWithSchema() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";

		@SuppressWarnings("deprecation")
		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(url, user, password, schema);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}

		JdbcConnectionContext ctx = ConnectionContextBuilder.jdbc(url + "_2", "sa", "sa", "PUBLIC")
				.transactionIsolation(Connection.TRANSACTION_READ_COMMITTED);

		try (Connection conn = supplier.getConnection(ctx)) {
			assertThat(conn.getMetaData().getURL(), is(ctx.url()));
			assertThat(conn.getSchema(), is(ctx.schema()));
			assertThat(conn.getAutoCommit(), is(ctx.autoCommit()));
			assertThat(conn.isReadOnly(), is(ctx.readOnly()));
			assertThat(conn.getTransactionIsolation(), is(ctx.transactionIsolation()));
		}
	}

	@Test
	public void testJdbcConnectionWithOption() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";
		boolean autoCommit = true;
		boolean readonly = true;

		@SuppressWarnings("deprecation")
		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(url, user, password, schema, autoCommit,
				readonly);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			// assertThat(conn.isReadOnly(), is(readonly)); // H2はreadonlyオプションが適用されないためコメントアウト
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testGetConnectionWithContext() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
		}

		String url2 = url + "_2";
		try (Connection conn = supplier.getConnection(
				ConnectionContextBuilder.jdbc(url2, user, password))) {
			assertThat(conn.getMetaData().getURL(), is(url2));
		}
	}

	@SuppressWarnings("deprecation")
	@Test
	public void testSetSchema() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(url, user, password);
		supplier.setSchema(schema);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultSchema() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultSchema(schema);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultAutoCommit() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";
		boolean autoCommit = true;
		boolean readonly = false;

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultAutoCommit(autoCommit);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			assertThat(conn.isReadOnly(), is(readonly));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultReadOnly() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";
		boolean autoCommit = false;
		boolean readonly = true;

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultReadOnly(readonly);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			// assertThat(conn.isReadOnly(), is(readonly)); // H2はreadonlyオプションが適用されないためコメントアウト
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultTransactionIsolation() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";
		String schema = "PUBLIC";

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password, schema));
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_UNCOMMITTED));
		}
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
		// H2 not support TRANSACTION_REPEATABLE_READ. TRANSACTION_SERIALIZABLEになってしまう
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_REPEATABLE_READ);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_SERIALIZABLE));
		}
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_SERIALIZABLE));
		}
		try {
			supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_NONE);
			fail();
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), containsString("Unsupported level"));
		}
	}

	@Test
	public void testGetDatabaseName() throws Exception {
		String url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		String user = "";
		String password = "";

		JdbcConnectionSupplierImpl supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		assertThat(supplier.getDatabaseName(), is("H2-1.4"));
	}

}
