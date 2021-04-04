package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.sql.Connection;
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
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testJdbcConnectionWithProps() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var checkSql = "select current timestamp from SYSIBM.SYSDUMMY1";

		var supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password));
		try (var conn = supplier.getConnection()) {
			conn.prepareStatement(checkSql);
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			// OK
		}

		supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder
				.jdbc("jdbc:h2:mem:db2?user=sa&password&sa")
				.user("sa")
				.password("sa")
				.set("MODE", "DB2"));
		try (var conn = supplier.getConnection()) {
			var stmt = conn.prepareStatement(checkSql);
			assertThat(stmt.executeQuery(), not(nullValue()));
		}
	}

	@Test
	public void testJdbcConnectionWithSchema() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";

		var supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password, schema));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}

		JdbcConnectionContext ctx = ConnectionContextBuilder.jdbc(url + "_2", "sa", "sa", "PUBLIC")
				.transactionIsolation(Connection.TRANSACTION_READ_COMMITTED);

		try (var conn = supplier.getConnection(ctx)) {
			assertThat(conn.getMetaData().getURL(), is(ctx.url()));
			assertThat(conn.getSchema(), is(ctx.schema()));
			assertThat(conn.getAutoCommit(), is(ctx.autoCommit()));
			assertThat(conn.isReadOnly(), is(ctx.readOnly()));
			assertThat(conn.getTransactionIsolation(), is(ctx.transactionIsolation()));
		}
	}

	@Test
	public void testJdbcConnectionWithOption() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";
		var autoCommit = true;
		var readonly = true;

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password, schema)
						.autoCommit(autoCommit)
						.readOnly(readonly));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			// assertThat(conn.isReadOnly(), is(readonly)); // H2はreadonlyオプションが適用されないためコメントアウト
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testGetConnectionWithContext() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
		}

		var url2 = url + "_2";
		try (var conn = supplier.getConnection(
				ConnectionContextBuilder.jdbc(url2, user, password))) {
			assertThat(conn.getMetaData().getURL(), is(url2));
		}
	}

	@Test
	public void testSetDefaultSchema() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultSchema(schema);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultAutoCommit() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";
		var autoCommit = true;
		var readonly = false;

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultAutoCommit(autoCommit);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			assertThat(conn.isReadOnly(), is(readonly));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultReadOnly() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";
		var autoCommit = false;
		var readonly = true;

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultReadOnly(readonly);
		try (var conn = supplier.getConnection()) {
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
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password, schema));
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_UNCOMMITTED));
		}
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
		// H2 not support TRANSACTION_REPEATABLE_READ. TRANSACTION_SERIALIZABLEになってしまう
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_REPEATABLE_READ);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_SERIALIZABLE));
		}
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_SERIALIZABLE));
		}
		try {
			supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_NONE);
			assertThat("Fail here.", false);
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), containsString("Unsupported level"));
		}
	}

	@Test
	public void testGetDatabaseName() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		assertThat(supplier.getDatabaseName(), is("H2-1.4"));
	}

}
