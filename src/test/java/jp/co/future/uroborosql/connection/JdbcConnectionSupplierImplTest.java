package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

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
	void testJdbcConnection() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var supplier = new JdbcConnectionSupplierImpl(
				new JdbcConnectionContext(url, user, password));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	void testJdbcConnectionWithProps() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var checkSql = "select current timestamp from SYSIBM.SYSDUMMY1";

		var supplier = new JdbcConnectionSupplierImpl(
				new JdbcConnectionContext(url, user, password));
		try (var conn = supplier.getConnection()) {
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
		try (var conn = supplier.getConnection()) {
			var stmt = conn.prepareStatement(checkSql);
			assertThat(stmt.executeQuery(), not(nullValue()));
		}
	}

	@Test
	void testJdbcConnectionNull() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {
			new JdbcConnectionSupplierImpl(new JdbcConnectionContext(null, null, null));
		});
	}

	@Test
	void testNotInstanceOfJdbcConnectionContext() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> {
			var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
			var user = "";
			var password = "";

			ConnectionContext ctx = ConnectionContextBuilder.dataSource();
			var supplier = new JdbcConnectionSupplierImpl(
					new JdbcConnectionContext(url, user, password));
			supplier.getConnection(ctx);
		});
	}

	@Test
	void testJdbcConnectionWithSchema() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";

		var supplier = new JdbcConnectionSupplierImpl(
				new JdbcConnectionContext(url, user, password, schema));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}

		var ctx = (JdbcConnectionContext) ConnectionContextBuilder.jdbc(url + "_2", "sa", "sa", "PUBLIC")
				.transactionIsolation(Connection.TRANSACTION_READ_COMMITTED);

		try (var conn = supplier.getConnection(ctx)) {
			assertThat(conn.getMetaData().getURL(), is(ctx.url()));
			assertThat(conn.getSchema(), is(ctx.schema()));
			assertThat(conn.getAutoCommit(), is(ctx.autoCommit()));
			assertThat(conn.isReadOnly(), is(ctx.readOnly()));
			assertThat(conn.unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(false));
			assertThat(conn.getTransactionIsolation(), is(ctx.transactionIsolation()));
		}
	}

	@Test
	void testJdbcConnectionWithOption() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";
		var autoCommit = true;
		var readonly = true;

		var supplier = new JdbcConnectionSupplierImpl(
				new JdbcConnectionContext(url, user, password, schema));
		supplier.setDefaultAutoCommit(autoCommit);
		supplier.setDefaultReadOnly(readonly);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			// assertThat(conn.isReadOnly(), is(readonly)); // H2はreadonlyオプションが適用されないためコメントアウト
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	void testGetConnectionWithContext() throws Exception {
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
	void testSetSchema() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";

		var supplier = new JdbcConnectionSupplierImpl(
				new JdbcConnectionContext(url, user, password));
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
	void testSetDefaultSchema() throws Exception {
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
	void testSetDefaultAutoCommit() throws Exception {
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
	void testSetDefaultReadOnly() throws Exception {
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
	public void testSetDefaultCacheSchema() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";
		var cache = true;

		var supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultCacheSchema(cache);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(true));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultFixSchema() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";
		var schema = "PUBLIC";
		var fixed = true;

		var supplier = new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password));
		supplier.setDefaultFixSchema(fixed);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is(schema));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isWrapperFor(MetadataCachedConnectionWrapper.class), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	void testSetDefaultTransactionIsolation() throws Exception {
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
			fail();
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), containsString("Unsupported level"));
		}
	}

	@Test
	void testGetDatabaseName() throws Exception {
		var url = "jdbc:h2:mem:" + this.getClass().getSimpleName();
		var user = "";
		var password = "";

		var supplier = new JdbcConnectionSupplierImpl(
				ConnectionContextBuilder.jdbc(url, user, password));
		assertThat(supplier.getDatabaseName(), is("H2-1.4"));
	}

}
