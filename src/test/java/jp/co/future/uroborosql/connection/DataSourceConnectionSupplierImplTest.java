package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.sql.Connection;

import javax.naming.Context;
import javax.naming.InitialContext;

import org.h2.jdbcx.JdbcDataSource;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

/**
 * Testcase for {@link DataSourceConnectionSupplierImpl}
 *
 * @author H.Sugimoto
 */
public class DataSourceConnectionSupplierImplTest {
	private static final String URL1 = "jdbc:h2:mem:ds1";
	private static final String URL2 = "jdbc:h2:mem:ds2";
	private static final String DS_NAME1 = DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME;
	private static final String DS_NAME2 = "java:comp/env/jdbc/second_datasource";

	@BeforeClass
	public static void setUpClass() throws Exception {
		System.setProperty(Context.INITIAL_CONTEXT_FACTORY, "jp.co.future.uroborosql.connection.LocalContextFactory");
		System.setProperty(Context.URL_PKG_PREFIXES, "local");

		var ds1 = new JdbcDataSource();
		ds1.setURL(URL1);
		var ds2 = new JdbcDataSource();
		ds2.setURL(URL2);

		Context ic = new InitialContext();
		ic.createSubcontext("java:comp");
		ic.createSubcontext("java:comp/env");
		ic.createSubcontext("java:comp/env/jdbc");
		ic.bind(DS_NAME1, ds1);
		ic.bind(DS_NAME2, ds2);

	}

	@Test
	public void testDataSourceConnection() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testDataSourceConnectionWithDataSource() throws Exception {
		var url = "jdbc:h2:mem:ds";
		var dataSource = new JdbcDataSource();
		dataSource.setURL(url);

		var supplier = new DataSourceConnectionSupplierImpl(dataSource);
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testGetConnectionWithContext() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
		}

		try (var conn = supplier.getConnection(ConnectionContextBuilder.dataSource(DS_NAME2))) {
			assertThat(conn.getMetaData().getURL(), is(URL2));
		}
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testGetConnectionMissingName() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		supplier.getConnection(ConnectionContextBuilder.dataSource("dummy"));
	}

	@Test
	public void testGetDefaultDataSourceName() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		assertThat(supplier.getDefaultDataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		var dataSourceName = "changedDataSourceName";
		supplier.setDefaultDataSourceName(dataSourceName);
		assertThat(supplier.getDefaultDataSourceName(), is(dataSourceName));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetDefaultDataSourceNameIsNull() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultDataSourceName(null);
	}

	@Test
	public void testSetDefaultAutoCommit() throws Exception {
		var autoCommit = true;
		var readonly = false;

		var supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultAutoCommit(autoCommit);
		assertThat(supplier.isDefaultAutoCommit(), is(autoCommit));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			assertThat(conn.isReadOnly(), is(readonly));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultReadOnly() throws Exception {
		var autoCommit = false;
		var readonly = true;

		var supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultReadOnly(readonly);
		assertThat(supplier.isDefaultReadOnly(), is(readonly));
		try (var conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			// assertThat(conn.isReadOnly(), is(readonly)); // H2はreadonlyオプションが適用されないためコメントアウト
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultTransactionIsolation() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
		assertThat(supplier.getDefaultTransactionIsolation(), is(Connection.TRANSACTION_READ_UNCOMMITTED));
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
	public void testGetDatabaseName() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		assertThat(supplier.getDatabaseName(), is("H2-1.4"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetConnectionMissingClass() throws Exception {
		var supplier = new DataSourceConnectionSupplierImpl();
		supplier.getConnection(ConnectionContextBuilder.jdbc("dummy"));
	}

}
