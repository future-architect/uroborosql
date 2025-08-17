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

		JdbcDataSource ds1 = new JdbcDataSource();
		ds1.setURL(URL1);
		JdbcDataSource ds2 = new JdbcDataSource();
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
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testDataSourceConnectionWithName() throws Exception {
		@SuppressWarnings("deprecation")
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl(DS_NAME2);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL2));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testDataSourceConnectionWithDataSource() throws Exception {
		String url = "jdbc:h2:mem:ds";
		JdbcDataSource dataSource = new JdbcDataSource();
		dataSource.setURL(url);

		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl(dataSource);
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(url));
			assertThat(conn.getSchema(), is("PUBLIC"));
			assertThat(conn.getAutoCommit(), is(false));
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testGetConnectionWithContext() throws Exception {
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
		}

		try (Connection conn = supplier.getConnection(ConnectionContextBuilder.dataSource(DS_NAME2))) {
			assertThat(conn.getMetaData().getURL(), is(URL2));
		}
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testGetConnectionMissingName() throws Exception {
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.getConnection(ConnectionContextBuilder.dataSource("dummy"));
	}

	@Test
	public void testGetDefaultDataSourceName() throws Exception {
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		assertThat(supplier.getDefaultDataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		String dataSourceName = "changedDataSourceName";
		supplier.setDefaultDataSourceName(dataSourceName);
		assertThat(supplier.getDefaultDataSourceName(), is(dataSourceName));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetDefaultDataSourceNameIsNull() throws Exception {
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultDataSourceName(null);
	}

	@Test
	public void testSetDefaultAutoCommit() throws Exception {
		boolean autoCommit = true;
		boolean readonly = false;

		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultAutoCommit(autoCommit);
		assertThat(supplier.isDefaultAutoCommit(), is(autoCommit));
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			assertThat(conn.isReadOnly(), is(readonly));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultReadOnly() throws Exception {
		boolean autoCommit = false;
		boolean readonly = true;

		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultReadOnly(readonly);
		assertThat(supplier.isDefaultReadOnly(), is(readonly));
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.getAutoCommit(), is(autoCommit));
			// assertThat(conn.isReadOnly(), is(readonly)); // H2はreadonlyオプションが適用されないためコメントアウト
			assertThat(conn.isReadOnly(), is(false));
			assertThat(conn.getTransactionIsolation(), is(Connection.TRANSACTION_READ_COMMITTED));
		}
	}

	@Test
	public void testSetDefaultCacheSchema() throws Exception {
		boolean cache = true;

		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultCacheSchema(cache);
		assertThat(supplier.isDefaultCacheSchema(), is(cache));
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.getMetaData().getURL(), is(URL1));
			assertThat(conn.unwrap(MetadataCachedConnectionWrapper.class).isCacheSchema(), is(cache));
		}
	}

	@Test
	public void testSetDefaultFixSchema() throws Exception {
		boolean fixed = true;

		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultFixSchema(fixed);
		assertThat(supplier.isDefaultFixSchema(), is(fixed));
		try (Connection conn = supplier.getConnection()) {
			assertThat(conn.isWrapperFor(SchemaFixedConnectionWrapper.class), is(true));
		}
	}

	@Test
	public void testSetDefaultTransactionIsolation() throws Exception {
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.setDefaultTransactionIsolation(Connection.TRANSACTION_READ_UNCOMMITTED);
		assertThat(supplier.getDefaultTransactionIsolation(), is(Connection.TRANSACTION_READ_UNCOMMITTED));
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
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		assertThat(supplier.getDatabaseName(), is("H2-1.4"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetConnectionMissingClass() throws Exception {
		DataSourceConnectionSupplierImpl supplier = new DataSourceConnectionSupplierImpl();
		supplier.getConnection(ConnectionContextBuilder.jdbc("dummy"));
	}

}
