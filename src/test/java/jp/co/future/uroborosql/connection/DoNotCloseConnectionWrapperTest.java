package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.NClob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.SQLXML;
import java.sql.Statement;
import java.util.Properties;
import java.util.concurrent.Executors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class DoNotCloseConnectionWrapperTest {
	private Connection target;
	private Connection conn;

	@SuppressWarnings("deprecation")
	@BeforeEach
	public void setUp() throws Exception {
		conn = DriverManager.getConnection("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";MODE=DB2",
				"sa", "sa");
		target = new DoNotCloseConnectionWrapper(conn);
	}

	@AfterEach
	public void tearDown() throws Exception {
		conn.close();
	}

	@Test
	public void testCreateStatement() throws Exception {
		assertThat(target.createStatement(), is(instanceOf(Statement.class)));
		assertThat(target.createStatement(
				ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY),
				is(instanceOf(Statement.class)));
		assertThat(target.createStatement(
				ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY,
				ResultSet.HOLD_CURSORS_OVER_COMMIT),
				is(instanceOf(Statement.class)));
	}

	@Test
	public void testPrepareStatement() throws Exception {
		var sql = "select * from information_schema.columns";
		assertThat(target.prepareStatement(sql),
				is(instanceOf(PreparedStatement.class)));
		assertThat(target.prepareStatement(sql, 0),
				is(instanceOf(PreparedStatement.class)));
		assertThat(target.prepareStatement(sql, new int[] { 0 }),
				is(instanceOf(PreparedStatement.class)));
		assertThat(target.prepareStatement(sql, new String[] { "TABLE_CATALOG" }),
				is(instanceOf(PreparedStatement.class)));
		assertThat(target.prepareStatement(sql,
				ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY),
				is(instanceOf(PreparedStatement.class)));
		assertThat(target.prepareStatement(sql,
				ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY,
				ResultSet.HOLD_CURSORS_OVER_COMMIT),
				is(instanceOf(PreparedStatement.class)));
	}

	@Test
	public void testPrepareCall() throws Exception {
		var sql = "select * from information_schema.columns";
		assertThat(target.prepareCall(sql),
				is(instanceOf(CallableStatement.class)));
		assertThat(target.prepareCall(sql,
				ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY),
				is(instanceOf(CallableStatement.class)));
		assertThat(target.prepareCall(sql,
				ResultSet.TYPE_FORWARD_ONLY,
				ResultSet.CONCUR_READ_ONLY,
				ResultSet.HOLD_CURSORS_OVER_COMMIT),
				is(instanceOf(CallableStatement.class)));
	}

	@Test
	public void testNativeSQL() throws Exception {
		var sql = "select * from information_schema.columns";
		assertThat(target.nativeSQL(sql), is(sql));
	}

	@Test
	public void testAutoCommit() throws Exception {
		target.setAutoCommit(true);
		assertThat(target.getAutoCommit(), is(true));
		target.setAutoCommit(false);
		assertThat(target.getAutoCommit(), is(false));
	}

	@Test
	public void testIsClosed() throws Exception {
		target.commit();
		target.rollback();
		target.close();
		assertThat(target.isClosed(), is(false));
	}

	@Test
	public void testGetMetaData() throws Exception {
		assertThat(target.getMetaData(), is(instanceOf(DatabaseMetaData.class)));
	}

	@Test
	public void testReadOnly() throws Exception {
		target.setReadOnly(false);
		assertThat(target.isReadOnly(), is(false));
	}

	@Test
	public void testCatalog() throws Exception {
		var catalog = target.getCatalog();
		target.setCatalog(catalog);
		assertThat(target.getCatalog(), is(catalog));
	}

	@Test
	public void testTransactionIsolation() throws Exception {
		target.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
		assertThat(target.getTransactionIsolation(), is(Connection.TRANSACTION_SERIALIZABLE));
	}

	@Test
	public void testGetWarnings() throws Exception {
		target.clearWarnings();
		assertThat(target.getWarnings(), is(nullValue()));
	}

	@Test
	public void testTypeMap() throws Exception {
		var typeMap = target.getTypeMap();
		target.setTypeMap(typeMap);
		assertThat(target.getTypeMap(), is(nullValue()));
	}

	@Test
	public void testHoldability() throws Exception {
		target.setHoldability(ResultSet.CLOSE_CURSORS_AT_COMMIT);
		assertThat(target.getHoldability(), is(ResultSet.CLOSE_CURSORS_AT_COMMIT));
	}

	@Test
	public void testSavepoint() throws Exception {
		var savepoint = target.setSavepoint();
		assertThat(savepoint, not(nullValue()));
		target.releaseSavepoint(savepoint);

		savepoint = target.setSavepoint("point1");
		assertThat(savepoint.getSavepointName(), is("point1"));
		target.releaseSavepoint(savepoint);
	}

	@Test
	public void testCreateClob() throws Exception {
		assertThat(target.createClob(), is(instanceOf(Clob.class)));
	}

	@Test
	public void testCreateBlob() throws Exception {
		assertThat(target.createBlob(), is(instanceOf(Blob.class)));
	}

	@Test
	public void testCreateNClob() throws Exception {
		assertThat(target.createNClob(), is(instanceOf(NClob.class)));
	}

	@Test
	public void testCreateSQLXML() throws Exception {
		assertThat(target.createSQLXML(), is(instanceOf(SQLXML.class)));
	}

	@Test
	public void testCreateStruct() throws Exception {
		assertThrows(SQLFeatureNotSupportedException.class, () -> target.createStruct("char", new Object[] {}));
	}

	@Test
	public void testIsValid() throws Exception {
		assertThat(target.isValid(1), is(true));
	}

	@Test
	public void testClientInfo() throws Exception {
		target.setClientInfo("ApplicationName", "app");
		assertThat(target.getClientInfo("ApplicationName"), is("app"));

		var props = new Properties();
		props.put("ClientUser", "user1");
		target.setClientInfo(props);
		assertThat(target.getClientInfo("ClientUser"), is("user1"));
		assertThat(target.getClientInfo(), is(instanceOf(Properties.class)));
	}

	@Test
	public void testCreateArrayOf() throws Exception {
		assertThat(target.createArrayOf("char", new Object[] {}), is(instanceOf(Array.class)));
	}

	@Test
	public void testSchema() throws Exception {
		target.setSchema("PUBLIC");
		assertThat(target.getSchema(), is("PUBLIC"));
	}

	@Test
	public void testNetworkTimeout() throws Exception {
		var service = Executors.newSingleThreadExecutor();
		target.setNetworkTimeout(service, 10);
		assertThat(target.getNetworkTimeout(), is(0)); // H2 not supported. return fixed value 0.
		target.abort(service); // H2 not supported.
	}

	@Test
	public void testUnwrap() throws Exception {
		assertThat(target.isWrapperFor(Connection.class), is(true));
		assertThat(target.unwrap(Connection.class), is(instanceOf(Connection.class)));
	}

}
