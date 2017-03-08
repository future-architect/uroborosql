package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.List;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.testlog.TestAppender;

import org.apache.commons.lang3.StringUtils;
import org.dbunit.database.DatabaseConnection;
import org.dbunit.dataset.excel.XlsDataSet;
import org.dbunit.operation.DatabaseOperation;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class AuditLogSqlFilterTest {
	private static final String DB_NAME = "alftestdb";

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {

		// DBが作成されていない場合にテーブルを作成する
		try (Connection conn = DriverManager.getConnection("jdbc:derby:target/db/" + DB_NAME
				+ ";create=true;user=test;password=test")) {
			SQLWarning warning = conn.getWarnings();
			conn.setAutoCommit(false);
			if (warning == null) {
				// テーブル作成
				try (Statement stmt = conn.createStatement()) {

					String[] sqls = new String(Files.readAllBytes(Paths
							.get("src/test/resources/sql/ddl/create_tables.sql")), StandardCharsets.UTF_8).split(";");
					for (String sql : sqls) {
						if (StringUtils.isNotBlank(sql)) {
							stmt.execute(sql);
						}
					}
					conn.commit();
				}
			}
		}
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
		try {
			DriverManager.getConnection("jdbc:derby:target/db/" + DB_NAME + ";shutdown=true");
			throw new SQLException("切断されなかった！");
		} catch (SQLException se) {
			if ("08006".equals(se.getSQLState())) {
				// 正常にシャットダウンされた
			} else {
				// シャットダウン失敗
				throw se;
			}
		}
	}

	private SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:derby:target/db/" + DB_NAME
				+ ";create=false", "test", "test"));

		SqlFilterManager sqlFilterManager;

		sqlFilterManager = config.getSqlFilterManager();

		AuditLogSqlFilter filter = new AuditLogSqlFilter();
		filter.initialize();

		sqlFilterManager.addSqlFilter(filter);
	}

	@After
	public void tearDown() throws Exception {
		Connection conn = config.getConnectionSupplier().getConnection();
		conn.rollback();
		conn.close();
	}

	/**
	 * dbUnit用Connection取得処理。<br>
	 *
	 * @return
	 * @throws SQLException
	 */
	private DatabaseConnection getDatabeseConnection() throws Exception {
		Connection conn = config.getConnectionSupplier().getConnection();
		String schema = conn.getMetaData().getUserName();
		DatabaseConnection databaseConnection = new DatabaseConnection(conn, schema);
		return databaseConnection;
	}

	@Test
	public void testExecuteQueryFilter() throws Exception {
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/auditlogsqlfilter/setup/testExecuteQuery.xls")));

		List<String> log = TestAppender.getLogbackLogs(() -> {
			try (SqlAgent agent = config.createAgent()) {
				SqlContext ctx = agent.contextFrom("example/select")
						.paramList("product_id", new BigDecimal("0"), new BigDecimal("2"))
						.param("_userName", "testUserName").param("_funcId", "testFunction").setSqlId("111");
				ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

				agent.query(ctx);
			}
		});

		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/jp/co/future/uroborosql/auditlogsqlfilter/testExecuteQueryFilter.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteUpdateFilter() throws Exception {
		DatabaseConnection databaseConnection = getDatabeseConnection();
		DatabaseOperation.CLEAN_INSERT.execute(
				databaseConnection,
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/auditlogsqlfilter/setup/testExecuteUpdate.xls")));
		List<String> log = TestAppender.getLogbackLogs(() -> {
			try (SqlAgent agent = config.createAgent()) {
				SqlContext ctx = agent.contextFrom("example/selectinsert").setSqlId("222")
						.param("_userName", "testUserName").param("_funcId", "testFunction")
						.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
						.param("jan_code", "1234567890123", Types.CHAR);

				agent.update(ctx);

			}
		});
		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/jp/co/future/uroborosql/auditlogsqlfilter/testExecuteUpdateFilter.txt"),
				StandardCharsets.UTF_8)));
	}

	@Test
	public void testExecuteBatchFilter() throws Exception {
		DatabaseConnection databaseConnection = getDatabeseConnection();
		DatabaseOperation.TRUNCATE_TABLE.execute(
				databaseConnection,
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/auditlogsqlfilter/setup/testExecuteBatch.xls")));

		Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
		List<String> log = TestAppender.getLogbackLogs(() -> {
			try (SqlAgent agent = config.createAgent()) {
				SqlContext ctx = agent.contextFrom("example/batchinsert").setSqlId("333")
						.param("product_id", new BigDecimal(1)).param("product_name", "商品名1")
						.param("product_kana_name", "ショウヒンメイイチ").param("jan_code", "1234567890123")
						.param("product_description", "1番目の商品").param("ins_datetime", currentDatetime)
						.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch()
						.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
						.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
						.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
						.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0))
						.param("_userName", "testUserName").param("_funcId", "testFunction").addBatch();

				agent.batch(ctx);
			}
		});
		assertThat(log, is(Files.readAllLines(
				Paths.get("src/test/resources/jp/co/future/uroborosql/auditlogsqlfilter/testExecuteBatchFilter.txt"),
				StandardCharsets.UTF_8)));
	}

	public void assertFile(final String expectedFilePath, final String actualFilePath) throws IOException {
		String expected = new String(Files.readAllBytes(Paths.get(expectedFilePath)), Charset.forName("UTF-8"));
		String actual = new String(Files.readAllBytes(Paths.get(actualFilePath)), Charset.forName("UTF-8"));

		assertEquals(expected, actual);
	}
}
