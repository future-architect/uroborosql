package jp.co.future.uroborosql;

import static org.junit.Assert.*;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.WrapContextSqlFilter;
import jp.co.future.uroborosql.utils.CaseFormat;

import org.apache.commons.lang3.StringUtils;
import org.dbunit.Assertion;
import org.dbunit.database.DatabaseConnection;
import org.dbunit.dataset.DefaultDataSet;
import org.dbunit.dataset.DefaultTable;
import org.dbunit.dataset.IDataSet;
import org.dbunit.dataset.ITable;
import org.dbunit.dataset.excel.XlsDataSet;
import org.dbunit.operation.DatabaseOperation;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class SqlAgentTest {
	private static final String DB_NAME = "satestdb";

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
				+ ";create=false;", "test", "test"));
	}

	@After
	public void tearDown() throws Exception {
		Connection conn = config.getConnectionSupplier().getConnection();
		if (conn != null && !conn.isClosed()) {
			conn.rollback();
			conn.close();
		}
	}

	/**
	 * dbUnit用Connection取得処理。<br>
	 *
	 * @return
	 * @throws SQLException SQL例外
	 */
	private DatabaseConnection getDatabeseConnection() throws Exception {
		Connection conn = config.getConnectionSupplier().getConnection();
		String schema = conn.getMetaData().getUserName();
		DatabaseConnection databaseConnection = new DatabaseConnection(conn, schema);
		return databaseConnection;
	}

	/**
	 * クエリ実行処理のテストケース。
	 *
	 */
	@Test
	public void testQuery() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			List<BigDecimal> productIdList = new ArrayList<>();
			productIdList.add(new BigDecimal("0"));
			productIdList.add(new BigDecimal("2"));
			SqlContext ctx = agent.contextFrom("example/select").param("product_id", productIdList)
					.setSqlId("test_sql_id");

			ResultSet rs = agent.query(ctx);
			assertNotNull("ResultSetが取得できませんでした。", rs);
			assertTrue("結果が0件です。", rs.next());
			assertEquals("0", rs.getString("PRODUCT_ID"));
			assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
			assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
			assertEquals("1234567890123", rs.getString("JAN_CODE"));
			assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
			assertFalse("結果が複数件です。", rs.next());
		}
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryFilter() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		SqlFilterManager manager = config.getSqlFilterManager();
		WrapContextSqlFilter filter = new WrapContextSqlFilter("",
				"OFFSET /*$startRowIndex*/0 ROWS FETCH NEXT /*$maxRowCount*/10 ROWS ONLY",
				".*(FOR\\sUPDATE|\\.NEXTVAL).*");
		filter.initialize();
		manager.addSqlFilter(filter);

		try (SqlAgent agent = config.createAgent()) {

			SqlContext ctx = agent.contextFrom("example/select")
					.paramList("product_id", new BigDecimal("0"), new BigDecimal("1")).param("startRowIndex", 0)
					.param("maxRowCount", 1);

			ResultSet rs = agent.query(ctx);
			assertNotNull("ResultSetが取得できませんでした。", rs);
			assertTrue("結果が0件です。", rs.next());
			assertEquals("0", rs.getString("PRODUCT_ID"));
			assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
			assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
			assertEquals("1234567890123", rs.getString("JAN_CODE"));
			assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
			assertFalse("結果が複数件です。", rs.next());
		}
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryArray() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			SqlContext ctx = agent.contextFrom("example/select").paramList("product_id", 0, 1, 2, 3);

			ResultSet rs = agent.query(ctx);
			assertNotNull("ResultSetが取得できませんでした。", rs);
			assertTrue("結果が0件です。", rs.next());
			assertEquals("0", rs.getString("PRODUCT_ID"));
			assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
			assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
			assertEquals("1234567890123", rs.getString("JAN_CODE"));
			assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		}
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentArray() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			ResultSet rs = agent.query("example/select").paramList("product_id", 0, 1, 2, 3).resultSet();
			assertNotNull("ResultSetが取得できませんでした。", rs);
			assertTrue("結果が0件です。", rs.next());
			assertEquals("0", rs.getString("PRODUCT_ID"));
			assertEquals("商品名0", rs.getString("PRODUCT_NAME"));
			assertEquals("ショウヒンメイゼロ", rs.getString("PRODUCT_KANA_NAME"));
			assertEquals("1234567890123", rs.getString("JAN_CODE"));
			assertEquals("0番目の商品", rs.getString("PRODUCT_DESCRIPTION"));
		}
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentCollect() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			List<Map<String, Object>> ans = agent.query("example/select").paramList("product_id", 0, 1, 2, 3).collect();
			assertEquals("結果の件数が一致しません。", 2, ans.size());
			Map<String, Object> map = ans.get(0);
			assertEquals(new BigDecimal("0"), map.get("PRODUCT_ID"));
			assertEquals("商品名0", map.get("PRODUCT_NAME"));
			assertEquals("ショウヒンメイゼロ", map.get("PRODUCT_KANA_NAME"));
			assertEquals("1234567890123", map.get("JAN_CODE"));
			assertEquals("0番目の商品", map.get("PRODUCT_DESCRIPTION"));
		}
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryLamda() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			SqlContext ctx = agent.contextFrom("example/select");
			ctx.paramList("product_id", 0, 1);

			Stream<Map<String, String>> stream = agent.query(ctx, (rs) -> {
				ResultSetMetaData rsmd = rs.getMetaData();

				int columnCount = rsmd.getColumnCount();

				Map<String, String> record = new LinkedHashMap<>(columnCount);

				for (int i = 1; i <= columnCount; i++) {
					record.put(rsmd.getColumnLabel(i), rs.getString(i));
				}
				return record;
			});

			stream.forEach((m) -> {
				assertTrue(m.containsKey("PRODUCT_ID"));
				assertTrue(m.containsKey("PRODUCT_NAME"));
				assertTrue(m.containsKey("PRODUCT_KANA_NAME"));
				assertTrue(m.containsKey("JAN_CODE"));
				assertTrue(m.containsKey("PRODUCT_DESCRIPTION"));
				assertTrue(m.containsKey("INS_DATETIME"));
				assertTrue(m.containsKey("UPD_DATETIME"));
				assertTrue(m.containsKey("VERSION_NO"));
			});
		}
	}

	/**
	 * クエリ実行処理のテストケース(Fluent API)。
	 */
	@Test
	public void testQueryFluentLamda() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			Stream<Map<String, Object>> stream = agent.query("example/select").paramList("product_id", 0, 1).stream();

			stream.forEach((m) -> {
				assertTrue(m.containsKey("PRODUCT_ID"));
				assertTrue(m.containsKey("PRODUCT_NAME"));
				assertTrue(m.containsKey("PRODUCT_KANA_NAME"));
				assertTrue(m.containsKey("JAN_CODE"));
				assertTrue(m.containsKey("PRODUCT_DESCRIPTION"));
				assertTrue(m.containsKey("INS_DATETIME"));
				assertTrue(m.containsKey("UPD_DATETIME"));
				assertTrue(m.containsKey("VERSION_NO"));
			});
		}
	}

	/**
	 * クエリ実行処理のテストケース。
	 */
	@Test
	public void testQueryMapResuletSetConverter() throws Exception {
		// 事前条件
		DatabaseOperation.CLEAN_INSERT.execute(
				getDatabeseConnection(),
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteQuery.xls")));

		try (SqlAgent agent = config.createAgent()) {
			SqlContext ctx = agent.contextFrom("example/select");
			ctx.paramList("product_id", 0, 1);

			List<Map<String, Object>> ans = agent.query(ctx, CaseFormat.CamelCase);
			ans.forEach((m) -> {
				assertTrue(m.containsKey("productId"));
				assertTrue(m.containsKey("productName"));
				assertTrue(m.containsKey("productKanaName"));
				assertTrue(m.containsKey("janCode"));
				assertTrue(m.containsKey("productDescription"));
				assertTrue(m.containsKey("insDatetime"));
				assertTrue(m.containsKey("updDatetime"));
				assertTrue(m.containsKey("versionNo"));
			});
		}

		try (SqlAgent agent = config.createAgent()) {
			SqlContext ctx = agent.contextFrom("example/select");
			ctx.paramList("product_id", 0, 1);

			List<Map<String, Object>> ans = agent.query(ctx, CaseFormat.SnakeCase);
			ans.forEach((m) -> {
				assertTrue(m.containsKey("PRODUCT_ID"));
				assertTrue(m.containsKey("PRODUCT_NAME"));
				assertTrue(m.containsKey("PRODUCT_KANA_NAME"));
				assertTrue(m.containsKey("JAN_CODE"));
				assertTrue(m.containsKey("PRODUCT_DESCRIPTION"));
				assertTrue(m.containsKey("INS_DATETIME"));
				assertTrue(m.containsKey("UPD_DATETIME"));
				assertTrue(m.containsKey("VERSION_NO"));
			});
		}
	}

	/**
	 * DB更新処理のテストケース。
	 */
	@Test
	public void testExecuteUpdate() throws Exception {
		// 事前条件
		DatabaseConnection databaseConnection = getDatabeseConnection();
		DatabaseOperation.CLEAN_INSERT.execute(
				databaseConnection,
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteUpdate.xls")));

		try (SqlAgent agent = config.createAgent()) {
			SqlContext ctx = agent.contextFrom("example/selectinsert")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR);

			int updateCount = agent.update(ctx);
			assertEquals("データの登録に失敗しました。", 1, updateCount);

			// 検証処理
			// 期待されるデータを取得
			IDataSet expectedDataSet = new XlsDataSet(Thread.currentThread().getContextClassLoader()
					.getResourceAsStream("jp/co/future/uroborosql/sqlagent/expected/testExecuteUpdate.xls"));

			// データベースのデータを取得
			IDataSet databaseDataSet = databaseConnection.createDataSet();
			ITable expectedTable = expectedDataSet.getTable("PRODUCT");
			ITable actualTable = databaseDataSet.getTable("PRODUCT");

			// 比較する
			Assertion.assertEquals(expectedTable, actualTable);
		}
	}

	/**
	 * DB更新処理のテストケース。(Fluent API)
	 */
	@Test
	public void testUpdateFluent() throws Exception {
		// 事前条件
		DatabaseConnection databaseConnection = getDatabeseConnection();
		DatabaseOperation.CLEAN_INSERT.execute(
				databaseConnection,
				new XlsDataSet(Thread.currentThread().getContextClassLoader()
						.getResourceAsStream("jp/co/future/uroborosql/sqlagent/setup/testExecuteUpdate.xls")));

		try (SqlAgent agent = config.createAgent()) {
			int updateCount = agent.update("example/selectinsert")
					.param("product_id", new BigDecimal("0"), JDBCType.DECIMAL)
					.param("jan_code", "1234567890123", Types.CHAR).count();
			assertEquals("データの登録に失敗しました。", 1, updateCount);

			// 検証処理
			// 期待されるデータを取得
			IDataSet expectedDataSet = new XlsDataSet(Thread.currentThread().getContextClassLoader()
					.getResourceAsStream("jp/co/future/uroborosql/sqlagent/expected/testExecuteUpdate.xls"));

			// データベースのデータを取得
			IDataSet databaseDataSet = databaseConnection.createDataSet();
			ITable expectedTable = expectedDataSet.getTable("PRODUCT");
			ITable actualTable = databaseDataSet.getTable("PRODUCT");

			// 比較する
			Assertion.assertEquals(expectedTable, actualTable);
		}
	}

	/**
	 * バッチ処理のテストケース。
	 */
	@Test
	public void testExecuteBatch() throws Exception {
		// 事前条件
		DatabaseConnection databaseConnection = getDatabeseConnection();
		DatabaseOperation.TRUNCATE_TABLE.execute(databaseConnection, new DefaultDataSet(new DefaultTable("PRODUCT")));

		// 処理実行
		try (SqlAgent agent = config.createAgent()) {
			Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

			SqlContext ctx = agent.contextFrom("example/batchinsert").param("product_id", new BigDecimal(1))
					.param("product_name", "商品名1").param("product_kana_name", "ショウヒンメイイチ")
					.param("jan_code", "1234567890123").param("product_description", "1番目の商品")
					.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0)).addBatch().param("product_id", new BigDecimal(2))
					.param("product_name", "商品名2").param("product_kana_name", "ショウヒンメイニ")
					.param("jan_code", "1234567890124").param("product_description", "2番目の商品")
					.param("ins_datetime", currentDatetime).param("upd_datetime", currentDatetime)
					.param("version_no", new BigDecimal(0)).addBatch();

			int[] count = agent.batch(ctx);
			assertEquals("データの登録件数が不正です。", 2, count.length);
			assertEquals("1行目のデータの登録に失敗しました。", 1, count[0]);
			assertEquals("2行目のデータの登録に失敗しました。", 1, count[1]);

			// 検証処理
			// 期待されるデータを取得
			IDataSet expectedDataSet = new XlsDataSet(Thread.currentThread().getContextClassLoader()
					.getResourceAsStream("jp/co/future/uroborosql/sqlagent/expected/testExecuteBatch.xls"));

			// データベースのデータを取得
			IDataSet databaseDataSet = databaseConnection.createDataSet();
			ITable expectedTable = expectedDataSet.getTable("PRODUCT");
			ITable actualTable = databaseDataSet.getTable("PRODUCT");

			// 比較する
			Assertion.assertEquals(expectedTable, actualTable);
		}
	}

	/**
	 * バッチ処理(Null挿入）のテストケース。
	 */
	@Test
	public void testExecuteBatchNull() throws Exception {
		// 事前条件
		DatabaseConnection databaseConnection = getDatabeseConnection();
		DatabaseOperation.TRUNCATE_TABLE.execute(databaseConnection, new DefaultDataSet(new DefaultTable("PRODUCT")));

		// 処理実行
		try (SqlAgent agent = config.createAgent()) {
			Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");
			SqlContext ctx = agent.contextFrom("example/batchinsert").param("product_id", new BigDecimal(1))
					.param("product_name", null).param("product_kana_name", null).param("jan_code", "1234567890123")
					.param("product_description", "1番目の商品").param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch()
					.param("product_id", new BigDecimal(2)).param("product_name", "商品名2")
					.param("product_kana_name", "ショウヒンメイニ").param("jan_code", "1234567890124")
					.param("product_description", "2番目の商品").param("ins_datetime", currentDatetime)
					.param("upd_datetime", currentDatetime).param("version_no", new BigDecimal(0)).addBatch();

			int[] count = agent.batch(ctx);
			assertEquals("データの登録件数が不正です。", 2, count.length);
			assertEquals("1行目のデータの登録に失敗しました。", 1, count[0]);
			assertEquals("2行目のデータの登録に失敗しました。", 1, count[1]);

			// 検証処理
			// 期待されるデータを取得
			IDataSet expectedDataSet = new XlsDataSet(Thread.currentThread().getContextClassLoader()
					.getResourceAsStream("jp/co/future/uroborosql/sqlagent/expected/testExecuteBatchNull.xls"));

			// データベースのデータを取得
			IDataSet databaseDataSet = databaseConnection.createDataSet();
			ITable expectedTable = expectedDataSet.getTable("PRODUCT");
			ITable actualTable = databaseDataSet.getTable("PRODUCT");

			// 比較する
			Assertion.assertEquals(expectedTable, actualTable);
		}
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	public void testNotFoundFile() throws Exception {
		try (SqlAgent agent = config.createAgent()) {
			SqlContext ctx = agent.contextFrom("file");
			agent.query(ctx);
			// 例外が発生しなかった場合
			fail();
		} catch (UroborosqlRuntimeException ex) {
			// OK
		} catch (Exception e) {
			fail(e.getMessage());
		}
	}
}
