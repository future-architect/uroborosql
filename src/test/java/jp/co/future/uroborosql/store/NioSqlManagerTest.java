package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import static org.junit.Assume.*;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.dialect.Oracle10Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

import org.junit.Test;

public class NioSqlManagerTest {
	private static final int WAIT_TIME = 100;
	private static final String TARGET_TEST_CLASSES_SQL = "target/test-classes/sql/";

	@Test
	public void testConstructor() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl("sql", ".sql", Charset.defaultCharset());
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));

	}

	@Test
	public void testGetSqlPathList() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		List<String> pathList = manager.getSqlPathList();
		assertThat(pathList, hasItem("example/select_test"));
		assertThat(pathList, hasItem("example/select_test2"));
		assertThat(pathList, hasItem("example/select_test3"));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testGetSqlLoader() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.getSqlLoader();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testSetSqlLoader() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setSqlLoader(null);
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testIsCache() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.isCache();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void testSetCache() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setCache(false);
	}

	@Test
	public void testGetSql() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			manager.getSql("example/select_test");
		} catch (Exception ex) {
			fail();
		}
		try {
			manager.getSql("example/select_test_no_file");
			fail();
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("sql file not found. sqlName : example/select_test_no_file"));
		} catch (Exception ex) {
			fail();
		}
	}

	@Test
	public void testGetSqlH2() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			assertThat(manager.existSql("example/select_test"), is(true));
			assertThat(manager.existSql("example/select_test2"), is(true));
			assertThat(manager.existSql("example/select_test3"), is(true));

			assertThat(manager.getSql("example/select_test"), containsString("H2DB"));
			assertThat(manager.getSql("example/select_test"), containsString("file"));

			assertThat(manager.getSql("example/select_test2"), containsString("default"));
			assertThat(manager.getSql("example/select_test2"), containsString("zip"));

			assertThat(manager.getSql("example/select_test3"), containsString("H2DB"));
			assertThat(manager.getSql("example/select_test3"), containsString("zip"));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void testGetSqlPostgresql() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setDialect(new PostgresqlDialect());
		manager.initialize();

		try {
			assertThat(manager.existSql("example/select_test"), is(true));
			assertThat(manager.existSql("example/select_test2"), is(true));
			assertThat(manager.existSql("example/select_test3"), is(true));

			assertThat(manager.getSql("example/select_test"), containsString("postgresql"));
			assertThat(manager.getSql("example/select_test"), containsString("file"));

			assertThat(manager.getSql("example/select_test2"), containsString("default"));
			assertThat(manager.getSql("example/select_test2"), containsString("zip"));

			assertThat(manager.getSql("example/select_test3"), containsString("postgresql"));
			assertThat(manager.getSql("example/select_test3"), containsString("zip"));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void testGetSqlWithWatcher() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		String sqlName = "test/ADD_WATCH";
		Path newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		NioSqlManagerImpl manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {
			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			Files.write(newFilePath, Arrays.asList("select * from ADD_WATCH"));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));

			Thread.sleep(WAIT_TIME);

			Files.deleteIfExists(newFilePath);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));
		} finally {
			manager.shutdown();
		}

	}

	@Test
	public void testGetSqlWithNoWatcher() throws Exception {

		String sqlName = "test/ADD_WATCH";
		Path newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {
			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			Files.write(newFilePath, Arrays.asList("select * from ADD_WATCH"));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql("example/select_test"), is(true));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void testAddDialectSqlFolder() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		String sqlName = "example/select_test";
		Path dir = Paths.get(TARGET_TEST_CLASSES_SQL, "oracle", "example");
		Path newFilePath = dir.resolve("select_test.sql");
		Files.deleteIfExists(newFilePath);
		Files.deleteIfExists(dir);

		NioSqlManagerImpl manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("default"));

			Thread.sleep(WAIT_TIME);

			Files.createDirectories(dir);

			String sql = "select * from test -- oracle";
			Files.write(newFilePath, Arrays.asList(sql));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("oracle"));

			Thread.sleep(WAIT_TIME);

			Files.deleteIfExists(newFilePath);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("default"));

			Files.deleteIfExists(dir);
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void testAddDefaultFolderAndDialectFolder() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		String sqlName = "unit_test/select_test";
		Path defaultDir = Paths.get(TARGET_TEST_CLASSES_SQL, "unit_test");
		Path dialectDir = Paths.get(TARGET_TEST_CLASSES_SQL, "oracle", "unit_test");
		Path defaultFilePath = defaultDir.resolve("select_test.sql");
		Path dialectFilePath = dialectDir.resolve("select_test.sql");
		Files.deleteIfExists(defaultFilePath);
		Files.deleteIfExists(defaultDir);
		Files.deleteIfExists(dialectFilePath);
		Files.deleteIfExists(dialectDir);

		NioSqlManagerImpl manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			// defaultから先に作る場合
			Files.createDirectories(defaultDir);

			String sql = "select * from test -- default";
			Files.write(defaultFilePath, Arrays.asList(sql));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("default"));

			Thread.sleep(WAIT_TIME);

			Files.createDirectories(dialectDir);

			sql = "select * from test -- oracle";
			Files.write(dialectFilePath, Arrays.asList(sql));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("oracle"));

			Thread.sleep(WAIT_TIME);

			Files.deleteIfExists(defaultFilePath);
			Files.deleteIfExists(defaultDir);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("oracle"));

			Files.deleteIfExists(dialectFilePath);
			Files.deleteIfExists(dialectDir);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void testAddDialectFolderAndDefaultFolder() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		String sqlName = "unit_test/select_test";
		Path defaultDir = Paths.get(TARGET_TEST_CLASSES_SQL, "unit_test");
		Path dialectDir = Paths.get(TARGET_TEST_CLASSES_SQL, "oracle", "unit_test");
		Path defaultFilePath = defaultDir.resolve("select_test.sql");
		Path dialectFilePath = dialectDir.resolve("select_test.sql");
		Files.deleteIfExists(defaultFilePath);
		Files.deleteIfExists(defaultDir);
		Files.deleteIfExists(dialectFilePath);
		Files.deleteIfExists(dialectDir);

		NioSqlManagerImpl manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			// dialectから先に作る場合
			Files.createDirectories(dialectDir);

			String sql = "select * from test -- oracle";
			Files.write(dialectFilePath, Arrays.asList(sql));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("oracle"));

			Thread.sleep(WAIT_TIME);

			Files.createDirectories(defaultDir);

			sql = "select * from test -- default";
			Files.write(defaultFilePath, Arrays.asList(sql));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("oracle")); // default より dialectが優先される

			Thread.sleep(WAIT_TIME);

			Files.deleteIfExists(dialectFilePath);
			Files.deleteIfExists(dialectDir);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("default")); // dialect が削除された段階でdefalutが有効になる

			Thread.sleep(WAIT_TIME);

			Files.deleteIfExists(defaultFilePath);
			Files.deleteIfExists(defaultDir);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));
		} finally {
			manager.shutdown();
		}
	}
}
