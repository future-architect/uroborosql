package jp.co.future.uroborosql.store;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.dialect.Oracle10Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class NioSqlManagerTest {
	private static final int WAIT_TIME = 100;
	private static final String TARGET_TEST_CLASSES_SQL1 = "target/test-classes/sql/";
	private static final String TARGET_TEST_CLASSES_SQL2 = "target/test-classes/parent/child/sql/";

	@Test
	public void testConstructor() throws Exception {
		var manager = new NioSqlManagerImpl("sql", ".sql", Charset.defaultCharset());
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));
	}

	@Test
	public void testConstructorMultiSqlPaths() throws Exception {
		var manager = new NioSqlManagerImpl(Arrays.asList("sql", "secondary_sql"));
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));
	}

	@Test
	public void testConstructorMultiSqlPathsNull() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> new NioSqlManagerImpl(Arrays.asList(null, "secondary_sql")));
	}

	@Test
	public void testGetSqlPathList() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new NioSqlManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		var pathList = manager.getSqlPathList();
		assertThat(pathList, hasItem("example/select_test"));
		assertThat(pathList, hasItem("example/select_test2"));
		assertThat(pathList, hasItem("example/select_test3"));
	}

	@Test
	public void testGetSqlLoader() throws Exception {
		var manager = new NioSqlManagerImpl();
		assertThrows(UnsupportedOperationException.class, () -> manager.getSqlLoader());
	}

	@Test
	public void testSetSqlLoader() throws Exception {
		var manager = new NioSqlManagerImpl();
		assertThrows(UnsupportedOperationException.class, () -> manager.setSqlLoader(null));
	}

	@Test
	public void testIsCache() throws Exception {
		var manager = new NioSqlManagerImpl();
		assertThrows(UnsupportedOperationException.class, () -> manager.isCache());
	}

	@Test
	public void testSetCache() throws Exception {
		var manager = new NioSqlManagerImpl();
		assertThrows(UnsupportedOperationException.class, () -> manager.setCache(false));
	}

	@Test
	public void testGetSql() throws Exception {
		var manager = new NioSqlManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			manager.getSql("example/select_test");
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
		try {
			manager.getSql("example/select_test_no_file");
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("sql file not found. sqlName : example/select_test_no_file"));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
	}

	@Test
	public void testGetSqlWithMultiSqlPaths() throws Exception {
		var manager = new NioSqlManagerImpl(Arrays.asList("sql", "secondary_sql"));
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			manager.getSql("example/select_test");
			assertThat(manager.getSql("example/select_product"), is(containsString("SELECT /* _SQL_ID_ */")));
			assertThat(manager.getSql("example/select_in_secondary_sql_folder"),
					is(containsString("secondary_sql/example file")));
			assertThat(manager.getSql("other/select_in_secondary_sql_other_folder"),
					is(containsString("secondary_sql/other file")));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
		try {
			manager.getSql("example/select_test_no_file");
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("sql file not found. sqlName : example/select_test_no_file"));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
	}

	@Test
	public void testGetSqlWithMultiSqlPathsReverse() throws Exception {
		var manager = new NioSqlManagerImpl(Arrays.asList("secondary_sql", "sql"));
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			manager.getSql("example/select_test");
			assertThat(manager.getSql("example/select_product"), is(containsString("secondary_sql folder")));
			assertThat(manager.getSql("example/select_in_secondary_sql_folder"),
					is(containsString("secondary_sql/example file")));
			assertThat(manager.getSql("other/select_in_secondary_sql_other_folder"),
					is(containsString("secondary_sql/other file")));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
		try {
			manager.getSql("example/select_test_no_file");
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("sql file not found. sqlName : example/select_test_no_file"));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
	}

	@Test
	public void testGetSqlH2() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new NioSqlManagerImpl();
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

		var manager = new NioSqlManagerImpl();
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

		var sqlName = "test/ADD_WATCH";
		var newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL1, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		var manager = new NioSqlManagerImpl(true);
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

		var sqlName = "test/ADD_WATCH";
		var newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL1, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		var manager = new NioSqlManagerImpl();
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

		var sqlName = "example/select_test";
		var dir = Paths.get(TARGET_TEST_CLASSES_SQL1, "oracle", "example");
		var newFilePath = dir.resolve("select_test.sql");
		Files.deleteIfExists(newFilePath);
		Files.deleteIfExists(dir);

		var manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("default"));

			Thread.sleep(WAIT_TIME);

			Files.createDirectories(dir);

			var sql = "select * from test -- oracle";
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

		var sqlName = "unit_test/select_test";
		var defaultDir = Paths.get(TARGET_TEST_CLASSES_SQL1, "unit_test");
		var dialectDir = Paths.get(TARGET_TEST_CLASSES_SQL1, "oracle", "unit_test");
		var defaultFilePath = defaultDir.resolve("select_test.sql");
		var dialectFilePath = dialectDir.resolve("select_test.sql");
		Files.deleteIfExists(defaultFilePath);
		Files.deleteIfExists(defaultDir);
		Files.deleteIfExists(dialectFilePath);
		Files.deleteIfExists(dialectDir);

		var manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			// defaultから先に作る場合
			Files.createDirectories(defaultDir);

			var sql = "select * from test -- default";
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

		var sqlName = "unit_test/select_test";
		var defaultDir = Paths.get(TARGET_TEST_CLASSES_SQL1, "unit_test");
		var dialectDir = Paths.get(TARGET_TEST_CLASSES_SQL1, "oracle", "unit_test");
		var defaultFilePath = defaultDir.resolve("select_test.sql");
		var dialectFilePath = dialectDir.resolve("select_test.sql");
		Files.deleteIfExists(defaultFilePath);
		Files.deleteIfExists(defaultDir);
		Files.deleteIfExists(dialectFilePath);
		Files.deleteIfExists(dialectDir);

		var manager = new NioSqlManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			// dialectから先に作る場合
			Files.createDirectories(dialectDir);

			var sql = "select * from test -- oracle";
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
			assertThat(manager.getSql(sqlName), containsString("default")); // dialect が削除された段階でdefaultが有効になる

			Thread.sleep(WAIT_TIME);

			Files.deleteIfExists(defaultFilePath);
			Files.deleteIfExists(defaultDir);

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	public void testConstructorLoadPathHasChildDir() throws Exception {
		var manager = new NioSqlManagerImpl("parent/child/sql", ".sql", Charset.defaultCharset());
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));

	}

	@Test
	public void testGetSqlPathListLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new NioSqlManagerImpl("parent/child/sql");
		manager.setDialect(new H2Dialect());
		manager.initialize();

		var pathList = manager.getSqlPathList();
		assertThat(pathList, hasItem("example/select_test"));
		assertThat(pathList, hasItem("example/select_test2"));
		assertThat(pathList, hasItem("example/select_test3"));
	}

	@Test
	public void testGetSqlLoadPathHasChildDir() throws Exception {
		var manager = new NioSqlManagerImpl("parent/child/sql");
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			manager.getSql("example/select_test");
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
		try {
			manager.getSql("example/select_test_no_file");
			assertThat("Fail here.", false);
		} catch (UroborosqlRuntimeException ex) {
			assertThat(ex.getMessage(), is("sql file not found. sqlName : example/select_test_no_file"));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
	}

	@Test
	public void testGetSqlH2LoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new NioSqlManagerImpl("parent/child/sql");
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
	public void testGetSqlPostgresqlLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new NioSqlManagerImpl("parent/child/sql");
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
	public void testGetSqlWithWatcherLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var sqlName = "test/ADD_WATCH";
		var newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL2, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		var manager = new NioSqlManagerImpl("parent/child/sql", ".sql", StandardCharsets.UTF_8, true);
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
	public void testGetSqlWithNoWatcherLoadPathHasChildDir() throws Exception {

		var sqlName = "test/ADD_WATCH";
		var newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL2, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		var manager = new NioSqlManagerImpl("parent/child/sql");
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
	public void testAddDialectSqlFolderLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var sqlName = "example/select_test";
		var dir = Paths.get(TARGET_TEST_CLASSES_SQL2, "oracle", "example");
		var newFilePath = dir.resolve("select_test.sql");
		Files.deleteIfExists(newFilePath);
		Files.deleteIfExists(dir);

		var manager = new NioSqlManagerImpl("parent/child/sql", ".sql", StandardCharsets.UTF_8, true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(true));
			assertThat(manager.getSql(sqlName), containsString("default"));

			Thread.sleep(WAIT_TIME);

			Files.createDirectories(dir);

			var sql = "select * from test -- oracle";
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
	public void testAddDefaultFolderAndDialectFolderLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var sqlName = "unit_test/select_test";
		var defaultDir = Paths.get(TARGET_TEST_CLASSES_SQL2, "unit_test");
		var dialectDir = Paths.get(TARGET_TEST_CLASSES_SQL2, "oracle", "unit_test");
		var defaultFilePath = defaultDir.resolve("select_test.sql");
		var dialectFilePath = dialectDir.resolve("select_test.sql");
		Files.deleteIfExists(defaultFilePath);
		Files.deleteIfExists(defaultDir);
		Files.deleteIfExists(dialectFilePath);
		Files.deleteIfExists(dialectDir);

		var manager = new NioSqlManagerImpl("parent/child/sql", ".sql", StandardCharsets.UTF_8, true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			// defaultから先に作る場合
			Files.createDirectories(defaultDir);

			var sql = "select * from test -- default";
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
	public void testAddDialectFolderAndDefaultFolderLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var sqlName = "unit_test/select_test";
		var defaultDir = Paths.get(TARGET_TEST_CLASSES_SQL2, "unit_test");
		var dialectDir = Paths.get(TARGET_TEST_CLASSES_SQL2, "oracle", "unit_test");
		var defaultFilePath = defaultDir.resolve("select_test.sql");
		var dialectFilePath = dialectDir.resolve("select_test.sql");
		Files.deleteIfExists(defaultFilePath);
		Files.deleteIfExists(defaultDir);
		Files.deleteIfExists(dialectFilePath);
		Files.deleteIfExists(dialectDir);

		var manager = new NioSqlManagerImpl("parent/child/sql", ".sql", StandardCharsets.UTF_8, true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			// dialectから先に作る場合
			Files.createDirectories(dialectDir);

			var sql = "select * from test -- oracle";
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
			assertThat(manager.getSql(sqlName), containsString("default")); // dialect が削除された段階でdefaultが有効になる

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
