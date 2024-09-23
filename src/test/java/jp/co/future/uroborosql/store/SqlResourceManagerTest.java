package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assumptions.assumeFalse;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.dialect.Oracle10Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class SqlResourceManagerTest {
	private static final int WAIT_TIME = 1000;
	private static final String TARGET_TEST_CLASSES_SQL1 = "target/test-classes/sql/";
	private static final String TARGET_TEST_CLASSES_SQL2 = "target/test-classes/parent/child/sql/";

	@Test
	void testConstructor() throws Exception {
		var manager = new SqlResourceManagerImpl("sql", ".sql", Charset.defaultCharset());
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));
	}

	@Test
	void testConstructorMultiSqlPaths() throws Exception {
		var manager = new SqlResourceManagerImpl(List.of("sql", "secondary_sql"));
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));
	}

	@Test
	void testGetSqlPathList() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new SqlResourceManagerImpl(true);
		manager.setDialect(new H2Dialect());
		manager.initialize();

		var pathList = manager.getSqlPathList();
		assertThat(pathList, hasItem("example/select_test"));
		assertThat(pathList, hasItem("example/select_test2"));
		assertThat(pathList, hasItem("example/select_test3"));
	}

	@Test
	void testGetSql() throws Exception {
		var manager = new SqlResourceManagerImpl();
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
	void testGetSqlWithMultiSqlPaths() throws Exception {
		var manager = new SqlResourceManagerImpl(List.of("sql", "secondary_sql"));
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
	void testGetSqlWithMultiSqlPathsReverse() throws Exception {
		var manager = new SqlResourceManagerImpl(List.of("secondary_sql", "sql"));
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
	void testGetSqlH2() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new SqlResourceManagerImpl(true);
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
	void testGetSqlPostgresql() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new SqlResourceManagerImpl(true);
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
	void testGetSqlWithNoWatcher() throws Exception {

		var sqlName = "test/ADD_WATCH";
		var newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL1, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		var manager = new SqlResourceManagerImpl(true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {
			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			Files.write(newFilePath, List.of("select * from ADD_WATCH"));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql("example/select_test"), is(true));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void testConstructorLoadPathHasChildDir() throws Exception {
		var manager = new SqlResourceManagerImpl("parent/child/sql", ".sql",
				Charset.defaultCharset());
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));

	}

	@Test
	void testGetSqlPathListLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new SqlResourceManagerImpl("parent/child/sql", true);
		manager.setDialect(new H2Dialect());
		manager.initialize();

		var pathList = manager.getSqlPathList();
		assertThat(pathList, hasItem("example/select_test"));
		assertThat(pathList, hasItem("example/select_test2"));
		assertThat(pathList, hasItem("example/select_test3"));
	}

	@Test
	void testGetSqlLoadPathHasChildDir() throws Exception {
		var manager = new SqlResourceManagerImpl("parent/child/sql");
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
	void testGetSqlH2LoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new SqlResourceManagerImpl("parent/child/sql", true);
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
	void testGetSqlPostgresqlLoadPathHasChildDir() throws Exception {
		assumeFalse(System.getProperty("os.name").toLowerCase().startsWith("mac"));

		var manager = new SqlResourceManagerImpl("parent/child/sql", true);
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
	void testGetSqlWithNoWatcherLoadPathHasChildDir() throws Exception {

		var sqlName = "test/ADD_WATCH";
		var newFilePath = Paths.get(TARGET_TEST_CLASSES_SQL2, sqlName + ".sql");
		Files.deleteIfExists(newFilePath);

		var manager = new SqlResourceManagerImpl("parent/child/sql", true);
		manager.setDialect(new Oracle10Dialect());
		manager.initialize();

		try {
			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			Files.write(newFilePath, List.of("select * from ADD_WATCH"));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql(sqlName), is(false));

			Thread.sleep(WAIT_TIME);

			assertThat(manager.existSql("example/select_test"), is(true));
		} finally {
			manager.shutdown();
		}
	}

}
