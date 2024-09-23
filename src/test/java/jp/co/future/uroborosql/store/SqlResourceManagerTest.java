package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class SqlResourceManagerTest {
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
		var manager = new SqlResourceManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();
		List.of("example/select_test",
				"example/select_test2",
				"example/select_test3")
				.forEach(sqlName -> manager.getSql(sqlName));

		var pathList = manager.getSqlPathList();
		assertThat(pathList, hasItem("example/select_test"));
		assertThat(pathList, hasItem("example/select_test2"));
		assertThat(pathList, hasItem("example/select_test3"));
	}

	@Test
	void testGetSqlPath() throws Exception {
		var manager = new SqlResourceManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();
		List.of("example/select_test")
				.forEach(sqlName -> manager.getSql(sqlName));

		assertThat(manager.getSqlPath("example/select_test"), is(Paths.get("sql/h2/example/select_test.sql")));
		assertThat(manager.getSqlPath("example/select_test2"), is(Paths.get("sql/example/select_test2.sql")));
		assertThat(manager.getSqlPath("example/select_test3"), is(Paths.get("sql/h2/example/select_test3.sql")));
		assertThrows(UroborosqlRuntimeException.class, () -> manager.getSqlPath("example/select_test4"));
	}

	@Test
	void testGetSqlName() throws Exception {
		var manager = new SqlResourceManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();
		List.of("example/select_test")
				.forEach(sqlName -> manager.getSql(sqlName));

		assertThat(manager.getSqlName(Paths.get("sql/h2/example/select_test.sql")), is("example/select_test"));
		assertThat(manager.getSqlName(Paths.get("sql/example/select_test2.sql")), is("example/select_test2"));
		assertThat(manager.getSqlName(Paths.get("sql/h2/example/select_test3.sql")), is("example/select_test3"));
		assertThat(manager.getSqlName(Paths.get("nosql/example/select_test4.sql")), is("nosql/example/select_test4"));
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
		var manager = new SqlResourceManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			assertThat(manager.getSql("example/select_test"), containsString("H2DB"));
			assertThat(manager.getSql("example/select_test"), containsString("file"));

			assertThat(manager.getSql("example/select_test2"), containsString("default"));
			assertThat(manager.getSql("example/select_test2"), containsString("zip"));

			assertThat(manager.getSql("example/select_test3"), containsString("H2DB"));
			assertThat(manager.getSql("example/select_test3"), containsString("zip"));

			assertThat(manager.existSql("example/select_test"), is(true));
			assertThat(manager.existSql("example/select_test2"), is(true));
			assertThat(manager.existSql("example/select_test3"), is(true));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void testGetSqlPostgresql() throws Exception {
		var manager = new SqlResourceManagerImpl();
		manager.setDialect(new PostgresqlDialect());
		manager.initialize();

		try {
			assertThat(manager.getSql("example/select_test"), containsString("postgresql"));
			assertThat(manager.getSql("example/select_test"), containsString("file"));

			assertThat(manager.getSql("example/select_test2"), containsString("default"));
			assertThat(manager.getSql("example/select_test2"), containsString("zip"));

			assertThat(manager.getSql("example/select_test3"), containsString("postgresql"));
			assertThat(manager.getSql("example/select_test3"), containsString("zip"));

			assertThat(manager.existSql("example/select_test"), is(true));
			assertThat(manager.existSql("example/select_test2"), is(true));
			assertThat(manager.existSql("example/select_test3"), is(true));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void testConstructorLoadPathHasChildDirAndClassLoader() throws Exception {
		var manager = new SqlResourceManagerImpl("parent/child/sql", ".sql",
				Charset.defaultCharset(), Thread.currentThread().getContextClassLoader());
		assertThat(manager.getCharset(), is(Charset.defaultCharset()));

		Dialect dialect = new H2Dialect();
		manager.setDialect(dialect);
		manager.initialize();

		assertThat(manager.getDialect(), is(dialect));

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
		var manager = new SqlResourceManagerImpl("parent/child/sql");
		manager.setDialect(new H2Dialect());
		manager.initialize();
		List.of("example/select_test",
				"example/select_test2",
				"example/select_test3")
				.forEach(sqlName -> manager.getSql(sqlName));

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
		var manager = new SqlResourceManagerImpl("parent/child/sql");
		manager.setDialect(new H2Dialect());
		manager.initialize();

		try {
			assertThat(manager.getSql("example/select_test"), containsString("H2DB"));
			assertThat(manager.getSql("example/select_test"), containsString("file"));

			assertThat(manager.getSql("example/select_test2"), containsString("default"));
			assertThat(manager.getSql("example/select_test2"), containsString("zip"));

			assertThat(manager.getSql("example/select_test3"), containsString("H2DB"));
			assertThat(manager.getSql("example/select_test3"), containsString("zip"));

			assertThat(manager.existSql("example/select_test"), is(true));
			assertThat(manager.existSql("example/select_test2"), is(true));
			assertThat(manager.existSql("example/select_test3"), is(true));
		} finally {
			manager.shutdown();
		}
	}

	@Test
	void testGetSqlPostgresqlLoadPathHasChildDir() throws Exception {
		var manager = new SqlResourceManagerImpl("parent/child/sql");
		manager.setDialect(new PostgresqlDialect());
		manager.initialize();

		try {
			assertThat(manager.getSql("example/select_test"), containsString("postgresql"));
			assertThat(manager.getSql("example/select_test"), containsString("file"));

			assertThat(manager.getSql("example/select_test2"), containsString("default"));
			assertThat(manager.getSql("example/select_test2"), containsString("zip"));

			assertThat(manager.getSql("example/select_test3"), containsString("postgresql"));
			assertThat(manager.getSql("example/select_test3"), containsString("zip"));

			assertThat(manager.existSql("example/select_test"), is(true));
			assertThat(manager.existSql("example/select_test2"), is(true));
			assertThat(manager.existSql("example/select_test3"), is(true));
		} finally {
			manager.shutdown();
		}
	}
}
