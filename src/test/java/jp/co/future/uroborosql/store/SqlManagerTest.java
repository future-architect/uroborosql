package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;

import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class SqlManagerTest {
	@Test
	public void testGet() throws Exception {
		SqlManagerImpl manager = new SqlManagerImpl();
		manager.setSqlLoader(new SqlLoaderImpl());
		manager.initialize();

		String sql = manager.getSql("example/select_product");
		assertThat(sql,
				is("SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator()));
	}

	@Test
	public void testGet2() throws Exception {
		SqlManagerImpl manager = new SqlManagerImpl();
		manager.initialize();

		String sql = manager.getSql("example/select_product");
		assertThat(sql,
				is("SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator()));
	}

	@Test
	public void testConstructor() throws Exception {
		SqlManagerImpl manager = new SqlManagerImpl();
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sql"));
		assertThat(manager.getFileExtension(), is(".sql"));

		manager = new SqlManagerImpl("sql");
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sql"));
		assertThat(manager.getFileExtension(), is(".sql"));

		manager = new SqlManagerImpl("sqlx", ".sqlx");
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sqlx"));
		assertThat(manager.getFileExtension(), is(".sqlx"));

		manager = new SqlManagerImpl(false);
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sql"));
		assertThat(manager.getFileExtension(), is(".sql"));
		assertThat(manager.isCache(), is(false));

		manager = new SqlManagerImpl("sqlx", false);
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sqlx"));
		assertThat(manager.getFileExtension(), is(".sql"));
		assertThat(manager.isCache(), is(false));

		List<String> loadPaths = new ArrayList<>();
		try {
			manager = new SqlManagerImpl(loadPaths);
			fail();
		} catch (IllegalArgumentException ex) {
			assertThat(ex.getMessage(), is("loadPaths is required. loadPaths=[]"));
		}

		loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		manager = new SqlManagerImpl(loadPaths);
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sql"));
		assertThat(manager.getFileExtension(), is(".sql"));
		assertThat(manager.isCache(), is(true));

		loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		manager = new SqlManagerImpl(loadPaths, ".sqlx");
		manager.initialize();
		assertThat(manager.getLoadPath(), is("sql"));
		assertThat(manager.getFileExtension(), is(".sqlx"));
		assertThat(manager.isCache(), is(true));

	}

	@Test
	public void testShutdown() throws Exception {
		SqlManagerImpl manager = new SqlManagerImpl();
		manager.initialize();
		assertThat(manager.existSql("example/select_product"), is(true));
		manager.shutdown();
		assertThat(manager.existSql("example/select_product"), is(false));
	}

	@Test
	public void testExistSql() throws Exception {
		SqlManagerImpl manager = new SqlManagerImpl();
		manager.initialize();

		assertThat(manager.existSql("example/select_product"), is(true));
		assertThat(manager.existSql("example/no_file"), is(false));
		assertThat(manager.existSql("example/select_in_secondary_sql_folder"), is(false));
		assertThat(manager.existSql("other/select_in_secondary_sql_other_folder"), is(false));
		assertThat(manager.existSql("other/no_file"), is(false));
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testGetSql() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);
		manager.setCache(false);
		manager.initialize();

		assertThat(manager.getSql("example/select_product"), is(containsString("SELECT")));
		assertThat(manager.getSql("example/select_in_secondary_sql_folder"),
				is(containsString("secondary_sql/example")));

		manager.getSql("example/no_file");
	}

	@Test
	public void testExistSqlWithMultiFolder() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);
		manager.setCache(false);
		manager.initialize();

		assertThat(manager.existSql("example/select_product"), is(true));
		assertThat(manager.existSql("example/no_file"), is(false));
		assertThat(manager.existSql("example/select_in_secondary_sql_folder"), is(true));
		assertThat(manager.existSql("other/select_in_secondary_sql_other_folder"), is(true));
		assertThat(manager.existSql("other/no_file"), is(false));
	}

	@Test
	@Ignore
	public void testGetSqlPathList() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);
		manager.initialize();

		assertThat(manager.getSqlPathList().size(), is(36));
	}

	@Test
	public void testGetSqlLoader() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);

		assertThat(manager.getSqlLoader().getLoadPath(), is("sql"));
	}

	@Test
	public void testSetSqlLoader() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);

		manager.setSqlLoader(new SqlLoaderImpl("new_sql"));

		assertThat(manager.getSqlLoader().getLoadPath(), is("new_sql"));
	}

	@Test
	public void testSetLoadPath() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);

		manager.setLoadPath("new_sql");

		assertThat(manager.getLoadPath(), is("new_sql"));
	}

	@Test
	public void testSetFileExtension() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);

		manager.setFileExtension(".sqlx");

		assertThat(manager.getFileExtension(), is(".sqlx"));
	}

	@Test
	public void testSetDialect() throws Exception {
		List<String> loadPaths = new ArrayList<>();
		loadPaths.add("sql");
		loadPaths.add("secondary_sql");
		SqlManagerImpl manager = new SqlManagerImpl(loadPaths);

		Dialect dialect = new PostgresqlDialect();
		manager.setDialect(dialect);

		assertThat(manager.getDialect(), is(dialect));
	}

}
