package jp.co.future.uroborosql.store;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;

import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class SqlLoaderTest {

	@Test
	public void testLoad() {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		Map<String, String> sqls = sqlLoader.load();
		assertThat(sqls, not(nullValue()));

		String sql = sqls.get("example/select_product");
		assertThat(sql,
				is("SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator()));
	}

	@Test
	public void testLoadEmptyFolder() {
		SqlLoader sqlLoader = new SqlLoaderImpl("no_sql");
		Map<String, String> sqls = sqlLoader.load();
		assertThat(sqls.isEmpty(), is(true));
	}

	@Test
	public void testLoadFile() throws Exception {
		SqlLoader sqlLoader = new SqlLoaderImpl();
		String sql = sqlLoader.load("example/select_product");
		assertThat(sql,
				is("SELECT /* _SQL_ID_ */" + System.lineSeparator() + "	*" + System.lineSeparator() + "FROM"
						+ System.lineSeparator() + "	PRODUCT" + System.lineSeparator() + "WHERE 1 = 1"
						+ System.lineSeparator() + "/*IF product_id != null */" + System.lineSeparator()
						+ "AND	PRODUCT_ID	IN	/*product_id*/(0, 2)" + System.lineSeparator() + "/*END*/"
						+ System.lineSeparator() + "ORDER BY PRODUCT_ID" + System.lineSeparator()));
	}

	@Test
	public void testLoadFileNull() throws Exception {
		assertThrows(IllegalArgumentException.class, () -> new SqlLoaderImpl().load(null));
	}

	@Test
	public void testNotExistsFile() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		try {
			sqlLoader.load("notexists/SQL_FILE");
			fail("No exception occurred");
		} catch (UroborosqlRuntimeException e) {
			assertThat(e.getMessage(), is(containsString("not found")));
			assertThat(e.getMessage(), is(containsString("sql/notexists/SQL_FILE.sql")));
		}
	}

	@Test
	public void testExistSql() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		assertThat(sqlLoader.existSql("example/select_test"), is(true));
		assertThat(sqlLoader.existSql("example/no_sql"), is(false));
	}

	@Test
	public void testGetLoadPath() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		assertThat(sqlLoader.getLoadPath(), is("sql"));
	}

	@Test
	public void testSetLoadPath() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		sqlLoader.setLoadPath(null);
		assertThat(sqlLoader.getLoadPath(), is("sql"));
		sqlLoader.setLoadPath("custom_sql");
		assertThat(sqlLoader.getLoadPath(), is("custom_sql"));
	}

	@Test
	public void testGetFileExtension() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		assertThat(sqlLoader.getFileExtension(), is(".sql"));
	}

	@Test
	public void testSetFileExtension() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		sqlLoader.setFileExtension(null);
		assertThat(sqlLoader.getFileExtension(), is(".sql"));
		sqlLoader.setFileExtension(".sqlx");
		assertThat(sqlLoader.getFileExtension(), is(".sqlx"));
	}

	@Test
	public void testGetSqlEncoding() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		assertThat(sqlLoader.getSqlEncoding(), is(System.getProperty("file.encoding")));
	}

	@Test
	public void testSetSqlEncoding() {
		SqlLoader sqlLoader = new SqlLoaderImpl("sql", ".sql");
		sqlLoader.setSqlEncoding("UTF-8");
		assertThat(sqlLoader.getSqlEncoding(), is("UTF-8"));
	}

}
