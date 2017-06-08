package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

import org.junit.Test;

public class SqlLoaderImplTest {

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

}
