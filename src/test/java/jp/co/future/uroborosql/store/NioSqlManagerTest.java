package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import org.junit.Assert;
import org.junit.Test;

public class NioSqlManagerTest {
	@Test
	public void testGetSql() throws Exception {
		NioSqlManagerImpl manager = new NioSqlManagerImpl();
		manager.setDialect(new H2Dialect());
		manager.initialize();

		assertThat(manager.existSql("example/select_test"), is(true));
		assertThat(manager.existSql("example/select_test2"), is(true));
		assertThat(manager.existSql("example/select_test3"), is(true));

		assertThat(manager.getSql("example/select_test"), containsString("H2DB"));
		assertThat(manager.getSql("example/select_test"), containsString("file"));

		assertThat(manager.getSql("example/select_test2"), containsString("default"));
		assertThat(manager.getSql("example/select_test2"), containsString("zip"));

		assertThat(manager.getSql("example/select_test3"), containsString("H2DB"));
		assertThat(manager.getSql("example/select_test3"), containsString("zip"));

		manager.setDialect(new PostgresqlDialect());
		manager.initialize();

		assertThat(manager.existSql("example/select_test"), is(true));
		assertThat(manager.existSql("example/select_test2"), is(true));
		assertThat(manager.existSql("example/select_test3"), is(true));

		assertThat(manager.getSql("example/select_test"), containsString("postgresql"));
		assertThat(manager.getSql("example/select_test"), containsString("file"));

		assertThat(manager.getSql("example/select_test2"), containsString("default"));
		assertThat(manager.getSql("example/select_test2"), containsString("zip"));

		assertThat(manager.getSql("example/select_test3"), containsString("postgresql"));
		assertThat(manager.getSql("example/select_test3"), containsString("zip"));

		for(;;) {
			Thread.sleep(1000);
		}
	}
}
