package jp.co.future.uroborosql.store;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import jp.co.future.uroborosql.dialect.H2Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;

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

		for (int i = 1; i <= 10; i++) {
			Thread.sleep(100);

			Path newFilePath = Paths.get("src/test/resources/sql/test", "ADD_WATCH" + i + ".sql");
			Files.deleteIfExists(newFilePath);

			Thread.sleep(100);

			Files.write(newFilePath, Arrays.asList("select * from ADD_WATCH" + i));

			Thread.sleep(100);
			Files.deleteIfExists(newFilePath);
		}
	}
}
