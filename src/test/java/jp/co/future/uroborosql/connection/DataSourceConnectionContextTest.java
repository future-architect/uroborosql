package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.sql.Connection;

import org.junit.jupiter.api.Test;

public class DataSourceConnectionContextTest {

	@Test
	void testDataSourceConnectionContext() {
		var ctx = ConnectionContextBuilder.dataSource();
		assertThat(ctx.dataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	void testDataSourceConnectionContextWithDataSourceName() {
		var ctx = ConnectionContextBuilder.dataSource("dataSourceName");
		assertThat(ctx.dataSourceName(), is("dataSourceName"));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	void testDataSourceConnectionContextWithDataSourceNameNull() {
		assertThrows(IllegalArgumentException.class, () -> {
			ConnectionContextBuilder.dataSource(null);
		});
	}

	@Test
	void testSetter() {
		var ctx = ConnectionContextBuilder.dataSource();
		assertThat(ctx.dataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		assertThat(ctx.dataSourceName("dataSourceName").dataSourceName(), is("dataSourceName"));
		assertThat(ctx.autoCommit(true).autoCommit(), is(true));
		assertThat(ctx.readOnly(true).readOnly(), is(true));
		assertThat(ctx.transactionIsolation(Connection.TRANSACTION_REPEATABLE_READ).transactionIsolation(),
				is(Connection.TRANSACTION_REPEATABLE_READ));
	}

	@Test
	void testSetDataSourceNameNull() {
		assertThrows(IllegalArgumentException.class, () -> {
			ConnectionContextBuilder.dataSource().dataSourceName(null);
		});
	}
}
