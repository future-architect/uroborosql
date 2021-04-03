package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.Connection;

import org.junit.jupiter.api.Test;

public class DataSourceConnectionContextTest {

	@Test
	public void testDataSourceConnectionContext() {
		DataSourceConnectionContext ctx = ConnectionContextBuilder.dataSource();
		assertThat(ctx.dataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testDataSourceConnectionContextWithDataSourceName() {
		DataSourceConnectionContext ctx = ConnectionContextBuilder.dataSource("dataSourceName");
		assertThat(ctx.dataSourceName(), is("dataSourceName"));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testDataSourceConnectionContextWithDataSourceNameNull() {
		assertThrows(IllegalArgumentException.class, () -> ConnectionContextBuilder.dataSource(null));
	}

	@Test
	public void testSetter() {
		DataSourceConnectionContext ctx = ConnectionContextBuilder.dataSource();
		assertThat(ctx.dataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		assertThat(ctx.dataSourceName("dataSourceName").dataSourceName(), is("dataSourceName"));
		assertThat(ctx.autoCommit(true).autoCommit(), is(true));
		assertThat(ctx.readOnly(true).readOnly(), is(true));
		assertThat(ctx.transactionIsolation(Connection.TRANSACTION_REPEATABLE_READ).transactionIsolation(),
				is(Connection.TRANSACTION_REPEATABLE_READ));
	}

	@Test
	public void testSetDataSourceNameNull() {
		assertThrows(IllegalArgumentException.class, () -> ConnectionContextBuilder.dataSource().dataSourceName(null));
	}
}
