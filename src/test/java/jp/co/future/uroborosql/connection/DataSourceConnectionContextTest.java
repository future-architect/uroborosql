package jp.co.future.uroborosql.connection;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.sql.Connection;

import org.junit.Test;

public class DataSourceConnectionContextTest {

	@Test
	public void testDataSourceConnectionContext() {
		DataSourceConnectionContext ctx = ConnectionContextBuilder.dataSource();
		assertThat(ctx.dataSourceName(), is(DataSourceConnectionContext.DEFAULT_DATASOURCE_NAME));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.cacheSchema(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testDataSourceConnectionContextWithDataSourceName() {
		DataSourceConnectionContext ctx = ConnectionContextBuilder.dataSource("dataSourceName");
		assertThat(ctx.dataSourceName(), is("dataSourceName"));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.cacheSchema(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testDataSourceConnectionContextWithCacheSchema() {
		DataSourceConnectionContext ctx = ConnectionContextBuilder.dataSource()
				.cacheSchema(true);
		assertThat(ctx.cacheSchema(), is(true));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testDataSourceConnectionContextWithDataSourceNameNull() {
		ConnectionContextBuilder.dataSource(null);
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

	@Test(expected = IllegalArgumentException.class)
	public void testSetDataSourceNameNull() {
		ConnectionContextBuilder.dataSource().dataSourceName(null);
	}
}
