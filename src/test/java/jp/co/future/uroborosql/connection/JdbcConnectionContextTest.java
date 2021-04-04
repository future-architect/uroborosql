package jp.co.future.uroborosql.connection;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.Connection;

import org.junit.jupiter.api.Test;

public class JdbcConnectionContextTest {
	private static final String URL = "jdbc:h2:mem:" + JdbcConnectionContextTest.class.getSimpleName();
	private static final String USER = "sa";
	private static final String PASSWORD = "";
	private static final String SCHEMA = "PUBLIC";

	@Test
	public void testJdbcConnectionContext() {
		var ctx = ConnectionContextBuilder.jdbc(URL);
		assertThat(ctx.url(), is(URL));
		assertThat(ctx.user(), is(nullValue()));
		assertThat(ctx.password(), is(nullValue()));
		assertThat(ctx.schema(), is(nullValue()));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testJdbcConnectionContextWithUserPass() {
		var ctx = ConnectionContextBuilder.jdbc(URL, USER, PASSWORD);
		assertThat(ctx.url(), is(URL));
		assertThat(ctx.user(), is(USER));
		assertThat(ctx.password(), is(PASSWORD));
		assertThat(ctx.schema(), is(nullValue()));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testJdbcConnectionContextWithUserPassSchema() {
		var ctx = ConnectionContextBuilder.jdbc(URL, USER, PASSWORD, SCHEMA);
		assertThat(ctx.url(), is(URL));
		assertThat(ctx.user(), is(USER));
		assertThat(ctx.password(), is(PASSWORD));
		assertThat(ctx.schema(), is(SCHEMA));
		assertThat(ctx.autoCommit(), is(false));
		assertThat(ctx.readOnly(), is(false));
		assertThat(ctx.transactionIsolation(), is(-1));
	}

	@Test
	public void testJdbcConnectionContextWithUrlNull() {
		assertThrows(IllegalArgumentException.class, () -> ConnectionContextBuilder.jdbc(null));
	}

	@Test
	public void testSetter() {
		var url2 = URL + "_2";
		var ctx = ConnectionContextBuilder.jdbc(URL, USER, PASSWORD, SCHEMA);
		assertThat(ctx.url(), is(URL));
		assertThat(ctx.url(url2).url(), is(url2));

		assertThat(ctx.user(null).user(), is(USER));
		assertThat(ctx.user("user").user(), is("user"));

		assertThat(ctx.password(null).password(), is(PASSWORD));
		assertThat(ctx.password("password").password(), is("password"));

		assertThat(ctx.schema(null).schema(), is(SCHEMA));
		assertThat(ctx.schema("schema").schema(), is("schema"));

		assertThat(ctx.autoCommit(true).autoCommit(), is(true));
		assertThat(ctx.readOnly(true).readOnly(), is(true));
		assertThat(ctx.transactionIsolation(Connection.TRANSACTION_REPEATABLE_READ).transactionIsolation(),
				is(Connection.TRANSACTION_REPEATABLE_READ));
	}

	@Test
	public void testSetUrlNull() {
		assertThrows(IllegalArgumentException.class, () -> ConnectionContextBuilder.jdbc(URL).url(null));
	}

	@Test
	public void testToProperties() {
		var ctx = ConnectionContextBuilder.jdbc(URL);
		var props = ctx.toProperties();
		assertThat(props.isEmpty(), is(true));

		ctx = ConnectionContextBuilder.jdbc(URL, USER, PASSWORD, SCHEMA);
		props = ctx.toProperties();
		assertThat(props.size(), is(2));
		assertThat(props.get("user"), is(USER));
		assertThat(props.get("password"), is(PASSWORD));

		ctx = ConnectionContextBuilder.jdbc(URL, USER, PASSWORD, SCHEMA)
				.autoCommit(true)
				.readOnly(true)
				.transactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
		props = ctx.toProperties();
		assertThat(props.size(), is(2));
		assertThat(props.get("user"), is(USER));
		assertThat(props.get("password"), is(PASSWORD));

		ctx = ConnectionContextBuilder.jdbc(URL, USER, PASSWORD, SCHEMA)
				.autoCommit(true)
				.readOnly(true)
				.transactionIsolation(Connection.TRANSACTION_READ_COMMITTED)
				.set("MODE", "DB2");
		props = ctx.toProperties();
		assertThat(props.size(), is(3));
		assertThat(props.get("user"), is(USER));
		assertThat(props.get("password"), is(PASSWORD));
		assertThat(props.get("MODE"), is("DB2"));
	}
}
