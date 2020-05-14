package jp.co.future.uroborosql.connection;

public final class ConnectionContextBuilder {
	private ConnectionContextBuilder() {
	}

	public static DataSourceConnectionContext dataSource() {
		return new DataSourceConnectionContext();
	}

	public static DataSourceConnectionContext dataSource(final String dataSourceName) {
		return new DataSourceConnectionContext(dataSourceName);
	}

	public static JdbcConnectionContext jdbc(final String url) {
		return new JdbcConnectionContext(url);
	}

	public static JdbcConnectionContext jdbc(final String url, final String user, final String password) {
		return new JdbcConnectionContext(url, user, password);
	}

	public static JdbcConnectionContext jdbc(final String url, final String user, final String password,
			final String schema) {
		return new JdbcConnectionContext(url, user, password, schema);
	}
}
