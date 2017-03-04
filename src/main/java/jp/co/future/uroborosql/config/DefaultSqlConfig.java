package jp.co.future.uroborosql.config;

import java.io.IOException;
import java.net.URISyntaxException;
import java.sql.Connection;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.SqlAgentFactory;
import jp.co.future.uroborosql.SqlAgentFactoryImpl;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.connection.DataSourceConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.JdbcConnectionSupplierImpl;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.store.SqlManagerImpl;

/**
 * SQLを発行するための設定を管理するクラスのデフォルト実装
 *
 * @author H.Sugimoto
 *
 */
public class DefaultSqlConfig implements SqlConfig {
	private final SqlManager sqlManager;
	private final SqlFilterManager sqlFilterManager;
	private final ConnectionSupplier connectionSupplier;
	private final SqlContextFactory sqlContextFactory;
	private final SqlAgentFactory sqlAgentFactory;

	/**
	 * コンストラクタ
	 *
	 * @param connectionSupplier コネクションサプライヤ
	 * @param loadPath SQLファイルの読み込みルートパス
	 * @throws IOException ファイルが読み込めなかった場合
	 * @throws URISyntaxException リソースアクセスできなかった場合
	 */
	private DefaultSqlConfig(final ConnectionSupplier connectionSupplier, final String loadPath) throws IOException,
			URISyntaxException {
		super();
		this.connectionSupplier = connectionSupplier;

		sqlManager = new SqlManagerImpl(loadPath);
		sqlFilterManager = new SqlFilterManagerImpl();
		sqlContextFactory = new SqlContextFactoryImpl();
		sqlAgentFactory = new SqlAgentFactoryImpl(this.connectionSupplier, sqlManager, sqlFilterManager);

		initialize();

	}

	private void initialize() throws IOException, URISyntaxException {
		sqlManager.initialize();
		sqlContextFactory.setSqlFilterManager(sqlFilterManager);
		sqlContextFactory.initialize();
	}

	/**
	 * DBコネクションを指定してSqlConfigを取得する
	 *
	 * @param conn DBコネクション
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final Connection conn)
			throws Exception {
		return new DefaultSqlConfig(new DefaultConnectionSupplierImpl(conn), null);
	}

	/**
	 * DBコネクションとSQL読み込みルートパスを指定してSqlConfigを取得する
	 *
	 * @param conn DBコネクション
	 * @param loadPath SQLファイルの読み込みルートパス
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final Connection conn, final String loadPath)
			throws Exception {
		return new DefaultSqlConfig(new DefaultConnectionSupplierImpl(conn), loadPath);
	}

	/**
	 * DB接続情報を指定してSqlConfigを取得する
	 *
	 * @param url JDBC接続URL
	 * @param user JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password)
			throws Exception {
		return getConfig(url, user, password, null, false, false, null);
	}

	/**
	 * DB接続情報を指定してSqlConfigを取得する
	 *
	 * @param url JDBC接続URL
	 * @param user JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @param schema JDBCスキーマ名
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password,
			final String schema) throws Exception {
		return getConfig(url, user, password, schema, false, false, null);
	}

	/**
	 * DB接続情報とSQL読み込みルートパスを指定してSqlConfigを取得する
	 *
	 * @param url JDBC接続URL
	 * @param user JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @param schema JDBCスキーマ名
	 * @param loadPath SQLファイルの読み込みルートパス
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password,
			final String schema, final String loadPath) throws Exception {
		return getConfig(url, user, password, schema, false, false, loadPath);
	}

	/**
	 * DB接続情報を指定してSqlConfigを取得する
	 *
	 * @param url JDBC接続URL
	 * @param user JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @param schema JDBCスキーマ名
	 * @param autoCommit 自動コミットするかどうか. 自動コミットの場合<code>true</code>
	 * @param readOnly 参照のみかどうか. 参照のみの場合<code>true</code>
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password,
			final String schema,
			final boolean autoCommit, final boolean readOnly) throws Exception {
		return new DefaultSqlConfig(new JdbcConnectionSupplierImpl(url, user, password, schema, autoCommit, readOnly),
				null);
	}

	/**
	 * DB接続情報とSQL読み込みルートパスを指定してSqlConfigを取得する
	 *
	 * @param url JDBC接続URL
	 * @param user JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @param schema JDBCスキーマ名
	 * @param autoCommit 自動コミットするかどうか. 自動コミットの場合<code>true</code>
	 * @param readOnly 参照のみかどうか. 参照のみの場合<code>true</code>
	 * @param loadPath SQLファイルの読み込みルートパス
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password,
			final String schema,
			final boolean autoCommit, final boolean readOnly, final String loadPath) throws Exception {
		return new DefaultSqlConfig(new JdbcConnectionSupplierImpl(url, user, password, schema, autoCommit, readOnly),
				loadPath);
	}

	/**
	 * データソースを指定してSqlConfigを取得する
	 *
	 * @param dataSourceName データソース名
	 * @return SqlConfigオブジェクト
	 * @throws Exception
	 */
	public static SqlConfig getConfig(final String dataSourceName) throws Exception {
		DataSourceConnectionSupplierImpl connectionSupplier = new DataSourceConnectionSupplierImpl(dataSourceName);
		return new DefaultSqlConfig(connectionSupplier, null);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#context()
	 */
	@Override
	public SqlContext context() {
		return sqlContextFactory.createSqlContext();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#contextFrom(java.lang.String)
	 */
	@Override
	public SqlContext contextFrom(final String sqlName) {
		return sqlContextFactory.createSqlContext().setSqlName(sqlName);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#contextWith(java.lang.String)
	 */
	@Override
	public SqlContext contextWith(final String sql) {
		return sqlContextFactory.createSqlContext().setSql(sql);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#createAgent()
	 */
	@Override
	public SqlAgent createAgent() {
		SqlAgent agent = sqlAgentFactory.createSqlAgent();
		agent.setSqlConfig(this);
		return agent;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getSqlManager()
	 */
	@Override
	public SqlManager getSqlManager() {
		return sqlManager;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getSqlFilterManager()
	 */
	@Override
	public SqlFilterManager getSqlFilterManager() {
		return sqlFilterManager;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getConnectionSupplier()
	 */
	@Override
	public ConnectionSupplier getConnectionSupplier() {
		return connectionSupplier;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getSqlContextFactory()
	 */
	@Override
	public SqlContextFactory getSqlContextFactory() {
		return sqlContextFactory;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getSqlAgentFactory()
	 */
	@Override
	public SqlAgentFactory getSqlAgentFactory() {
		return sqlAgentFactory;
	}

}
