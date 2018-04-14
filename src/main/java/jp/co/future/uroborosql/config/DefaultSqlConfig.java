/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql.config;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.SqlAgentFactory;
import jp.co.future.uroborosql.SqlAgentFactoryImpl;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.connection.DataSourceConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.JdbcConnectionSupplierImpl;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.DefaultEntityHandler;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.store.SqlManagerImpl;

import javax.sql.DataSource;
import java.sql.Connection;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

/**
 * SQLを発行するための設定を管理するクラスのデフォルト実装
 *
 * @deprecated Instead, use the {@link UroboroSQL}.
 * @author H.Sugimoto
 *
 */
@Deprecated
public class DefaultSqlConfig implements SqlConfig {
	private final SqlManager sqlManager;
	private final SqlFilterManager sqlFilterManager;
	private final ConnectionSupplier connectionSupplier;
	private final SqlContextFactory sqlContextFactory;
	private final SqlAgentFactory sqlAgentFactory;
	private final Dialect dialect;
	private EntityHandler<?> entityHandler;

	/**
	 * コンストラクタ
	 *
	 * @param connectionSupplier コネクションサプライヤ
	 * @param loadPath SQLファイルの読み込みルートパス
	 */
	private DefaultSqlConfig(final ConnectionSupplier connectionSupplier, final String loadPath) {
		super();
		this.connectionSupplier = connectionSupplier;

		this.sqlManager = new SqlManagerImpl(loadPath);
		this.sqlFilterManager = new SqlFilterManagerImpl();
		this.sqlContextFactory = new SqlContextFactoryImpl();
		this.entityHandler = new DefaultEntityHandler();

		this.dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false).filter(d -> d.accept(connectionSupplier)).findFirst().orElseGet(DefaultDialect::new);

		this.sqlAgentFactory = new SqlAgentFactoryImpl(this);

		initialize();
	}

	private void initialize() {
		this.sqlManager.initialize();
		this.sqlContextFactory.setSqlFilterManager(sqlFilterManager);
		this.sqlContextFactory.initialize();

	}

	/**
	 * DBコネクションを指定してSqlConfigを取得する
	 *
	 * @param conn DBコネクション
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final Connection conn) {
		return new DefaultSqlConfig(new DefaultConnectionSupplierImpl(conn), null);
	}

	/**
	 * DBコネクションとSQL読み込みルートパスを指定してSqlConfigを取得する
	 *
	 * @param conn DBコネクション
	 * @param loadPath SQLファイルの読み込みルートパス
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final Connection conn, final String loadPath) {
		return new DefaultSqlConfig(new DefaultConnectionSupplierImpl(conn), loadPath);
	}

	/**
	 * DB接続情報を指定してSqlConfigを取得する
	 *
	 * @param url JDBC接続URL
	 * @param user JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password) {
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
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password, final String schema) {
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
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password, final String schema,
			final String loadPath) {
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
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password, final String schema,
			final boolean autoCommit, final boolean readOnly) {
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
	 */
	public static SqlConfig getConfig(final String url, final String user, final String password, final String schema,
			final boolean autoCommit, final boolean readOnly, final String loadPath) {
		return new DefaultSqlConfig(new JdbcConnectionSupplierImpl(url, user, password, schema, autoCommit, readOnly),
				loadPath);
	}

	/**
	 * データソースを指定してSqlConfigを取得する
	 *
	 * @param dataSource データソース
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final DataSource dataSource) {
		DataSourceConnectionSupplierImpl connectionSupplier = new DataSourceConnectionSupplierImpl(dataSource);
		return new DefaultSqlConfig(connectionSupplier, null);
	}

	/**
	 * データソースを指定してSqlConfigを取得する
	 *
	 * @param dataSource データソース
	 * @param autoCommit 自動コミットの指定
	 * @param readOnly 読み取り専用モードの指定
	 * @param transactionIsolation トランザクション隔離レベルの指定
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final DataSource dataSource, final boolean autoCommit, final boolean readOnly,
			final int transactionIsolation) {
		DataSourceConnectionSupplierImpl connectionSupplier = new DataSourceConnectionSupplierImpl(dataSource);
		connectionSupplier.setDefaultAutoCommit(autoCommit);
		connectionSupplier.setDefaultReadOnly(readOnly);
		connectionSupplier.setDefaultTransactionIsolation(transactionIsolation);
		return new DefaultSqlConfig(connectionSupplier, null);
	}

	/**
	 * データソース名を指定してSqlConfigを取得する
	 *
	 * @param dataSourceName データソース名
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final String dataSourceName) {
		DataSourceConnectionSupplierImpl connectionSupplier = new DataSourceConnectionSupplierImpl(dataSourceName);
		return new DefaultSqlConfig(connectionSupplier, null);
	}

	/**
	 * データソース名を指定してSqlConfigを取得する
	 *
	 * @param dataSourceName データソース名
	 * @param autoCommit 自動コミットの指定
	 * @param readOnly 読み取り専用モードの指定
	 * @param transactionIsolation トランザクション隔離レベルの指定
	 * @return SqlConfigオブジェクト
	 */
	public static SqlConfig getConfig(final String dataSourceName, final boolean autoCommit, final boolean readOnly,
			final int transactionIsolation) {
		DataSourceConnectionSupplierImpl connectionSupplier = new DataSourceConnectionSupplierImpl(dataSourceName);
		connectionSupplier.setDefaultAutoCommit(autoCommit);
		connectionSupplier.setDefaultReadOnly(readOnly);
		connectionSupplier.setDefaultTransactionIsolation(transactionIsolation);
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
	@Deprecated
	public SqlAgent createAgent() {
		return this.agent();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#agent()
	 */
	@Override
	public SqlAgent agent() {
		return sqlAgentFactory.createSqlAgent();
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

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getDialect()
	 */
	@Override
	public Dialect getDialect() {
		return dialect;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#getEntityHandler()
	 */
	@Override
	public EntityHandler<?> getEntityHandler() {
		return entityHandler;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.config.SqlConfig#setEntityHandler(EntityHandler)
	 */
	@Override
	public void setEntityHandler(final EntityHandler<?> entityHandler) {
		this.entityHandler = entityHandler;
	}

}
