/**
 * Copyright (c) 2017-present, Future Corporation
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package jp.co.future.uroborosql;

import java.sql.Connection;
import java.time.Clock;
import java.util.ServiceLoader;
import java.util.stream.StreamSupport;

import javax.sql.DataSource;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.connection.ConnectionContextBuilder;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.connection.DataSourceConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.JdbcConnectionSupplierImpl;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.context.ExecutionContextProvider;
import jp.co.future.uroborosql.context.ExecutionContextProviderImpl;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.event.EventListenerHolder;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.expr.ExpressionParserFactory;
import jp.co.future.uroborosql.log.ServiceLogger;
import jp.co.future.uroborosql.log.SettingLogger;
import jp.co.future.uroborosql.mapping.DefaultEntityHandler;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlResourceManager;
import jp.co.future.uroborosql.store.SqlResourceManagerImpl;

/**
 * UroboroSQLを利用する際、初めに利用するクラス.
 *
 * @author H.Sugimoto
 * @since v0.4.0
 */
public final class UroboroSQL implements ServiceLogger, SettingLogger {
	private UroboroSQL() {
	}

	/**
	 * Builderの生成
	 *
	 * @return UroboroSQLBuilder
	 */
	public static UroboroSQLBuilder builder() {
		return new UroboroSQLBuilder();
	}

	/**
	 * DBコネクションを指定してSqlConfigを取得する
	 *
	 * @param conn DBコネクション
	 * @return UroboroSQLBuilder
	 */
	public static UroboroSQLBuilder builder(final Connection conn) {
		return builder().setConnectionSupplier(new DefaultConnectionSupplierImpl(conn));
	}

	/**
	 * DB接続情報を指定してSqlConfigを取得する
	 *
	 * @param url      JDBC接続URL
	 * @param user     JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @return UroboroSQLBuilder
	 */
	public static UroboroSQLBuilder builder(final String url, final String user, final String password) {
		return builder().setConnectionSupplier(
				new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password)));
	}

	/**
	 * DB接続情報を指定してSqlConfigを取得する
	 *
	 * @param url      JDBC接続URL
	 * @param user     JDBC接続ユーザ
	 * @param password JDBC接続パスワード
	 * @param schema   JDBCスキーマ名
	 * @return UroboroSQLBuilder
	 */
	public static UroboroSQLBuilder builder(final String url, final String user, final String password,
			final String schema) {
		return builder().setConnectionSupplier(
				new JdbcConnectionSupplierImpl(ConnectionContextBuilder.jdbc(url, user, password, schema)));
	}

	/**
	 * データソースを指定してSqlConfigを取得する
	 *
	 * @param dataSource データソース
	 * @return UroboroSQLBuilder
	 */
	public static UroboroSQLBuilder builder(final DataSource dataSource) {
		return builder().setConnectionSupplier(new DataSourceConnectionSupplierImpl(dataSource));
	}

	public static final class UroboroSQLBuilder {
		private ConnectionSupplier connectionSupplier;
		private SqlResourceManager sqlResourceManager;
		private EventListenerHolder eventListenerHolder;
		private ExecutionContextProvider executionContextProvider;
		private SqlAgentProvider sqlAgentProvider;
		private Clock clock;
		private Dialect dialect;
		private ExpressionParser expressionParser;

		UroboroSQLBuilder() {
			this.connectionSupplier = null;
			this.sqlResourceManager = new SqlResourceManagerImpl();
			this.eventListenerHolder = new EventListenerHolder();
			this.executionContextProvider = new ExecutionContextProviderImpl();
			this.sqlAgentProvider = new SqlAgentProviderImpl();
			this.clock = null;
			this.dialect = null;
			this.expressionParser = null;
		}

		/**
		 * SqlResourceManagerの設定.
		 *
		 * @param sqlResourceManager sqlResourceManager
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setSqlResourceManager(final SqlResourceManager sqlResourceManager) {
			this.sqlResourceManager = sqlResourceManager;
			return this;
		}

		/**
		 * EventListenerHolderの設定.
		 *
		 * @param eventListenerHolder eventListenerHolder
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setEventListenerHolder(final EventListenerHolder eventListenerHolder) {
			this.eventListenerHolder = eventListenerHolder;
			return this;
		}

		/**
		 * ConnectionSupplierの設定.
		 *
		 * @param connectionSupplier connectionSupplier
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setConnectionSupplier(final ConnectionSupplier connectionSupplier) {
			this.connectionSupplier = connectionSupplier;
			return this;
		}

		/**
		 * ExecutionContextProviderの作成.
		 *
		 * @param executionContextProvider executionContextProvider
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setExecutionContextProvider(final ExecutionContextProvider executionContextProvider) {
			this.executionContextProvider = executionContextProvider;
			return this;
		}

		/**
		 * SqlAgentProviderの設定.
		 *
		 * @param sqlAgentProvider sqlAgentProvider
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setSqlAgentProvider(final SqlAgentProvider sqlAgentProvider) {
			this.sqlAgentProvider = sqlAgentProvider;
			return this;
		}

		/**
		 * Clockの設定.
		 *
		 * @param clock clock
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setClock(final Clock clock) {
			this.clock = clock;
			return this;
		}

		/**
		 * Dialectの設定.
		 *
		 * @param dialect dialect
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setDialect(final Dialect dialect) {
			this.dialect = dialect;
			return this;
		}

		/**
		 * ExpressionParserの設定
		 *
		 * @param expressionParser ExpressionParser
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setExpressionParser(final ExpressionParser expressionParser) {
			this.expressionParser = expressionParser;
			return this;
		}

		/**
		 * Builderに設定された内容を元にSqlConfigを構築する
		 *
		 * @return SqlConfig
		 */
		public SqlConfig build() {
			if (this.connectionSupplier == null) {
				throw new IllegalStateException(
						"ConnectionSupplier is mandatory. Please set ConnectionSupplier instance before calling build() method.");
			}

			return new InternalConfig(this.connectionSupplier,
					this.sqlResourceManager,
					this.executionContextProvider,
					this.sqlAgentProvider,
					this.eventListenerHolder,
					this.clock,
					this.dialect,
					this.expressionParser);
		}

	}

	public static final class InternalConfig implements SqlConfig, SettingLogger {
		/**
		 * コネクション提供クラス.
		 */
		private final ConnectionSupplier connectionSupplier;

		/**
		 * SQLリソース管理クラス.
		 */
		private final SqlResourceManager sqlResourceManager;

		/**
		 * ExecutionContextProviderプロバイダクラス.
		 */
		private final ExecutionContextProvider executionContextProvider;

		/**
		 * SqlAgentファクトリクラス.
		 */
		private final SqlAgentProvider sqlAgentProvider;

		/**
		 * EventListenerHolderクラス.
		 */
		private final EventListenerHolder eventListenerHolder;

		/**
		 * Entityハンドラ.
		 */
		private final EntityHandler<?> entityHandler;

		/**
		 * Clock.
		 */
		private final Clock clock;

		/**
		 * Dialect.
		 */
		private final Dialect dialect;

		/**
		 * ExpressionParser
		 */
		private final ExpressionParser expressionParser;

		InternalConfig(final ConnectionSupplier connectionSupplier,
				final SqlResourceManager sqlResourceManager,
				final ExecutionContextProvider executionContextProvider,
				final SqlAgentProvider sqlAgentProvider,
				final EventListenerHolder eventListenerHolder,
				final Clock clock,
				final Dialect dialect,
				final ExpressionParser expressionParser) {
			this.connectionSupplier = connectionSupplier;
			this.sqlResourceManager = sqlResourceManager;
			this.executionContextProvider = executionContextProvider;
			this.sqlAgentProvider = sqlAgentProvider;
			this.eventListenerHolder = eventListenerHolder;
			this.entityHandler = new DefaultEntityHandler();
			if (clock == null) {
				this.clock = Clock.systemDefaultZone();
				atWarn(SETTING_LOG)
						.log("SqlConfig - Clock was not set. Set SystemClock.");
			} else {
				this.clock = clock;
			}
			atInfo(SETTING_LOG)
					.setMessage("SqlConfig - Clock : {} has been selected.")
					.addArgument(this.clock)
					.log();

			if (dialect == null) {
				this.dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
						.filter(d -> d.accept(this.connectionSupplier))
						.findFirst()
						.orElseGet(DefaultDialect::new);
			} else {
				this.dialect = dialect;
			}
			atInfo(SETTING_LOG)
					.setMessage("SqlConfig - Dialect : {} has been selected.")
					.addArgument(() -> this.dialect.getClass().getSimpleName())
					.log();

			if (expressionParser == null) {
				var expressionParserFactory = StreamSupport
						.stream(ServiceLoader.load(ExpressionParserFactory.class).spliterator(), false)
						.filter(ExpressionParserFactory::accept)
						.findFirst()
						.orElseThrow(() -> new IllegalStateException("ExpressionParser not found."));
				this.expressionParser = expressionParserFactory.create();
			} else {
				this.expressionParser = expressionParser;
			}
			atInfo(SETTING_LOG)
					.setMessage("SqlConfig - ExpressionParser : {} has been selected.")
					.addArgument(() -> this.expressionParser.getClass().getSimpleName())
					.log();

			this.sqlResourceManager.setDialect(this.dialect);
			this.executionContextProvider.setSqlConfig(this);
			this.sqlAgentProvider.setSqlConfig(this);
			this.expressionParser.setSqlConfig(this);
			this.entityHandler.setSqlConfig(this);
			this.eventListenerHolder.initialize();
			this.sqlResourceManager.initialize();
			this.executionContextProvider.initialize();
			this.expressionParser.initialize();
			this.entityHandler.initialize();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#context()
		 */
		@Override
		public ExecutionContext context() {
			return executionContextProvider.createExecutionContext();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#agent()
		 */
		@Override
		public SqlAgent agent() {
			return sqlAgentProvider.agent();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#agent(jp.co.future.uroborosql.connection.ConnectionContext)
		 */
		@Override
		public SqlAgent agent(final ConnectionContext connectionContext) {
			return sqlAgentProvider.agent(connectionContext);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#getSqlResourceManager()
		 */
		@Override
		public SqlResourceManager getSqlResourceManager() {
			return sqlResourceManager;
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#getEventListenerHolder()
		 */
		@Override
		public EventListenerHolder getEventListenerHolder() {
			return eventListenerHolder;
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
		 * @see jp.co.future.uroborosql.config.SqlConfig#getExecutionContextProvider()
		 */
		@Override
		public ExecutionContextProvider getExecutionContextProvider() {
			return executionContextProvider;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#getSqlAgentProvider()
		 */
		@Override
		public SqlAgentProvider getSqlAgentProvider() {
			return sqlAgentProvider;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#getClock()
		 */
		@Override
		public Clock getClock() {
			return clock;
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
		 * @see jp.co.future.uroborosql.config.SqlConfig#getExpressionParser()
		 */
		@Override
		public ExpressionParser getExpressionParser() {
			return expressionParser;
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
	}
}
