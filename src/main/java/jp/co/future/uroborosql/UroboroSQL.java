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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.connection.ConnectionContext;
import jp.co.future.uroborosql.connection.ConnectionContextBuilder;
import jp.co.future.uroborosql.connection.ConnectionSupplier;
import jp.co.future.uroborosql.connection.DataSourceConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.DefaultConnectionSupplierImpl;
import jp.co.future.uroborosql.connection.JdbcConnectionSupplierImpl;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.context.SqlContextFactory;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.dialect.DefaultDialect;
import jp.co.future.uroborosql.dialect.Dialect;
import jp.co.future.uroborosql.expr.ExpressionParser;
import jp.co.future.uroborosql.expr.ExpressionParserFactory;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.DefaultEntityHandler;
import jp.co.future.uroborosql.mapping.EntityHandler;
import jp.co.future.uroborosql.store.SqlManager;
import jp.co.future.uroborosql.store.SqlManagerImpl;

/**
 * UroboroSQLを利用する際、初めに利用するクラス.
 *
 * @author H.Sugimoto
 * @since v0.4.0
 */
public final class UroboroSQL {
	/** ロガー */
	private static final Logger log = LoggerFactory.getLogger(UroboroSQL.class);

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
		private SqlManager sqlManager;
		private SqlFilterManager sqlFilterManager;
		private SqlContextFactory sqlContextFactory;
		private SqlAgentFactory sqlAgentFactory;
		private EntityHandler<?> entityHandler;
		private Clock clock;
		private Dialect dialect;
		private ExpressionParser expressionParser;

		UroboroSQLBuilder() {
			this.connectionSupplier = null;
			this.sqlManager = new SqlManagerImpl();
			this.sqlFilterManager = new SqlFilterManagerImpl();
			this.sqlContextFactory = new SqlContextFactoryImpl();
			this.sqlAgentFactory = new SqlAgentFactoryImpl();
			this.entityHandler = new DefaultEntityHandler();
			this.clock = null;
			this.dialect = null;
			this.expressionParser = null;
		}

		/**
		 * SqlManagerの設定.
		 *
		 * @param sqlManager sqlManager
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setSqlManager(final SqlManager sqlManager) {
			this.sqlManager = sqlManager;
			return this;
		}

		/**
		 * SqlFilterManagerの設定.
		 *
		 * @param sqlFilterManager sqlFilterManager
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setSqlFilterManager(final SqlFilterManager sqlFilterManager) {
			this.sqlFilterManager = sqlFilterManager;
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
		 * SqlContextFactoryの作成.
		 *
		 * @param sqlContextFactory sqlContextFactory
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setSqlContextFactory(final SqlContextFactory sqlContextFactory) {
			this.sqlContextFactory = sqlContextFactory;
			return this;
		}

		/**
		 * SqlAgentFactoryの設定.
		 *
		 * @param sqlAgentFactory sqlAgentFactory
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setSqlAgentFactory(final SqlAgentFactory sqlAgentFactory) {
			this.sqlAgentFactory = sqlAgentFactory;
			return this;
		}

		/**
		 * EntityHandlerの設定.
		 *
		 * @param entityHandler entityHandler
		 * @return UroboroSQLBuilder
		 */
		public UroboroSQLBuilder setEntityHandler(final EntityHandler<?> entityHandler) {
			this.entityHandler = entityHandler;
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
					this.sqlManager,
					this.sqlContextFactory,
					this.sqlAgentFactory,
					this.sqlFilterManager,
					this.entityHandler,
					this.clock,
					this.dialect,
					this.expressionParser);
		}

	}

	public static final class InternalConfig implements SqlConfig {
		/**
		 * コネクション提供クラス.
		 */
		private final ConnectionSupplier connectionSupplier;

		/**
		 * SQL管理クラス.
		 */
		private final SqlManager sqlManager;

		/**
		 * SqlContextファクトリクラス.
		 */
		private final SqlContextFactory sqlContextFactory;

		/**
		 * SqlAgentファクトリクラス.
		 */
		private final SqlAgentFactory sqlAgentFactory;

		/**
		 * SqlFilter管理クラス.
		 */
		private final SqlFilterManager sqlFilterManager;

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
				final SqlManager sqlManager,
				final SqlContextFactory sqlContextFactory,
				final SqlAgentFactory sqlAgentFactory,
				final SqlFilterManager sqlFilterManager,
				final EntityHandler<?> entityHandler,
				final Clock clock,
				final Dialect dialect,
				final ExpressionParser expressionParser) {
			this.connectionSupplier = connectionSupplier;
			this.sqlManager = sqlManager;
			this.sqlContextFactory = sqlContextFactory;
			this.sqlAgentFactory = sqlAgentFactory;
			this.sqlFilterManager = sqlFilterManager;
			this.entityHandler = entityHandler;
			if (clock == null) {
				this.clock = Clock.systemDefaultZone();
			} else {
				this.clock = clock;
			}
			log.debug("SqlConfig - Clock : " + this.clock.toString() + " has been selected.");

			if (dialect == null) {
				this.dialect = StreamSupport.stream(ServiceLoader.load(Dialect.class).spliterator(), false)
						.filter(d -> d.accept(this.connectionSupplier)).findFirst().orElseGet(DefaultDialect::new);
			} else {
				this.dialect = dialect;
			}
			log.debug("SqlConfig - Dialect : " + this.dialect.getClass().getSimpleName() + " has been selected.");

			if (expressionParser == null) {
				ExpressionParserFactory expressionParserFactory = StreamSupport
						.stream(ServiceLoader.load(ExpressionParserFactory.class).spliterator(), false)
						.filter(e -> e.accept()).findFirst()
						.orElseThrow(() -> new IllegalStateException("ExpressionParser not found."));
				this.expressionParser = expressionParserFactory.create();
			} else {
				this.expressionParser = expressionParser;
			}
			log.debug("SqlConfig - ExpressionParser : " + this.expressionParser.getClass().getSimpleName()
					+ " has been selected.");

			this.sqlManager.setDialect(this.dialect);
			this.sqlContextFactory.setSqlConfig(this);
			this.sqlAgentFactory.setSqlConfig(this);
			this.expressionParser.setSqlConfig(this);
			this.entityHandler.setSqlConfig(this);

			this.sqlManager.initialize();
			this.sqlFilterManager.initialize();
			this.sqlContextFactory.initialize();
			this.expressionParser.initialize();
			this.entityHandler.initialize();
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
		 * @see jp.co.future.uroborosql.config.SqlConfig#createAgent()
		 */
		@Override
		public SqlAgent agent() {
			return sqlAgentFactory.agent();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.config.SqlConfig#agent(jp.co.future.uroborosql.connection.ConnectionContext)
		 */
		@Override
		public SqlAgent agent(final ConnectionContext connectionContext) {
			return sqlAgentFactory.agent(connectionContext);
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
