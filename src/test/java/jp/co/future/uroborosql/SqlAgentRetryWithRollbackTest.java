package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.event.subscriber.EventSubscriber;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * エラーハンドリングのテスト
 *
 * @author H.Sugimoto
 */
public class SqlAgentRetryWithRollbackTest {
	private SqlConfig config;

	private SqlAgent agent;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1", "sa", "sa")
				.setDialect(new PostgresqlDialect())
				.build();
		config.getSqlAgentProvider().setSqlRetryCodeList(List.of("54", "60", "30006"));
		agent = config.agent();

		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (var sql : sqls) {
			if (ObjectUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
	}

	@AfterEach
	public void tearDown() throws Exception {
		agent.close();
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	void testQueryRetryNoWait() throws Exception {
		var retryCount = 3;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, 60));

		var query = agent.query("example/select_product")
				.param("product_id", 0, 1).retry(retryCount + 1);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	void testQueryRetryWait() throws Exception {
		var retryCount = 3;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, 60));

		var query = agent.query("example/select_product")
				.param("product_id", 0, 1)
				.retry(retryCount + 1, 10);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（リトライ回数上限）
	 */
	@Test
	void testQueryRetryOver() throws Exception {
		var retryCount = 3;
		var errorCode = 60;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, errorCode));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product")
					.param("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	void testQueryNoRetry() throws Exception {
		var retryCount = 3;
		var errorCode = 1;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, errorCode));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product")
					.param("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	void testUpdateRetryNoWait() throws Exception {
		var retryCount = 3;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, 60));

		var update = agent.update("example/insert_product_regist_work")
				.param("product_name", "test")
				.param("product_kana_name", "test_kana")
				.param("jan_code", "1234567890123")
				.param("product_description", "")
				.param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	void testUpdateRetryWait() throws Exception {
		var retryCount = 3;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, 60));

		var update = agent.update("example/insert_product_regist_work")
				.param("product_name", "test")
				.param("product_kana_name", "test_kana")
				.param("jan_code", "1234567890123")
				.param("product_description", "")
				.param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ（リトライ回数上限）
	 */
	@Test
	void testUpdateRetryOver() throws Exception {
		var retryCount = 3;
		var errorCode = 60;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, errorCode));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work")
					.param("product_name", "test")
					.param("product_kana_name", "test_kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "")
					.param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * 更新のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	void testUpdateNotRetry() throws Exception {
		var retryCount = 3;
		var errorCode = 1;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, errorCode));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work")
					.param("product_name", "test")
					.param("product_kana_name", "test_kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "")
					.param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	void testProcedureRetryNoWait() throws Exception {
		var retryCount = 3;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, 60));

		var proc = agent.proc("example/insert_product_regist_work")
				.param("product_name", "test")
				.param("product_kana_name", "test_kana")
				.param("jan_code", "1234567890123")
				.param("product_description", "")
				.param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	void testProcedureRetryWait() throws Exception {
		var retryCount = 3;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, 60));

		var proc = agent.proc("example/insert_product_regist_work")
				.param("product_name", "test")
				.param("product_kana_name", "test_kana")
				.param("jan_code", "1234567890123")
				.param("product_description", "")
				.param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ（リトライ回数上限）
	 */
	@Test
	void testProcedureRetryOver() throws Exception {
		var retryCount = 3;
		var errorCode = 60;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, errorCode));

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work")
					.param("product_name", "test")
					.param("product_kana_name", "test_kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "")
					.param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	void testProcedureNoRetry() throws Exception {
		var retryCount = 3;
		var errorCode = 1;
		config.getEventListenerHolder().addEventSubscriber(new RetryEventSubscriber(retryCount, errorCode));

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work")
					.param("product_name", "test")
					.param("product_kana_name", "test_kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "")
					.param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * リトライテスト用の例外をスローするイベントサブスクライバ
	 *
	 * @author H.Sugimoto
	 */
	private final class RetryEventSubscriber extends EventSubscriber {
		private int retryCount = 0;
		private int currentCount = 0;
		private int errorCode = -1;
		@SuppressWarnings("unused")
		private String sqlState = "";

		public RetryEventSubscriber(final int retryCount, final int errorCode) {
			this(retryCount, errorCode, "23000");
		}

		public RetryEventSubscriber(final int retryCount, final int errorCode, final String sqlState) {
			this.retryCount = retryCount;
			this.currentCount = 0;
			this.errorCode = errorCode;
			this.sqlState = sqlState;
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.subscriber.EventSubscriber#initialize()
		 */
		@Override
		public void initialize() {
			sqlQueryListener(evt -> {
				if (retryCount > currentCount++) {
					//				preparedStatement.getConnection().rollback();
					throw new SQLException("Test Retry Exception", "23000", errorCode);
				}
			});
			sqlUpdateListener(evt -> {
				if (retryCount > currentCount++) {
					//				preparedStatement.getConnection().rollback();
					throw new SQLException("Test Retry Exception", "23000", errorCode);
				}
			});
			sqlBatchListener(evt -> {
				if (retryCount > currentCount++) {
					//				preparedStatement.getConnection().rollback();
					throw new SQLException("Test Retry Exception", "23000", errorCode);
				}
			});
			procedureListener(evt -> {
				if (retryCount > currentCount++) {
					//				callableStatement.getConnection().rollback();
					throw new SQLException("Test Retry Exception", "23000", errorCode);
				}
			});
		}
	}

}
