package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.event.EventSubscriber;
import jp.co.future.uroborosql.event.ResultEvent.BatchResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.ProcedureResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.QueryResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.UpdateResultEvent;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * エラーハンドリングのテスト
 *
 * @author H.Sugimoto
 */
public class SqlAgentRetryWithRollbackTest {
	private SqlConfig config;

	private SqlAgent agent;

	private RetryEventSubscriber subscriber = new RetryEventSubscriber(0, 0);

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentRetryWithRollbackTest;DB_CLOSE_DELAY=-1", "sa", "sa")
				.setDialect(new PostgresqlDialect())
				.addSubscriber(subscriber)
				.build();
		config.getSqlAgentProvider().setSqlRetryCodeList(Arrays.asList("54", "60", "30006"));
		agent = config.agent();

		var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
	}

	@AfterEach
	public void tearDown() {
		agent.close();
	}

	private void setRetrySubscriber(final int retryCount, final int errorCode) {
		subscriber.initialize(retryCount, errorCode);
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryRetryNoWait() {
		var retryCount = 3;
		setRetrySubscriber(retryCount, 60);

		var query = agent.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount + 1);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	public void testQueryRetryWait() {
		var retryCount = 3;
		setRetrySubscriber(retryCount, 60);

		var query = agent.query("example/select_product").param("product_id", List.of(0, 1))
				.retry(retryCount + 1, 10);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（リトライ回数上限）
	 */
	@Test
	public void testQueryRetryOver() {
		var retryCount = 3;
		var errorCode = 60;
		setRetrySubscriber(retryCount, errorCode);

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testQueryNoRetry() {
		var retryCount = 3;
		var errorCode = 1;
		setRetrySubscriber(retryCount, errorCode);

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryNoWait() {
		var retryCount = 3;
		setRetrySubscriber(retryCount, 60);

		var update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryWait() {
		var retryCount = 3;
		setRetrySubscriber(retryCount, 60);

		var update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ（リトライ回数上限）
	 */
	@Test
	public void testUpdateRetryOver() {
		var retryCount = 3;
		var errorCode = 60;
		setRetrySubscriber(retryCount, errorCode);

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * 更新のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testUpdateNotRetry() {
		var retryCount = 3;
		var errorCode = 1;
		setRetrySubscriber(retryCount, errorCode);

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryNoWait() throws Exception {
		var retryCount = 3;
		setRetrySubscriber(retryCount, 60);

		var proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryWait() throws Exception {
		var retryCount = 3;
		setRetrySubscriber(retryCount, 60);

		var proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ（リトライ回数上限）
	 */
	@Test
	public void testProcedureRetryOver() {
		var retryCount = 3;
		var errorCode = 60;
		setRetrySubscriber(retryCount, errorCode);

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testProcedureNoRetry() {
		var retryCount = 3;
		var errorCode = 1;
		setRetrySubscriber(retryCount, errorCode);

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * リトライテスト用の例外をスローするイベントサブスクライバ.
	 *
	 * @author yanagihara
	 */
	private final class RetryEventSubscriber implements EventSubscriber {
		private int retryCount = 0;
		private int currentCount = 0;
		private int errorCode = -1;

		public RetryEventSubscriber(final int retryCount, final int errorCode) {
			initialize(retryCount, errorCode);
		}

		public void initialize(final int retryCount, final int errorCode) {
			this.retryCount = retryCount;
			this.currentCount = 0;
			this.errorCode = errorCode;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.EventSubscriber#doQuery(QueryResultEvent)
		 */
		@Override
		public ResultSet doQuery(final QueryResultEvent event) throws SQLException {
			if (retryCount > currentCount++) {
				throw new SQLException("Test Retry Exception", "23000", errorCode);
			}

			return event.getResultSet();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.EventSubscriber#doUpdate(UpdateResultEvent)
		 */
		@Override
		public int doUpdate(final UpdateResultEvent event) throws SQLException {
			if (retryCount > currentCount++) {
				throw new SQLException("Test Retry Exception", "23000", errorCode);
			}

			return event.getResult();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.EventSubscriber#doBatch(BatchResultEvent)
		 */
		@Override
		public int[] doBatch(final BatchResultEvent event) throws SQLException {
			if (retryCount > currentCount++) {
				throw new SQLException("Test Retry Exception", "23000", errorCode);
			}

			return event.getResult();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.EventSubscriber#doProcedure(ProcedureResultEvent)
		 */
		@Override
		public boolean doProcedure(ProcedureResultEvent event) throws SQLException {
			if (retryCount > currentCount++) {
				throw new SQLException("Test Retry Exception", "23000", errorCode);
			}

			return event.getResult();
		}
	}
}
