package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.EventSubscriber;
import jp.co.future.uroborosql.event.ResultEvent.BatchResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.ProcedureResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.QueryResultEvent;
import jp.co.future.uroborosql.event.ResultEvent.UpdateResultEvent;
import jp.co.future.uroborosql.exception.PessimisticLockException;
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
public class SqlAgentRetryTest {
	private SqlConfig config;

	private SqlAgent agent;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentRetryTest")).build();
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

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryRetryNoWait() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 60);

		var query = agent.addSubscriber(subscriber)
				.query("example/select_product")
				.param("product_id", List.of(0, 1))
				.retry(retryCount + 1);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryRetryNoWaitSqlState() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 0, "60");

		var query = agent.addSubscriber(subscriber)
				.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount + 1);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	public void testQueryRetryWait() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 60);

		var query = agent.addSubscriber(subscriber)
				.query("example/select_product").param("product_id", List.of(0, 1))
				.retry(retryCount + 1, 10);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	public void testQueryRetryWaitSqlState() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 0, "60");

		var query = agent.addSubscriber(subscriber)
				.query("example/select_product").param("product_id", List.of(0, 1))
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
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		SqlQuery query = null;
		try {
			query = agent.addSubscriber(subscriber)
					.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ回数上限）
	 */
	@Test
	public void testQueryRetryOverSqlState() {
		var retryCount = 3;
		var errorCode = 0;
		var sqlState = "60";
		var subscriber = new RetryEventSubscriber(retryCount, errorCode, sqlState);

		SqlQuery query = null;
		try {
			query = agent.addSubscriber(subscriber)
					.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testQueryNoRetry() {
		var retryCount = 3;
		var errorCode = 1;
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		SqlQuery query = null;
		try {
			query = agent.addSubscriber(subscriber)
					.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testQueryNoRetrySqlState() {
		var retryCount = 3;
		var errorCode = 0;
		var sqlState = "1";
		var subscriber = new RetryEventSubscriber(retryCount, errorCode, sqlState);

		SqlQuery query = null;
		try {
			query = agent.addSubscriber(subscriber)
					.query("example/select_product").param("product_id", List.of(0, 1)).retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testQueryRetryWithPessimisticLock() {
		var retryCount = 3;
		var errorCode = 50200;
		// SqlRetryCodeListを空にして、悲観ロック対象エラーコードで判定する
		config.getSqlAgentProvider().setSqlRetryCodeList(Collections.emptyList());
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		SqlQuery query = null;
		try {
			query = agent.addSubscriber(subscriber)
					.query("example/select_product").param("product_id", Arrays.asList(0, 1))
					.retry(retryCount - 1);
			query.collect();
			assertThat("Fail here.", false);
		} catch (PessimisticLockException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
		} catch (Exception ex) {
			assertThat("Fail here.", false);
		}
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryNoWait() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 60);

		var update = agent.addSubscriber(subscriber)
				.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryNoWaitSqlState() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 0, "60");

		var update = agent.addSubscriber(subscriber)
				.update("example/insert_product_regist_work").param("product_name", "test")
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
		var subscriber = new RetryEventSubscriber(retryCount, 60);

		var update = agent.addSubscriber(subscriber)
				.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryWaitSqlState() {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 0, "60");

		var update = agent.addSubscriber(subscriber)
				.update("example/insert_product_regist_work").param("product_name", "test")
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
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		SqlUpdate update = null;
		try {
			update = agent.addSubscriber(subscriber)
					.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * 更新のリトライ（リトライ回数上限）
	 */
	@Test
	public void testUpdateRetryOverSqlState() {
		var retryCount = 3;
		var errorCode = 0;
		var sqlState = "60";
		var subscriber = new RetryEventSubscriber(retryCount, errorCode, sqlState);

		SqlUpdate update = null;
		try {
			update = agent.addSubscriber(subscriber)
					.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * 更新のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testUpdateNotRetry() {
		var retryCount = 3;
		var errorCode = 1;
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		SqlUpdate update = null;
		try {
			update = agent.addSubscriber(subscriber)
					.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * 更新のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testUpdateNotRetrySqlState() {
		var retryCount = 3;
		var errorCode = 0;
		var sqlState = "1";
		var subscriber = new RetryEventSubscriber(retryCount, errorCode, sqlState);

		SqlUpdate update = null;
		try {
			update = agent.addSubscriber(subscriber)
					.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			assertThat("Fail here.", false);
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryNoWait() throws Exception {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 60);

		var proc = agent.addSubscriber(subscriber)
				.proc("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryNoWaitSqlState() throws Exception {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 0, "60");

		var proc = agent.addSubscriber(subscriber)
				.proc("example/insert_product_regist_work").param("product_name", "test")
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
		var subscriber = new RetryEventSubscriber(retryCount, 60);

		var proc = agent.addSubscriber(subscriber)
				.proc("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryWaitSqlState() throws Exception {
		var retryCount = 3;
		var subscriber = new RetryEventSubscriber(retryCount, 0, "60");

		var proc = agent.addSubscriber(subscriber)
				.proc("example/insert_product_regist_work").param("product_name", "test")
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
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		Procedure proc = null;
		try {
			proc = agent.addSubscriber(subscriber)
					.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ回数上限）
	 */
	@Test
	public void testProcedureRetryOverSqlState() {
		var retryCount = 3;
		var errorCode = 0;
		var sqlState = "60";
		var subscriber = new RetryEventSubscriber(retryCount, errorCode, sqlState);

		Procedure proc = null;
		try {
			proc = agent.addSubscriber(subscriber)
					.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testProcedureNoRetry() {
		var retryCount = 3;
		var errorCode = 1;
		var subscriber = new RetryEventSubscriber(retryCount, errorCode);

		Procedure proc = null;
		try {
			proc = agent.addSubscriber(subscriber)
					.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testProcedureNoRetrySqlState() {
		var retryCount = 3;
		var errorCode = 0;
		var sqlState = "1";
		var subscriber = new RetryEventSubscriber(retryCount, errorCode, sqlState);

		Procedure proc = null;
		try {
			proc = agent.addSubscriber(subscriber)
					.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			assertThat("Fail here.", false);
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	private final class RetryEventSubscriber implements EventSubscriber {
		private int retryCount = 0;
		private int currentCount = 0;
		private int errorCode = -1;
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
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.EventSubscriber#doQuery(QueryResultEvent)
		 */
		@Override
		public ResultSet doQuery(final QueryResultEvent event) throws SQLException {
			if (retryCount > currentCount++) {
				event.getPreparedStatement().getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
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
				event.getPreparedStatement().getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
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
				event.getPreparedStatement().getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return event.getResult();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.event.EventSubscriber#doProcedure(ProcedureResultEvent)
		 */
		@Override
		public boolean doProcedure(final ProcedureResultEvent event) throws SQLException {
			if (retryCount > currentCount++) {
				event.getCallableStatement().getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return event.getResult();
		}
	}
}
