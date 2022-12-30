package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.CallableStatement;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.exception.PessimisticLockException;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlBatch;
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

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SqlAgentRetryTest")).build();
		config.getSqlAgentProvider().setSqlRetryCodeList(Arrays.asList("54", "60", "30006"));

		agent = config.agent();

		String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
				StandardCharsets.UTF_8).split(";");
		for (String sql : sqls) {
			if (StringUtils.isNotBlank(sql)) {
				agent.updateWith(sql.trim()).count();
			}
		}
		agent.commit();
	}

	@After
	public void tearDown() throws Exception {
		agent.close();
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		SqlQuery query = agent.query("example/select_product").param("product_id", 0, 1).retry(retryCount + 1);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryRetryNoWaitSqlState() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		SqlQuery query = agent.query("example/select_product").param("product_id", 0, 1).retry(retryCount + 1);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	public void testQueryRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		SqlQuery query = agent.query("example/select_product").param("product_id", 0, 1)
				.retry(retryCount + 1, 10);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	public void testQueryRetryWaitSqlState() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		SqlQuery query = agent.query("example/select_product").param("product_id", 0, 1)
				.retry(retryCount + 1, 10);
		query.collect();
		assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * クエリ実行のリトライ（リトライ回数上限）
	 */
	@Test
	public void testQueryRetryOver() throws Exception {
		int retryCount = 3;
		int errorCode = 60;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ回数上限）
	 */
	@Test
	public void testQueryRetryOverSqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "60";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
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
	public void testQueryNoRetry() throws Exception {
		int retryCount = 3;
		int errorCode = 1;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * クエリ実行のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testQueryNoRetrySqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "1";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
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
	public void testQueryRetryWithPessimisticLock() throws Exception {
		int retryCount = 3;
		int errorCode = 50200;
		// SqlRetryCodeListを空にして、悲観ロック対象エラーコードで判定する
		config.getSqlAgentProvider().setSqlRetryCodeList(Collections.emptyList());
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlQuery query = null;
		try {
			query = agent.query("example/select_product").param("product_id", Arrays.asList(0, 1))
					.retry(retryCount - 1);
			query.collect();
			fail();
		} catch (PessimisticLockException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
		} catch (Exception ex) {
			fail();
		}
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryNoWaitSqlState() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryWaitSqlState() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		update.count();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * 更新のリトライ（リトライ回数上限）
	 */
	@Test
	public void testUpdateRetryOver() throws Exception {
		int retryCount = 3;
		int errorCode = 60;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * 更新のリトライ（リトライ回数上限）
	 */
	@Test
	public void testUpdateRetryOverSqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "60";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
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
	public void testUpdateNotRetry() throws Exception {
		int retryCount = 3;
		int errorCode = 1;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * 更新のリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testUpdateNotRetrySqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "1";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * バッチ更新のリトライ
	 */
	@Test
	public void testBatchRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		var paramList = new ArrayList<Map<String, Object>>();
		var paramMap = new HashMap<String, Object>();
		paramMap.put("product_name", "test");
		paramMap.put("product_kana_name", "test_kana");
		paramMap.put("jan_code", "1234567890123");
		paramMap.put("product_description", "");
		paramMap.put("ins_datetime", LocalDate.now());
		paramList.add(paramMap);

		paramMap = new HashMap<String, Object>();
		paramMap.put("product_name", "test2");
		paramMap.put("product_kana_name", "test_kana2");
		paramMap.put("jan_code", "1234567890124");
		paramMap.put("product_description", "1");
		paramMap.put("ins_datetime", LocalDate.now());
		paramList.add(paramMap);

		SqlBatch batch = agent.batch("example/insert_product_regist_work")
				.paramStream(paramList.stream())
				.retry(retryCount + 1);
		batch.count();
		assertThat(batch.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		Procedure proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
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
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		Procedure proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
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
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		Procedure proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
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
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		Procedure proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
		proc.call();
		assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * プロシージャのリトライ（リトライ回数上限）
	 */
	@Test
	public void testProcedureRetryOver() throws Exception {
		int retryCount = 3;
		int errorCode = 60;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ回数上限）
	 */
	@Test
	public void testProcedureRetryOverSqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "60";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
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
	public void testProcedureNoRetry() throws Exception {
		int retryCount = 3;
		int errorCode = 1;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * プロシージャのリトライ（リトライ対象外のエラー発生）
	 */
	@Test
	public void testProcedureNoRetrySqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "1";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		Procedure proc = null;
		try {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * リトライテスト用の例外をスローするフィルター
	 *
	 * @author H.Sugimoto
	 */
	private final class RetrySqlFilter extends AbstractSqlFilter {
		private int retryCount = 0;
		private int currentCount = 0;
		private int errorCode = -1;
		private String sqlState = "";

		public RetrySqlFilter(final int retryCount, final int errorCode) {
			this(retryCount, errorCode, "23000");
		}

		public RetrySqlFilter(final int retryCount, final int errorCode, final String sqlState) {
			this.retryCount = retryCount;
			this.currentCount = 0;
			this.errorCode = errorCode;
			this.sqlState = sqlState;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, java.sql.ResultSet)
		 */
		@Override
		public ResultSet doQuery(final ExecutionContext ExecutionContext, final PreparedStatement preparedStatement,
				final ResultSet resultSet) throws SQLException {
			if (retryCount > currentCount++) {
				preparedStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doQuery(ExecutionContext, preparedStatement, resultSet);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doUpdate(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, int)
		 */
		@Override
		public int doUpdate(final ExecutionContext ExecutionContext, final PreparedStatement preparedStatement,
				final int result)
				throws SQLException {
			if (retryCount > currentCount++) {
				preparedStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doUpdate(ExecutionContext, preparedStatement, result);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doBatch(jp.co.future.uroborosql.context.ExecutionContext, java.sql.PreparedStatement, int[])
		 */
		@Override
		public int[] doBatch(final ExecutionContext ExecutionContext, final PreparedStatement preparedStatement,
				final int[] result)
				throws SQLException {
			if (retryCount > currentCount++) {
				preparedStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doBatch(ExecutionContext, preparedStatement, result);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doProcedure(jp.co.future.uroborosql.context.ExecutionContext, java.sql.CallableStatement, boolean)
		 */
		@Override
		public boolean doProcedure(final ExecutionContext ExecutionContext, final CallableStatement callableStatement,
				final boolean result) throws SQLException {
			if (retryCount > currentCount++) {
				callableStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doProcedure(ExecutionContext, callableStatement, result);
		}
	}
}
