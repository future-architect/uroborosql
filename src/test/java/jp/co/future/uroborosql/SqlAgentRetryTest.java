package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.CallableStatement;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.util.Arrays;

import org.apache.commons.lang3.StringUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;

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
		config.getSqlAgentFactory().setSqlRetryCodeList(Arrays.asList("54", "60", "30006"));

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

		SqlQuery query = agent.query("example/select_product").paramArray("product_id", 0, 1).retry(retryCount + 1);
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

		SqlQuery query = agent.query("example/select_product").paramArray("product_id", 0, 1).retry(retryCount + 1);
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

		SqlQuery query = agent.query("example/select_product").paramArray("product_id", 0, 1)
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

		SqlQuery query = agent.query("example/select_product").paramArray("product_id", 0, 1)
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
			query = agent.query("example/select_product").paramArray("product_id", 0, 1).retry(retryCount - 1);
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
			query = agent.query("example/select_product").paramArray("product_id", 0, 1).retry(retryCount - 1);
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
			query = agent.query("example/select_product").paramArray("product_id", 0, 1).retry(retryCount - 1);
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
			query = agent.query("example/select_product").paramArray("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
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
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
				.param("product_name", "test2").param("product_kana_name", "test_kana2")
				.param("jan_code", "1234567890124").param("product_description", "1")
				.param("ins_datetime", LocalDate.now()).addBatch();
		update.retry(retryCount + 1).batch();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * バッチ更新のリトライ
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchRetryNoWaitSqlState() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
				.param("product_name", "test2").param("product_kana_name", "test_kana2")
				.param("jan_code", "1234567890124").param("product_description", "1")
				.param("ins_datetime", LocalDate.now()).addBatch();
		update.retry(retryCount + 1).batch();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * バッチ更新のリトライ
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
				.param("product_name", "test2").param("product_kana_name", "test_kana2")
				.param("jan_code", "1234567890124").param("product_description", "1")
				.param("ins_datetime", LocalDate.now()).addBatch();
		update.retry(retryCount + 1, 10).batch();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * バッチ更新のリトライ
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchRetryWaitSqlState() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 0, "60"));

		SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
				.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
				.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
				.param("product_name", "test2").param("product_kana_name", "test_kana2")
				.param("jan_code", "1234567890124").param("product_description", "1")
				.param("ins_datetime", LocalDate.now()).addBatch();
		update.retry(retryCount + 1, 10).batch();
		assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
	}

	/**
	 * バッチ更新のリトライ（リトライ回数上限）
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchRetryOver() throws Exception {
		int retryCount = 3;
		int errorCode = 60;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount - 1).batch();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * バッチ更新のリトライ（リトライ回数上限）
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchRetryOverSqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "60";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount - 1).batch();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(ex.getErrorCode(), is(errorCode));
			assertThat(ex.getSQLState(), is(sqlState));
		}
	}

	/**
	 * バッチ更新のリトライ
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchNoRetry() throws Exception {
		int retryCount = 3;
		int errorCode = 1;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount - 1).batch();
			fail();
		} catch (UroborosqlSQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(ex.getErrorCode(), is(errorCode));
		}
	}

	/**
	 * バッチ更新のリトライ
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testBatchNoRetrySqlState() throws Exception {
		int retryCount = 3;
		int errorCode = 0;
		String sqlState = "1";
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode, sqlState));

		SqlUpdate update = null;
		try {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount - 1).batch();
			fail();
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
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doQuery(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, java.sql.ResultSet)
		 */
		@Override
		public ResultSet doQuery(final SqlContext sqlContext, final PreparedStatement preparedStatement,
				final ResultSet resultSet) throws SQLException {
			if (retryCount > currentCount++) {
				preparedStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doQuery(sqlContext, preparedStatement, resultSet);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doUpdate(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int)
		 */
		@Override
		public int doUpdate(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int result)
				throws SQLException {
			if (retryCount > currentCount++) {
				preparedStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doUpdate(sqlContext, preparedStatement, result);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doBatch(jp.co.future.uroborosql.context.SqlContext, java.sql.PreparedStatement, int[])
		 */
		@Override
		public int[] doBatch(final SqlContext sqlContext, final PreparedStatement preparedStatement, final int[] result)
				throws SQLException {
			if (retryCount > currentCount++) {
				preparedStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doBatch(sqlContext, preparedStatement, result);
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see jp.co.future.uroborosql.filter.AbstractSqlFilter#doProcedure(jp.co.future.uroborosql.context.SqlContext, java.sql.CallableStatement, boolean)
		 */
		@Override
		public boolean doProcedure(final SqlContext sqlContext, final CallableStatement callableStatement,
				final boolean result) throws SQLException {
			if (retryCount > currentCount++) {
				callableStatement.getConnection().rollback();
				throw new SQLException("Test Retry Exception", sqlState, errorCode);
			}

			return super.doProcedure(sqlContext, callableStatement, result);
		}
	}
}
