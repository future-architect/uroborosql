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

import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.filter.AbstractSqlFilter;
import jp.co.future.uroborosql.fluent.Procedure;
import jp.co.future.uroborosql.fluent.SqlQuery;
import jp.co.future.uroborosql.fluent.SqlUpdate;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;

/**
 * エラーハンドリングのテスト
 *
 * @author H.Sugimoto
 */
public class SqlAgentRetryTest {
	private SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SqlAgentRetryTest"));
		config.getSqlAgentFactory().setSqlRetryCodeList(Arrays.asList("54", "60", "30006"));

		try (SqlAgent agent = config.createAgent()) {

			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		}
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			SqlQuery query = agent.query("example/select_product").paramList("product_id", 0, 1).retry(retryCount + 1);
			query.collect();
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
	}

	/**
	 * クエリ実行のリトライ（待機あり）
	 */
	@Test
	public void testQueryRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			SqlQuery query = agent.query("example/select_product").paramList("product_id", 0, 1)
					.retry(retryCount + 1, 10);
			query.collect();
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
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
		try (SqlAgent agent = config.createAgent()) {
			query = agent.query("example/select_product").paramList("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (SQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
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
		try (SqlAgent agent = config.createAgent()) {
			query = agent.query("example/select_product").paramList("product_id", 0, 1).retry(retryCount - 1);
			query.collect();
			fail();
		} catch (SQLException ex) {
			assertThat(query.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
			update.count();
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
	}

	/**
	 * 更新のリトライ
	 */
	@Test
	public void testUpdateRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
			update.count();
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
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
		try (SqlAgent agent = config.createAgent()) {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (SQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
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
		try (SqlAgent agent = config.createAgent()) {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			update.count();
			fail();
		} catch (SQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * バッチ更新のリトライ
	 */
	@Test
	public void testBatchRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount + 1).batch();
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
	}

	/**
	 * バッチ更新のリトライ
	 */
	@Test
	public void testBatchRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			SqlUpdate update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount + 1, 10).batch();
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
	}

	/**
	 * バッチ更新のリトライ（リトライ回数上限）
	 */
	@Test
	public void testBatchRetryOver() throws Exception {
		int retryCount = 3;
		int errorCode = 60;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlUpdate update = null;
		try (SqlAgent agent = config.createAgent()) {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount - 1).batch();
			fail();
		} catch (SQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(retryCount - 1));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * バッチ更新のリトライ
	 */
	@Test
	public void testBatchNoRetry() throws Exception {
		int retryCount = 3;
		int errorCode = 1;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		SqlUpdate update = null;
		try (SqlAgent agent = config.createAgent()) {
			update = agent.update("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).addBatch()
					.param("product_name", "test2").param("product_kana_name", "test_kana2")
					.param("jan_code", "1234567890124").param("product_description", "1")
					.param("ins_datetime", LocalDate.now()).addBatch();
			update.retry(retryCount - 1).batch();
			fail();
		} catch (SQLException ex) {
			assertThat(update.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryNoWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			Procedure proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1);
			proc.call();
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
	}

	/**
	 * プロシージャのリトライ
	 */
	@Test
	public void testProcedureRetryWait() throws Exception {
		int retryCount = 3;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, 60));

		try (SqlAgent agent = config.createAgent()) {
			Procedure proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount + 1, 10);
			proc.call();
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(retryCount));
		}
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
		try (SqlAgent agent = config.createAgent()) {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
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
	public void testProcedureNoRetry() throws Exception {
		int retryCount = 3;
		int errorCode = 1;
		config.getSqlFilterManager().addSqlFilter(new RetrySqlFilter(retryCount, errorCode));

		Procedure proc = null;
		try (SqlAgent agent = config.createAgent()) {
			proc = agent.proc("example/insert_product_regist_work").param("product_name", "test")
					.param("product_kana_name", "test_kana").param("jan_code", "1234567890123")
					.param("product_description", "").param("ins_datetime", LocalDate.now()).retry(retryCount - 1);
			proc.call();
			fail();
		} catch (SQLException ex) {
			assertThat(proc.context().contextAttrs().get("__retryCount"), is(0));
			assertThat(errorCode, is(ex.getErrorCode()));
		}
	}

	/**
	 * リトライテスト用の例外をスローするフィルター
	 *
	 * @author H.Sugimoto
	 *
	 */
	private final class RetrySqlFilter extends AbstractSqlFilter {
		private int retryCount = 0;
		private int currentCount = 0;
		private int errorCode = -1;

		public RetrySqlFilter(final int retryCount, final int errorCode) {
			this.retryCount = retryCount;
			this.currentCount = 0;
			this.errorCode = errorCode;
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
				throw new SQLException("Test Retry Exception", "23000", errorCode);
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
				throw new SQLException("Test Retry Exception", "23000", errorCode);
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
				throw new SQLException("Test Retry Exception", "23000", errorCode);
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
				throw new SQLException("Test Retry Exception", "23000", errorCode);
			}

			return super.doProcedure(sqlContext, callableStatement, result);
		}
	}

}
