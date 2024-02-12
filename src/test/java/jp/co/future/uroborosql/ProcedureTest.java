package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.JDBCType;
import java.sql.SQLException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;

public class ProcedureTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@BeforeEach
	public void setUp() {
		config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1", "sa", "sa")
				.build();
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("DROP ALIAS IF EXISTS MYFUNCTION").count();
				agent.updateWith("CREATE ALIAS MYFUNCTION AS $$\r\n" +
						"String toUpperCase(String lower) throws Exception {\r\n" +
						"    return lower.toUpperCase();\r\n" +
						"}\r\n" +
						"$$;").count();
			});
		}
	}

	@Test
	void testCallStoredFunctionWithinTransaction() {
		config.getSqlAgentProvider().setForceUpdateWithinTransaction(true);

		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR)
							.param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});

			agent.required(() -> {
				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR.getVendorTypeNumber())
							.param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});

			// TODO H2ではinoutパラメータがうまく動作しないため、outパラメータと同様の動作をすることだけ確認する

			agent.required(() -> {
				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParam("ret", "test2", JDBCType.VARCHAR)
							.param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});

			agent.required(() -> {
				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParam("ret", "test2", JDBCType.VARCHAR.getVendorTypeNumber())
							.param("param1", "test1")
							.call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});

			agent.required(() -> {
				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParamIfAbsent("ret", "test2", JDBCType.VARCHAR)
							.param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});

			agent.required(() -> {
				try {
					var ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParamIfAbsent("ret", "test2", JDBCType.VARCHAR.getVendorTypeNumber())
							.param("param1", "test1")
							.call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					Assertions.fail();
				}
			});
		}
	}

	/**
	 * SQLファイルが存在しない場合のテストケース。
	 */
	@Test
	void testNotFoundFile() throws Exception {
		Assertions.assertThrowsExactly(UroborosqlRuntimeException.class, () -> {
			try (var agent = config.agent()) {
				var ctx = agent.context().setSqlName("file");
				agent.procedure(ctx);
			}
		});
	}

	/**
	 * SQLファイルが空文字の場合のテストケース。
	 */
	@Test
	void testSqlNameEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			try (var agent = config.agent()) {
				agent.proc("");
			}
		});
	}

	/**
	 * プロシージャ実行処理のテストケース（SQLがNULLの場合）。
	 */
	@Test
	void testProcWithSqlNull() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			try (var agent = config.agent()) {
				agent.procWith(null);
			}
		});
	}

	/**
	 * プロシージャ実行処理のテストケース（SQLがEmptyの場合）。
	 */
	@Test
	void testProcWithSqlEmpty() throws Exception {
		Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
			try (var agent = config.agent()) {
				agent.procWith("");
			}
		});
	}

}
