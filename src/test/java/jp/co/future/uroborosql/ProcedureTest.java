package jp.co.future.uroborosql;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;

public class ProcedureTest {
	/**
	 * SQL管理クラス
	 */
	SqlConfig config;

	@BeforeEach
	public void setUp() {
		config = UroboroSQL.builder("jdbc:h2:mem:LocalTxManagerTest;DB_CLOSE_DELAY=-1", "sa", null).build();
		try (SqlAgent agent = config.agent()) {
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
	public void testCallStoredFunctionWithinTransaction() {
		config.getSqlAgentFactory().setForceUpdateWithinTransaction(true);

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR).param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					fail();
				}
			});

			agent.required(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.outParam("ret", JDBCType.VARCHAR.getVendorTypeNumber()).param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					fail();
				}
			});

			// TODO H2ではinoutパラメータがうまく動作しないため、outパラメータと同様の動作をすることだけ確認する

			agent.required(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParam("ret", "test2", JDBCType.VARCHAR).param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					fail();
				}
			});

			agent.required(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParam("ret", "test2", JDBCType.VARCHAR.getVendorTypeNumber()).param("param1", "test1")
							.call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					fail();
				}
			});

			agent.required(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParamIfAbsent("ret", "test2", JDBCType.VARCHAR).param("param1", "test1").call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					fail();
				}
			});

			agent.required(() -> {
				try {
					Map<String, Object> ans = agent.procWith("{/*ret*/ = call MYFUNCTION(/*param1*/)}")
							.inOutParamIfAbsent("ret", "test2", JDBCType.VARCHAR.getVendorTypeNumber())
							.param("param1", "test1")
							.call();
					assertThat(ans.get("ret"), is("TEST1"));
				} catch (SQLException ex) {
					fail();
				}
			});
		}
	}
}
