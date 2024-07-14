/**
 *
 */
package jp.co.future.uroborosql.config;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.util.List;

import org.h2.jdbcx.JdbcDataSource;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.context.ExecutionContextProviderImpl;
import jp.co.future.uroborosql.context.test.TestConsts;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.utils.ObjectUtils;

/**
 * TestCase DefaultSqlConfig
 *
 * @author H.Sugimoto
 *
 */
public class SqlConfigTest {
	private static final String JDBC_URL = "jdbc:h2:mem:SqlConfigTest";
	private static final String JDBC_USER = "sa";
	private static final String JDBC_PASSWORD = "sa";

	private JdbcDataSource ds;

	private Connection conn;

	@BeforeEach
	public void setUp() throws Exception {
		ds = new JdbcDataSource();
		ds.setURL(JDBC_URL + System.currentTimeMillis() + ";DB_CLOSE_DELAY=-1");
		ds.setUser(JDBC_USER);
		ds.setPassword(JDBC_PASSWORD);

		var config = UroboroSQL.builder(ds).build();

		try (var agent = config.agent()) {
			// create table
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (ObjectUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();

			// insert init data
			sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/setup/insert_product.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (ObjectUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		}

		conn = ds.getConnection();

	}

	@AfterEach
	public void tearDown() throws Exception {
		conn.close();
	}

	/**
	 * Testcase of {@link jp.co.future.uroborosql.config.DefaultSqlConfig#getConfig(java.sql.Connection)}
	 */
	@Test
	void testGetConfigConnection() throws Exception {
		validate(UroboroSQL.builder(conn).build());
	}

	/**
	 * Testcase of {@link jp.co.future.uroborosql.config.DefaultSqlConfig#getConfig(javax.sql.DataSource)} のためのテスト・メソッド。
	 */
	@Test
	void testGetConfigDataSource() throws Exception {
		validate(UroboroSQL.builder(ds).build());
	}

	private void validate(final SqlConfig config) throws Exception {
		try (var agent = config.agent()) {
			assertThat(agent.query("example/select_product")
					.collect().size(), is(2));
		}
	}

	@Test
	void testWithExecutionContextProviderConstantSettings() throws Exception {
		var config = UroboroSQL.builder(ds)
				.setExecutionContextProvider(new ExecutionContextProviderImpl()
						.setConstantClassNames(List.of(TestConsts.class.getName()))
						.setEnumConstantPackageNames(List.of(TestEnum1.class.getPackage().getName())))
				.build();
		try (var agent = config.agent()) {
			var ans = agent
					.queryWith("select /*#CLS_INNER_CLASS_ISTRING*/'' as VAL, /*#CLS_TEST_ENUM1_A*/ as ENUM")
					.one();

			assertThat(ans.get("VAL"), is(TestConsts.InnerClass.ISTRING));
			assertThat(ans.get("ENUM"), is(TestEnum1.A.toString()));
		}
	}

}
