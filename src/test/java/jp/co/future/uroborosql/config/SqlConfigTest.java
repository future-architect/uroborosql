/**
 *
 */
package jp.co.future.uroborosql.config;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;
import java.util.Arrays;
import java.util.Map;

import org.h2.jdbcx.JdbcDataSource;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.context.test.TestConsts;
import jp.co.future.uroborosql.context.test.TestEnum1;
import jp.co.future.uroborosql.utils.StringUtils;

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

	@Before
	public void setUp() throws Exception {
		ds = new JdbcDataSource();
		ds.setURL(JDBC_URL + System.currentTimeMillis() + ";DB_CLOSE_DELAY=-1");
		ds.setUser(JDBC_USER);
		ds.setPassword(JDBC_PASSWORD);

		SqlConfig config = UroboroSQL.builder(ds).build();

		try (SqlAgent agent = config.agent()) {
			// create table
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();

			// insert init data
			sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/setup/insert_product.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		}

		conn = ds.getConnection();

	}

	@After
	public void tearDown() throws Exception {
		conn.close();
	}

	/**
	 * Testcase of {@link jp.co.future.uroborosql.config.DefaultSqlConfig#getConfig(java.sql.Connection)}
	 */
	@Test
	public void testGetConfigConnection() throws Exception {
		validate(UroboroSQL.builder(conn).build());
	}

	/**
	 * Testcase of {@link jp.co.future.uroborosql.config.DefaultSqlConfig#getConfig(javax.sql.DataSource)} のためのテスト・メソッド。
	 */
	@Test
	public void testGetConfigDataSource() throws Exception {
		validate(UroboroSQL.builder(ds).build());
	}

	private void validate(final SqlConfig config) throws Exception {
		try (SqlAgent agent = config.agent()) {
			assertThat(agent.query("example/select_product").collect().size(), is(2));
		}
	}

	@Test
	public void testWithSqlContextFactoryConstantSettings() throws Exception {
		SqlConfig config = UroboroSQL.builder(ds)
				.setSqlContextFactory(new SqlContextFactoryImpl()
						.setConstantClassNames(Arrays.asList(TestConsts.class.getName()))
						.setEnumConstantPackageNames(Arrays.asList(TestEnum1.class.getPackage().getName())))
				.build();
		try (SqlAgent agent = config.agent()) {
			Map<String, Object> ans = agent
					.queryWith("select /*#CLS_INNER_CLASS_ISTRING*/'' as VAL, /*#CLS_TEST_ENUM1_A*/ as ENUM").one();

			assertThat(ans.get("VAL"), is(TestConsts.InnerClass.ISTRING));
			assertThat(ans.get("ENUM"), is(TestEnum1.A.toString()));
		}
	}

}
