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

import jp.co.future.uroborosql.SqlAgent;

import jp.co.future.uroborosql.utils.StringUtils;
import org.h2.jdbcx.JdbcDataSource;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * TestCase DefaultSqlConfig
 *
 * @author H.Sugimoto
 *
 */
public class DefaultSqlConfigTest {
	private static final String JDBC_URL = "jdbc:h2:mem:SqlConfigTest";
	private static final String JDBC_USER = "sa";
	private static final String JDBC_PASSWORD = "sa";

	private JdbcDataSource ds;

	private Connection conn;

	@SuppressWarnings("deprecation")
	@Before
	public void setUp() throws Exception {
		ds = new JdbcDataSource();
		ds.setURL(JDBC_URL + System.currentTimeMillis() + ";DB_CLOSE_DELAY=-1");
		ds.setUser(JDBC_USER);
		ds.setPassword(JDBC_PASSWORD);

		SqlConfig config = DefaultSqlConfig.getConfig(ds);

		try (SqlAgent agent = config.createAgent()) {
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
	@SuppressWarnings("deprecation")
	@Test
	public void testGetConfigConnection() throws Exception {
		validate(DefaultSqlConfig.getConfig(conn));
	}

	/**
	 * Testcase of {@link jp.co.future.uroborosql.config.DefaultSqlConfig#getConfig(javax.sql.DataSource)} のためのテスト・メソッド。
	 */
	@SuppressWarnings("deprecation")
	@Test
	public void testGetConfigDataSource() throws Exception {
		validate(DefaultSqlConfig.getConfig(ds));
	}

	private void validate(final SqlConfig config) throws Exception {
		try (SqlAgent agent = config.agent()) {
			assertThat(agent.query("example/select_product").collect().size(), is(2));
		}

	}
}
