/**
 * Connection Poolを使ったDataSourceのテスト
 */
package jp.co.future.uroborosql.config;

import jp.co.future.uroborosql.SqlAgent;
import org.apache.commons.lang3.StringUtils;
import org.h2.jdbcx.JdbcConnectionPool;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Connection;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * TestCase DefaultSqlConfigConnectionPool
 *
 * @author H.Sugimoto
 */
public class DefaultSqlConfigConnectionPoolTest {
    private static final String JDBC_URL = "jdbc:h2:mem:SqlConfigConnectionPoolTest;DB_CLOSE_DELAY=-1";
    private static final String JDBC_USER = "sa";
    private static final String JDBC_PASSWORD = "sa";

    private static SqlConfig config;

    private static JdbcConnectionPool pool;

    @BeforeClass
    public static void setUpClass() throws Exception {

        pool = JdbcConnectionPool.create(JDBC_URL, JDBC_USER, JDBC_PASSWORD);
        pool.setMaxConnections(10);

        config = DefaultSqlConfig.getConfig(pool);

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
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
        pool.dispose();
    }

    /**
     * Testcase of {@link DefaultSqlConfig#getConfig(javax.sql.DataSource)} のためのテスト・メソッド。
     */
    @Test
    public void testGetConfigDataSourceNoQuery() throws Exception {
        try (SqlAgent agent = config.createAgent()) {
            assertThat(pool.getActiveConnections(), is(0));
        }
        assertThat(pool.getActiveConnections(), is(0));
    }

    /**
     * Testcase of {@link DefaultSqlConfig#getConfig(javax.sql.DataSource)} のためのテスト・メソッド。
     */
    @Test
    public void testGetConfigDataSource() throws Exception {
        try (SqlAgent agent = config.createAgent()) {
            assertThat(agent.query("example/select_product").collect().size(), is(2));
            assertThat(pool.getActiveConnections(), is(1));
        }
        assertThat(pool.getActiveConnections(), is(0));
    }

    /**
     * Testcase of {@link DefaultSqlConfig#getConfig(javax.sql.DataSource)} のためのテスト・メソッド。
     */
    @Test
    public void testGetConfigDataSourceRequired() throws Exception {
        try (SqlAgent agent = config.createAgent()) {
            assertThat(agent.query("example/select_product").collect().size(), is(2));
            assertThat(pool.getActiveConnections(), is(1));

            agent.required(() -> {
                assertThat(agent.query("example/select_product").collect().size(), is(2));
                assertThat(pool.getActiveConnections(), is(2));
                agent.requiresNew(() -> {
                    assertThat(agent.query("example/select_product").collect().size(), is(2));
                    assertThat(pool.getActiveConnections(), is(3));
                });
            });
            assertThat(pool.getActiveConnections(), is(1));
        }
        assertThat(pool.getActiveConnections(), is(0));
    }
}
