package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.PessimisticLockException;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * 悲観ロックのテスト
 *
 * @author H.Sugimoto
 */
public class SqlAgentPessimisticLockTest {
	private static SqlConfig config;

	@BeforeClass
	public static void setUpClass() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:SqlAgentPessimisticLockTest;DB_CLOSE_DELAY=-1;MVCC=true;", "sa", "sa")
				.setSqlAgentProvider(new SqlAgentProviderImpl().setQueryTimeout(10))
				.build();
		try (SqlAgent agent = config.agent()) {
			String[] ddls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String ddl : ddls) {
				if (StringUtils.isNotBlank(ddl)) {
					agent.updateWith(ddl.trim()).count();
				}
			}
			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/setup/insert_product.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
		}
	}

	/**
	 * クエリ実行のリトライ
	 */
	@Test
	public void testQueryNoRetry() throws Exception {
		String sql = "select * from product where product_id = 1 for update";
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				List<Map<String, Object>> products1 = agent.queryWith(sql).collect();
				assertThat(products1.size(), is(1));

				agent.requiresNew(() -> {
					try {
						agent.queryWith(sql).collect();
						fail();
					} catch (PessimisticLockException ex) {
						// OK
					} catch (Exception ex) {
						fail();
					}
				});
				agent.setRollbackOnly();
			});
		}
	}
}
