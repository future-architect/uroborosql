package jp.co.future.uroborosql;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.PessimisticLockException;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * 悲観ロックのテスト
 *
 * @author H.Sugimoto
 */
public class SqlAgentPessimisticLockTest {
	private SqlConfig config;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:" + this.getClass().getSimpleName() + ";DB_CLOSE_DELAY=-1", "sa", "sa")
				.setSqlAgentProvider(new SqlAgentProviderImpl().setQueryTimeout(10))
				.build();
		try (var agent = config.agent()) {
			var ddls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var ddl : ddls) {
				if (StringUtils.isNotBlank(ddl)) {
					agent.updateWith(ddl.trim()).count();
				}
			}
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/setup/insert_product.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
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
	void testQueryNoRetry() throws Exception {
		var sql = "select * from product where product_id = 1 for update";
		try (var agent = config.agent()) {
			agent.required(() -> {
				var products1 = agent.queryWith(sql)
						.collect();
				assertThat(products1.size(), is(1));

				agent.requiresNew(() -> {
					try {
						agent.queryWith(sql)
								.collect();
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
