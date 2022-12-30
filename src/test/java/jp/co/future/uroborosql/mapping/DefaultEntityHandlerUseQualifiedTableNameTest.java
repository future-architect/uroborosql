package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;

/**
 * DefaultEntityHandler の "uroborosql.use.qualified.table.name=true" オプションのテスト.<br>
 *
 * @author H.Sugimoto
 */
public class DefaultEntityHandlerUseQualifiedTableNameTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:DefaultEntityHandlerWithDefaultValueTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test");
				stmt.execute(
						"create table if not exists test(id INTEGER auto_increment ,name VARCHAR(20) default 'default name',age NUMERIC(5) not null default 10,birthday DATE default '2000-01-01',memo VARCHAR(500),lock_version INTEGER, primary key(id))");
				stmt.execute("comment on table test is 'test'");
				stmt.execute("comment on column test.id is 'id'");
				stmt.execute("comment on column test.name is 'name'");
				stmt.execute("comment on column test.age is 'age'");
				stmt.execute("comment on column test.birthday is 'birthday'");
				stmt.execute("comment on column test.memo is 'memo'");
				stmt.execute("comment on column test.lock_version is 'lock version'");
			}
		}

		System.setProperty("uroborosql.use.qualified.table.name", "true");

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Test
	public void testSelectContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					DefaultEntityHandler handler = (DefaultEntityHandler) config.getEntityHandler();
					TableMetadata metadata = handler.getMetadata(agent, TestEntity.class);
					ExecutionContext context = handler.createSelectContext(agent, metadata, TestEntity.class, false);
					assertThat(context.getSql(), containsString("PUBLIC"));
				} catch (SQLException e) {
					Assert.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	public void testInsertContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					DefaultEntityHandler handler = (DefaultEntityHandler) config.getEntityHandler();
					TableMetadata metadata = handler.getMetadata(agent, TestEntity.class);
					ExecutionContext context = handler.createInsertContext(agent, metadata, TestEntity.class);
					assertThat(context.getSql(), containsString("PUBLIC"));
				} catch (SQLException e) {
					Assert.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	public void testUpdateContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					DefaultEntityHandler handler = (DefaultEntityHandler) config.getEntityHandler();
					TableMetadata metadata = handler.getMetadata(agent, TestEntity.class);
					ExecutionContext context = handler.createUpdateContext(agent, metadata, TestEntity.class, false);
					assertThat(context.getSql(), containsString("PUBLIC"));
				} catch (SQLException e) {
					Assert.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	public void testDeleteContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					DefaultEntityHandler handler = (DefaultEntityHandler) config.getEntityHandler();
					TableMetadata metadata = handler.getMetadata(agent, TestEntity.class);
					ExecutionContext context = handler.createDeleteContext(agent, metadata, TestEntity.class, false);
					assertThat(context.getSql(), containsString("PUBLIC"));
				} catch (SQLException e) {
					Assert.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	public void testBatchInsertContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					DefaultEntityHandler handler = (DefaultEntityHandler) config.getEntityHandler();
					TableMetadata metadata = handler.getMetadata(agent, TestEntity.class);
					ExecutionContext context = handler.createBatchInsertContext(agent, metadata, TestEntity.class);
					assertThat(context.getSql(), containsString("PUBLIC"));
				} catch (SQLException e) {
					Assert.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	public void testBatchUpdateContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				try {
					DefaultEntityHandler handler = (DefaultEntityHandler) config.getEntityHandler();
					TableMetadata metadata = handler.getMetadata(agent, TestEntity.class);
					ExecutionContext context = handler.createBatchUpdateContext(agent, metadata, TestEntity.class);
					assertThat(context.getSql(), containsString("PUBLIC"));
				} catch (SQLException e) {
					Assert.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	public void testTruncateContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				// SQL文が取得できないので、成功することで確認とする
				agent.truncate(TestEntity.class);
			});
		}
	}

}
