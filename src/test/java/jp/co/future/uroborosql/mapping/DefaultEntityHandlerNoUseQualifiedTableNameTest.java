package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.sql.SQLException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.AuditLogEventSubscriber;

/**
 * DefaultEntityHandler の "uroborosql.use.qualified.table.name=false" オプションのテスト.<br>
 * クラスロード時に設定されるため、単一のテストでは成功するが一括実行では失敗してしまう。そのためIgnoreを指定している
 *
 * @author H.Sugimoto
 */
@Disabled
public class DefaultEntityHandlerNoUseQualifiedTableNameTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:DefaultEntityHandlerWithDefaultValueTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
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

		System.setProperty("uroborosql.use.qualified.table.name", "false");

		config = UroboroSQL.builder(url, user, password)
				.build();
		config.getEventListenerHolder().addEventSubscriber(new AuditLogEventSubscriber());

		DefaultEntityHandler.clearCache();
		MappingUtils.clearCache();
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Test
	void testSelectContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var handler = (DefaultEntityHandler) config.getEntityHandler();
					var metadata = handler.getMetadata(agent, TestEntity.class);
					var context = handler.createSelectContext(agent, metadata, TestEntity.class, false);
					assertThat(context.getSql(), not(containsString("PUBLIC")));
				} catch (SQLException e) {
					Assertions.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	void testInsertContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var handler = (DefaultEntityHandler) config.getEntityHandler();
					var metadata = handler.getMetadata(agent, TestEntity.class);
					var context = handler.createInsertContext(agent, metadata, TestEntity.class);
					assertThat(context.getSql(), not(containsString("PUBLIC")));
				} catch (SQLException e) {
					Assertions.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	void testUpdateContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var handler = (DefaultEntityHandler) config.getEntityHandler();
					var metadata = handler.getMetadata(agent, TestEntity.class);
					var context = handler.createUpdateContext(agent, metadata, TestEntity.class, false);
					assertThat(context.getSql(), not(containsString("PUBLIC")));
				} catch (SQLException e) {
					Assertions.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	void testDeleteContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var handler = (DefaultEntityHandler) config.getEntityHandler();
					var metadata = handler.getMetadata(agent, TestEntity.class);
					var context = handler.createDeleteContext(agent, metadata, TestEntity.class, false);
					assertThat(context.getSql(), not(containsString("PUBLIC")));
				} catch (SQLException e) {
					Assertions.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	void testBatchInsertContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var handler = (DefaultEntityHandler) config.getEntityHandler();
					var metadata = handler.getMetadata(agent, TestEntity.class);
					var context = handler.createBatchInsertContext(agent, metadata, TestEntity.class);
					assertThat(context.getSql(), not(containsString("PUBLIC")));
				} catch (SQLException e) {
					Assertions.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	void testBatchUpdateContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					var handler = (DefaultEntityHandler) config.getEntityHandler();
					var metadata = handler.getMetadata(agent, TestEntity.class);
					var context = handler.createBatchUpdateContext(agent, metadata, TestEntity.class);
					assertThat(context.getSql(), not(containsString("PUBLIC")));
				} catch (SQLException e) {
					Assertions.fail(e.getMessage());
				}
			});
		}
	}

	@Test
	void testTruncateContext() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				// SQL文が取得できないので、成功することで確認とする
				agent.truncate(TestEntity.class);
			});
		}
	}

}
