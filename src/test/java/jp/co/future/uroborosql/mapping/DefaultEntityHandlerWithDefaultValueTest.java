package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.time.LocalDate;
import java.time.Month;
import java.util.Optional;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.AuditLogEventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SqlFilterManagerImpl;

public class DefaultEntityHandlerWithDefaultValueTest {

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
						"create table if not exists test(id INTEGER auto_increment ,name VARCHAR(20) default 'default name',age NUMERIC(5) not null default 10,birthday DATE default '2000-01-01',memo VARCHAR(500), primary key(id))");
				stmt.execute("comment on table test is 'test'");
				stmt.execute("comment on column test.id is 'id'");
				stmt.execute("comment on column test.name is 'name'");
				stmt.execute("comment on column test.age is 'age'");
				stmt.execute("comment on column test.birthday is 'birthday'");
				stmt.execute("comment on column test.memo is 'memo'");

			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogEventSubscriber()))
				.build();
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
	void testInsert1() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityWithDefaultValue(1L,
						Optional.of("name1"),
						Optional.of(20),
						Optional.of(LocalDate.of(1990, Month.APRIL, 1)),
						Optional.of("memo1"));
				agent.insert(test1);
				var test2 = new TestEntityWithDefaultValue(2L,
						null,
						null,
						null,
						null);
				agent.insert(test2);
				var data = agent.find(TestEntityWithDefaultValue.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithDefaultValue.class, 2).orElse(null);
				assertThat(data.getId(), is(2L));
				assertThat(data.getName().get(), is("default name"));
				assertThat(data.getAge().get(), is(10));
				assertThat(data.getBirthday().get(), is(LocalDate.of(2000, Month.JANUARY, 1)));
				assertThat(data.getMemo(), is(Optional.empty()));
			});
		}
	}

	@Test
	void testInsert2() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntityWithDefaultValue(null,
						Optional.of("name1"),
						Optional.of(20),
						Optional.of(LocalDate.of(1990, Month.APRIL, 1)),
						Optional.of("memo1"));
				var result1 = agent.insertAndReturn(test1);
				var test2 = new TestEntityWithDefaultValue(null,
						null,
						null,
						null,
						null);
				var result2 = agent.insertAndReturn(test2);
				var data = agent.find(TestEntityWithDefaultValue.class, result1.getId())
						.orElse(null);
				assertThat(data.getId(), is(result1.getId()));
				assertThat(data.getName().get(), is("name1"));
				assertThat(data.getAge().get(), is(20));
				assertThat(data.getBirthday().get(), is(LocalDate.of(1990, Month.APRIL, 1)));
				assertThat(data.getMemo(), is(Optional.of("memo1")));
				data = agent.find(TestEntityWithDefaultValue.class, result2.getId()).orElse(null);
				assertThat(data.getId(), is(result2.getId()));
				assertThat(data.getName().get(), is("default name"));
				assertThat(data.getAge().get(), is(10));
				assertThat(data.getBirthday().get(), is(LocalDate.of(2000, Month.JANUARY, 1)));
				assertThat(data.getMemo(), is(Optional.empty()));
			});
		}
	}

}
