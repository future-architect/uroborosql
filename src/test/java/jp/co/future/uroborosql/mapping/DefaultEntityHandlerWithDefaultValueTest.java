package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.Optional;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;

public class DefaultEntityHandlerWithDefaultValueTest {

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
	public void testInsert1() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithDefaultValue test1 = new TestEntityWithDefaultValue(1L,
						Optional.of("name1"),
						Optional.of(20),
						Optional.of(LocalDate.of(1990, Month.APRIL, 1)),
						Optional.of("memo1"));
				agent.insert(test1);
				TestEntityWithDefaultValue test2 = new TestEntityWithDefaultValue(2L,
						null,
						null,
						null,
						null);
				agent.insert(test2);
				TestEntityWithDefaultValue data = agent.find(TestEntityWithDefaultValue.class, 1).orElse(null);
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
	public void testInsert2() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithDefaultValue test1 = new TestEntityWithDefaultValue(null,
						Optional.of("name1"),
						Optional.of(20),
						Optional.of(LocalDate.of(1990, Month.APRIL, 1)),
						Optional.of("memo1"));
				TestEntityWithDefaultValue result1 = agent.insertAndReturn(test1);
				TestEntityWithDefaultValue test2 = new TestEntityWithDefaultValue(null,
						null,
						null,
						null,
						null);
				TestEntityWithDefaultValue result2 = agent.insertAndReturn(test2);
				TestEntityWithDefaultValue data = agent.find(TestEntityWithDefaultValue.class, result1.getId())
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
