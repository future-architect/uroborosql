package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

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
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;

public class SequenceGeneratedKeysTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:SequenceGeneratedKeysTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(5),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500),lock_version NUMERIC(4), primary key(id))");
				stmt.execute("create sequence if not exists test_id_seq");
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
			agent.updateWith("drop sequence if exists test_id_seq").count();
			agent.updateWith("create sequence if not exists test_id_seq").count();
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithSeq test1 = new TestEntityWithSeq("name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test1);
				assertThat(test1.getId(), is(1L));

				TestEntityWithSeq test2 = new TestEntityWithSeq("name2", 21, LocalDate.of(1990, Month.APRIL, 2),
						Optional.empty());
				agent.insert(test2);
				assertThat(test2.getId(), is(2L));

				TestEntityWithSeq test3 = new TestEntityWithSeq("name3", 22, LocalDate.of(1990, Month.APRIL, 3),
						Optional.of("memo3"));
				agent.insert(test3);
				assertThat(test3.getId(), is(3L));

				TestEntityWithSeq data = agent.find(TestEntityWithSeq.class, test1.getId()).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityWithSeq.class, test2.getId()).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityWithSeq.class, test3.getId()).orElse(null);
				assertThat(data, is(test3));
			});
		}
	}

	@Test(expected = UroborosqlRuntimeException.class)
	public void testEntityNotSequenceGenerator() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityWithSeqError test1 = new TestEntityWithSeqError("name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test1);
			});
		}
	}

}
