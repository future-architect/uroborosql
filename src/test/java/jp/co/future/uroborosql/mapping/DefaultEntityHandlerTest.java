package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.List;
import java.util.Optional;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.*;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class DefaultEntityHandlerTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:DefaultEntityHandlerTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500),lock_version NUMERIC(4), primary key(id))");

				stmt.execute(
						"drop table if exists test_data_no_key");
				stmt.execute(
						"create table if not exists test_data_no_key( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500))");

				stmt.execute(
						"drop table if exists test_data_multi_key");
				stmt.execute(
						"create table if not exists test_data_multi_key( id NUMERIC(4),key VARCHAR(10),name VARCHAR(10), primary key(id, key))");

			}
		}

		config = DefaultSqlConfig.getConfig(url, user, password);

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.createAgent()) {
			agent.updateWith("delete from test").count();
			agent.updateWith("delete from test_data_no_key").count();
			agent.updateWith("delete from test_data_multi_key").count();
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional.of("memo1"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2), Optional.empty());
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3), Optional.of("memo3"));
				agent.insert(test3);
				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testInsert2() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity2 test1 = new TestEntity2(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				TestEntity2 test2 = new TestEntity2(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2));
				agent.insert(test2);
				TestEntity2 test3 = new TestEntity2(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3));
				agent.insert(test3);
				TestEntity2 data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity2.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity2.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testQuery() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity2 test1 = new TestEntity2(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				TestEntity2 test2 = new TestEntity2(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				TestEntity2 test3 = new TestEntity2(3, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);

				List<TestEntity2> list = agent.query(TestEntity2.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity2.class)
						.param("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	public void testUpdate() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity2 test = new TestEntity2(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntity2 data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	public void testUpdateLockVersionSuccess() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity3 test = new TestEntity3(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntity3 data = agent.find(TestEntity3.class, 1).orElse(null);
				test.setLockVersion(test.getLockVersion() + 1);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test(expected = OptimisticLockException.class)
	public void testUpdateLockVersionError() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity3 test = new TestEntity3(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				test.setLockVersion(1);
				agent.insert(test);

				test.setName("updatename");
				test.setLockVersion(0);
				agent.update(test);
			});
		}
	}

	@Test
	public void testDelete() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestEntity2 test = new TestEntity2(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				agent.delete(test);

				TestEntity2 data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testQueryNothingKey() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestDataNoKeyEntity test1 = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional.of("memo1"));
				agent.insert(test1);
				TestDataNoKeyEntity test2 = new TestDataNoKeyEntity(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2), Optional.empty());
				agent.insert(test2);
				TestDataNoKeyEntity test3 = new TestDataNoKeyEntity(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3), Optional.of("memo3"));
				agent.insert(test3);

				List<TestDataNoKeyEntity> list = agent.query(TestDataNoKeyEntity.class).collect();
				assertThat(list.size(), is(3));
			});
		}
	}

	@Test
	public void testQueryMultiKeyEntity() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TestDataMultiKeyEntity test1 = new TestDataMultiKeyEntity(1, "key1", "name1");
				agent.insert(test1);
				TestDataMultiKeyEntity test2 = new TestDataMultiKeyEntity(1, "key2", "name2");
				agent.insert(test2);
				TestDataMultiKeyEntity test3 = new TestDataMultiKeyEntity(2, "key1", "name3");
				agent.insert(test3);

				List<TestDataMultiKeyEntity> list = agent.query(TestDataMultiKeyEntity.class).collect();
				assertThat(list.size(), is(3));
			});
		}
	}
}
