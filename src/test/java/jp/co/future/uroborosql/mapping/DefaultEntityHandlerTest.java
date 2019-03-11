package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.fluent.SqlEntityQuery.Nulls;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

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
				stmt.execute("drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500),lock_version NUMERIC(4), primary key(id))");
				stmt.execute("comment on table test is 'test'");
				stmt.execute("comment on column test.id is 'id'");
				stmt.execute("comment on column test.name is 'name'");
				stmt.execute("comment on column test.age is 'age'");
				stmt.execute("comment on column test.birthday is 'birthday'");
				stmt.execute("comment on column test.memo is 'memo'");

				stmt.execute("drop table if exists test_data_no_key");
				stmt.execute(
						"create table if not exists test_data_no_key( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500))");

				stmt.execute("drop table if exists test_data_multi_key");
				stmt.execute(
						"create table if not exists test_data_multi_key( id NUMERIC(4),key VARCHAR(10),name VARCHAR(10), primary key(id, key))");

				stmt.execute("drop table if exists test_data_lock_version");
				stmt.execute(
						"create table if not exists test_data_lock_version( id NUMERIC(4), name VARCHAR(10), lock_version NUMERIC(10))");
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
			agent.updateWith("delete from test_data_no_key").count();
			agent.updateWith("delete from test_data_multi_key").count();
			agent.updateWith("delete from test_data_lock_version").count();
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2), Optional.empty());
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3), Optional
						.of("memo3"));
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

		try (SqlAgent agent = config.agent()) {
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
	public void testInsert3() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity3 test1 = new TestEntity3(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				TestEntity3 test2 = new TestEntity3(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2));
				agent.insert(test2);
				TestEntity3 test3 = new TestEntity3(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3));
				agent.insert(test3);
				assertThat(agent.find(TestEntity3.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity3.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity3.class, 3).orElse(null), is(test3));
			});
		}
	}

	@Test
	public void testQuery1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1), Optional
						.of("memo2"));
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 22, LocalDate.of(1990, Month.MAY, 1), Optional.empty());
				agent.insert(test3);

				List<TestEntity> list = agent.query(TestEntity.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class)
						.param("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	public void testQuery2() throws Exception {

		try (SqlAgent agent = config.agent()) {
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
	public void testQuery3() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity3 test1 = new TestEntity3(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				TestEntity3 test2 = new TestEntity3(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				TestEntity3 test3 = new TestEntity3(3, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);

				List<TestEntity3> list = agent.query(TestEntity3.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity3.class)
						.param("birthday", LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));
			});
		}
	}

	@Test
	public void testQuery4() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity3 test1 = new TestEntity3(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				TestEntity3 test2 = new TestEntity3(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test2);
				TestEntity3 test3 = new TestEntity3(3, "name3", 22, LocalDate.of(1990, Month.MAY, 1));
				agent.insert(test3);

				long count = agent.query(TestEntity3.class).count();
				assertThat(count, is(3L));
			});
		}
	}

	@Test
	public void testQueryWithCondition() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 22, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo"));
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2", 21, LocalDate.of(1990, Month.MAY, 1),
						Optional.of("memo2"));
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3", 20, LocalDate.of(1990, Month.JUNE, 1), Optional.empty());
				agent.insert(test3);

				// Equal
				List<TestEntity> list = null;
				list = agent.query(TestEntity.class).equal("id", 2).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				// Not Equal
				list = agent.query(TestEntity.class).notEqual("id", 2).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				// Greater Than
				list = agent.query(TestEntity.class).greaterThan("age", 21).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// Greater Equal
				list = agent.query(TestEntity.class).greaterEqual("age", 21).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// Less Than
				list = agent.query(TestEntity.class).lessThan("age", 21).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Greater Equal
				list = agent.query(TestEntity.class).lessEqual("age", 21).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// In (array)
				list = agent.query(TestEntity.class).in("id", 1, 2).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// In (list)
				list = agent.query(TestEntity.class).in("id", Arrays.asList(2, 3)).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// Not In (array)
				list = agent.query(TestEntity.class).notIn("id", 1, 2).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Not In (list)
				list = agent.query(TestEntity.class).notIn("id", Arrays.asList(2, 3)).collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// Like
				list = agent.query(TestEntity.class).like("name", "name3").collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// Like with wildcards (_)
				list = agent.query(TestEntity.class).like("name", "n_me_").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// Like with wildcards (%)
				list = agent.query(TestEntity.class).like("name", "name%").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// startsWith
				list = agent.query(TestEntity.class).startsWith("name", "name").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// startsWith wildcards
				list = agent.query(TestEntity.class).startsWith("name", "%ame").collect();
				assertThat(list.size(), is(0));

				// endsWith
				list = agent.query(TestEntity.class).endsWith("name", "3").collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// endsWith wildcards
				list = agent.query(TestEntity.class).endsWith("name", "%3").collect();
				assertThat(list.size(), is(0));

				// contains
				list = agent.query(TestEntity.class).contains("name", "me").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class).contains("name", "%me_").collect();
				assertThat(list.size(), is(0));

				// Not Like
				list = agent.query(TestEntity.class).notLike("name", "name3").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// Not Like with wildcards (_)
				list = agent.query(TestEntity.class).notLike("name", "name_").collect();
				assertThat(list.size(), is(0));

				// Not Like with wildcards (%)
				list = agent.query(TestEntity.class).notLike("name", "name%").collect();
				assertThat(list.size(), is(0));

				// notStartsWith
				list = agent.query(TestEntity.class).notStartsWith("name", "name").collect();
				assertThat(list.size(), is(0));

				// notStartsWith wildcards
				list = agent.query(TestEntity.class).notStartsWith("name", "%name").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// notEndsWith
				list = agent.query(TestEntity.class).notEndsWith("name", "3").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// notEndsWith wildcards
				list = agent.query(TestEntity.class).notEndsWith("name", "%3").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// notContains
				list = agent.query(TestEntity.class).notContains("name", "2").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test3));

				// notContains wildcards
				list = agent.query(TestEntity.class).notContains("name", "_2").collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				// Between
				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 15), LocalDate.of(1990, Month.MAY, 15))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));
				list = agent.query(TestEntity.class)
						.between("birthday", LocalDate.of(1990, Month.APRIL, 1), LocalDate.of(1990, Month.MAY, 1))
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// is null
				list = agent.query(TestEntity.class).isNull("memo").collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// is not null
				list = agent.query(TestEntity.class).isNotNull("memo").collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// where
				list = agent.query(TestEntity.class)
						.where("BIRTHDAY < /*birthday1*/ or BIRTHDAY > /*birthday2*/")
						.param("birthday1", LocalDate.of(1990, Month.APRIL, 15))
						.param("birthday2", LocalDate.of(1990, Month.MAY, 15))
						.where("AGE < /*age*/")
						.param("age", 21)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test3));

				// order by asc
				list = agent.query(TestEntity.class)
						.asc("age")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by desc
				list = agent.query(TestEntity.class)
						.desc("birthday")
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// order by asc and desc
				list = agent.query(TestEntity.class)
						.asc("age")
						.desc("birthday", Nulls.FIRST)
						.collect();
				assertThat(list.size(), is(3));
				assertThat(list.get(0), is(test3));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test1));

				// limit, offset
				list = agent.query(TestEntity.class)
						.limit(2)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				list = agent.query(TestEntity.class)
						.limit(2)
						.offset(1)
						.collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test2));
				assertThat(list.get(1), is(test3));

				// count
				long count = agent.query(TestEntity.class)
						.asc("age")
						.count();
				assertThat(count, is(3L));
			});
		}
	}

	@Test
	public void testUpdate1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	public void testUpdate2() throws Exception {

		try (SqlAgent agent = config.agent()) {
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
	public void testUpdateNoKey() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestDataNoKeyEntity test = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestDataNoKeyEntity data = agent.find(TestDataNoKeyEntity.class).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	public void testUpdateLockVersionSuccess() throws Exception {

		try (SqlAgent agent = config.agent()) {
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

		try (SqlAgent agent = config.agent()) {
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
	public void testUpdateLockVersionOnly() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityLockVersion test = new TestEntityLockVersion(1L, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntityLockVersion data = agent.find(TestEntityLockVersion.class).orElse(null);
				test.setLockVersion(test.getLockVersion() + 1);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	public void testDelete1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				agent.insert(test);

				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testDelete2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity2 test = new TestEntity2(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test);

				TestEntity2 data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testDeleteNothingKey() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestDataNoKeyEntity test = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test);

				TestDataNoKeyEntity data = agent.find(TestDataNoKeyEntity.class).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestDataNoKeyEntity.class).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testDeleteLockVersionOnly() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityLockVersion test = new TestEntityLockVersion(1L, "name1");
				agent.insert(test);

				TestEntityLockVersion data = agent.find(TestEntityLockVersion.class).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntityLockVersion.class).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testDeleteWithKeys() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				TestEntity test2 = new TestEntity(2, "name2", 30, LocalDate.of(1980, Month.MAY, 1), Optional
						.of("memo2"));
				TestEntity test3 = new TestEntity(3, "name3", 40, LocalDate.of(1970, Month.JUNE, 1), Optional
						.empty());
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				assertThat(agent.delete(TestEntity.class, 1, 3), is(2));

				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
			});
		}
	}

	@Test
	public void testDeleteWithKeysForNothingKey() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestDataNoKeyEntity test1 = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				TestDataNoKeyEntity test2 = new TestDataNoKeyEntity(2, "name2", 30, LocalDate.of(1980, Month.MAY, 1),
						Optional.of("memo2"));
				TestDataNoKeyEntity test3 = new TestDataNoKeyEntity(3, "name3", 40, LocalDate.of(1970, Month.JUNE, 1),
						Optional.empty());
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				assertThat(agent.delete(TestDataNoKeyEntity.class, 1, 3), is(2));

				assertThat(agent.query(TestDataNoKeyEntity.class).equal("id", 1).first().orElse(null), is(nullValue()));
				assertThat(agent.query(TestDataNoKeyEntity.class).equal("id", 2).first().orElse(null), is(test2));
				assertThat(agent.query(TestDataNoKeyEntity.class).equal("id", 3).first().orElse(null), is(nullValue()));
			});
		}
	}

	@Test
	public void testDeleteWithKeyForMultiKey() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestDataMultiKeyEntity test1 = new TestDataMultiKeyEntity(1, "key1", "name1");
				TestDataMultiKeyEntity test2 = new TestDataMultiKeyEntity(1, "key2", "name2");
				TestDataMultiKeyEntity test3 = new TestDataMultiKeyEntity(2, "key1", "name3");
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				try {
					agent.delete(TestDataMultiKeyEntity.class, 1, 2);
					fail();
				} catch (IllegalArgumentException ex) {
					assertThat(ex.getMessage(), is("Entity has multiple keys"));
				} catch (Exception ex) {
					fail(ex.getMessage());
				}
			});
		}
	}

	@Test
	public void testDeleteWithCondition() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
						.of("memo1"));
				TestEntity test2 = new TestEntity(2, "name2", 30, LocalDate.of(1980, Month.MAY, 1), Optional
						.of("memo2"));
				TestEntity test3 = new TestEntity(3, "name3", 40, LocalDate.of(1970, Month.JUNE, 1), Optional
						.empty());
				agent.insert(test1);
				agent.insert(test2);
				agent.insert(test3);

				agent.setSavepoint("sp-equal");
				assertThat(agent.delete(TestEntity.class).equal("name", "name1").count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-equal");

				agent.setSavepoint("sp-notEqual");
				assertThat(agent.delete(TestEntity.class).notEqual("name", "name1").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-notEqual");

				agent.setSavepoint("sp-greaterThan");
				assertThat(agent.delete(TestEntity.class).greaterThan("age", 30).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-greaterThan");

				agent.setSavepoint("sp-lessThan");
				assertThat(agent.delete(TestEntity.class).lessThan("age", 30).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-lessThan");

				agent.setSavepoint("sp-greaterEqual");
				assertThat(agent.delete(TestEntity.class).greaterEqual("age", 30).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-greaterEqual");

				agent.setSavepoint("sp-lessEqual");
				assertThat(agent.delete(TestEntity.class).lessEqual("age", 30).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-lessEqual");

				agent.setSavepoint("sp-in-array");
				assertThat(agent.delete(TestEntity.class).in("id", 1, 3).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-in-array");

				agent.setSavepoint("sp-in-list");
				assertThat(agent.delete(TestEntity.class).in("id", Arrays.asList(1, 3)).count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-in-list");

				agent.setSavepoint("sp-notIn-array");
				assertThat(agent.delete(TestEntity.class).notIn("id", 1, 3).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notIn-array");

				agent.setSavepoint("sp-notIn-list");
				assertThat(agent.delete(TestEntity.class).notIn("id", Arrays.asList(1, 3)).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notIn-list");

				agent.setSavepoint("sp-like");
				assertThat(agent.delete(TestEntity.class).like("name", "name%").count(), is(3));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-like");

				agent.setSavepoint("sp-startsWith");
				assertThat(agent.delete(TestEntity.class).startsWith("name", "name").count(), is(3));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-startsWith");

				agent.setSavepoint("sp-endsWith");
				assertThat(agent.delete(TestEntity.class).endsWith("name", "2").count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-endsWith");

				agent.setSavepoint("sp-contains");
				assertThat(agent.delete(TestEntity.class).contains("name", "name").count(), is(3));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-contains");

				agent.setSavepoint("sp-notLike");
				assertThat(agent.delete(TestEntity.class).notLike("name", "name%").count(), is(0));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notLike");

				agent.setSavepoint("sp-notStartsWith");
				assertThat(agent.delete(TestEntity.class).notStartsWith("name", "name").count(), is(0));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-notStartsWith");

				agent.setSavepoint("sp-notEndsWith");
				assertThat(agent.delete(TestEntity.class).notEndsWith("name", "2").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-notEndsWith");

				agent.setSavepoint("sp-notContains");
				assertThat(agent.delete(TestEntity.class).notContains("name", "2").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-notContains");

				agent.setSavepoint("sp-between");
				assertThat(agent.delete(TestEntity.class)
						.between("birthday", LocalDate.of(1980, Month.MAY, 1), LocalDate.of(1990, Month.APRIL, 1))
						.count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-between");

				agent.setSavepoint("sp-isNull");
				assertThat(agent.delete(TestEntity.class).isNull("memo").count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(test1));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(nullValue()));
				agent.rollback("sp-isNull");

				agent.setSavepoint("sp-isNotNull");
				assertThat(agent.delete(TestEntity.class).isNotNull("memo").count(), is(2));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-isNotNull");

				agent.setSavepoint("sp-where");
				assertThat(agent.delete(TestEntity.class)
						.where("BIRTHDAY > /*birthdayFrom*/ and BIRTHDAY <= /*birthdayTo*/")
						.param("birthdayFrom", LocalDate.of(1980, Month.MAY, 1))
						.param("birthdayTo", LocalDate.of(1990, Month.APRIL, 1)).count(), is(1));
				assertThat(agent.find(TestEntity.class, 1).orElse(null), is(nullValue()));
				assertThat(agent.find(TestEntity.class, 2).orElse(null), is(test2));
				assertThat(agent.find(TestEntity.class, 3).orElse(null), is(test3));
				agent.rollback("sp-where");
			});
		}
	}

	@Test
	public void testBatchInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBatchInsert2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(TestEntityForInserts.class, Stream.of(test1, test2, test3),
						InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBatchInsert3() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(Stream.of(test1, test2, test3), (ctx, cnt, r) -> cnt > 0,
						InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBatchInsert4() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");
				TestEntityForInserts test4 = new TestEntityForInserts(4, "name4", 23,
						LocalDate.of(1990, Month.APRIL, 4),
						"memo4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, cnt, r) -> cnt == 3,
						InsertsType.BATCH);
				assertThat(count, is(4));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityForInserts.class, 4).orElse(null);
				assertThat(data, is(test4));

			});
		}
	}

	@Test
	public void testBatchInsertEmpty() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				int count = agent.inserts(Stream.empty(), InsertsType.BATCH);
				assertThat(count, is(0));

				assertThat(agent.query(TestEntityForInserts.class).collect().size(), is(0));
			});
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test(expected = IllegalArgumentException.class)
	public void testBatchInsertTypeError1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts();

				agent.inserts((Class) TestEntity.class, Stream.of(test1), InsertsType.BATCH);

			});
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test(expected = IllegalArgumentException.class)
	public void testBatchInsertTypeError2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts();

				agent.inserts((Class) int.class, Stream.of(test1), InsertsType.BATCH);

			});
		}
	}

	@Test
	public void testBulkInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBulkInsert2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(TestEntityForInserts.class, Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBulkInsert3() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");

				int count = agent.inserts(Stream.of(test1, test2, test3), (ctx, cnt, r) -> cnt > 0);
				assertThat(count, is(3));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBulkInsert4() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts(1, "name1", 20,
						LocalDate.of(1990, Month.APRIL, 1),
						"memo1");
				TestEntityForInserts test2 = new TestEntityForInserts(2, "name2", 21,
						LocalDate.of(1990, Month.APRIL, 2),
						null);
				TestEntityForInserts test3 = new TestEntityForInserts(3, "name3", 22,
						LocalDate.of(1990, Month.APRIL, 3),
						"memo3");
				TestEntityForInserts test4 = new TestEntityForInserts(4, "name4", 23,
						LocalDate.of(1990, Month.APRIL, 4),
						"memo4");

				int count = agent.inserts(Stream.of(test1, test2, test3, test4), (ctx, cnt, r) -> cnt == 3);
				assertThat(count, is(4));

				TestEntityForInserts data = agent.find(TestEntityForInserts.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntityForInserts.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntityForInserts.class, 3).orElse(null);
				assertThat(data, is(test3));
				data = agent.find(TestEntityForInserts.class, 4).orElse(null);
				assertThat(data, is(test4));

			});
		}
	}

	@Test
	public void testBulkInsertEmpty() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				int count = agent.inserts(Stream.empty());
				assertThat(count, is(0));

				assertThat(agent.query(TestEntityForInserts.class).collect().size(), is(0));
			});
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test(expected = IllegalArgumentException.class)
	public void testBulkInsertTypeError1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts();

				agent.inserts((Class) TestEntity.class, Stream.of(test1));

			});
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test(expected = IllegalArgumentException.class)
	public void testBulkInsertTypeError2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntityForInserts test1 = new TestEntityForInserts();

				agent.inserts((Class) int.class, Stream.of(test1));

			});
		}
	}

	@Test
	public void testQueryNothingKey() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestDataNoKeyEntity test1 = new TestDataNoKeyEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1),
						Optional.of("memo1"));
				agent.insert(test1);
				TestDataNoKeyEntity test2 = new TestDataNoKeyEntity(2, "name2", 21, LocalDate.of(1990, Month.APRIL, 2),
						Optional.empty());
				agent.insert(test2);
				TestDataNoKeyEntity test3 = new TestDataNoKeyEntity(3, "name3", 22, LocalDate.of(1990, Month.APRIL, 3),
						Optional.of("memo3"));
				agent.insert(test3);

				List<TestDataNoKeyEntity> list = agent.query(TestDataNoKeyEntity.class).collect();
				assertThat(list.size(), is(3));
			});
		}
	}

	@Test
	public void testQueryMultiKeyEntity() throws Exception {

		try (SqlAgent agent = config.agent()) {
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

	@Test
	public void testCreateSelectContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createSelectContext(agent, metadata, null, true);

			String sql = ctx.getSql();
			assertThat(sql, containsString("SF.isNotEmpty"));

			try (ResultSet rs = agent.query(ctx)) {
				assertThat(rs.next(), is(true));
			}
		}
	}

	@Test
	public void testCreateSelectContextEmptyNotEqualsNull() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createSelectContext(agent, metadata, null, true);

			String sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			try (ResultSet rs = agent.query(ctx)) {
				assertThat(rs.next(), is(true));
			}

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	public void testCreateInsertContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createInsertContext(agent, metadata, null);

			String sql = ctx.getSql();
			assertThat(sql, containsString("SF.isNotEmpty"));

			ctx.param("id", 1).param("name", "name1").param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1)).param("memo", Optional.of("memo1"));
			assertThat(agent.update(ctx), is(1));
		}
	}

	@Test
	public void testCreateInsertContextEmptyNotEqualsNull() throws Exception {
		try (SqlAgent agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createInsertContext(agent, metadata, null);

			String sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			ctx.param("id", 1).param("name", "name1").param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1)).param("memo", Optional.of("memo1"));
			assertThat(agent.update(ctx), is(1));

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	public void testCreateUpdateContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createUpdateContext(agent, metadata, null, true);

			String sql = ctx.getSql();
			assertThat(sql, containsString("SF.isNotEmpty"));

			ctx.param("id", 1).param("name", "updatename");
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).param("id", 1).first().orElse(null).getName(), is("updatename"));
		}
	}

	@Test
	public void testCreateUpdateContextEmptyStringEqualsNull() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			handler.setEmptyStringEqualsNull(false);
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createUpdateContext(agent, metadata, null, true);

			String sql = ctx.getSql();
			assertThat(sql, not(containsString("SF.isNotEmpty")));

			ctx.param("id", 1).param("name", "updatename");
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).param("id", 1).first().orElse(null).getName(), is("updatename"));

			handler.setEmptyStringEqualsNull(true);
		}
	}

	@Test
	public void testCreateDeleteContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			TestEntity test = new TestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1), Optional
					.of("memo1"));
			agent.insert(test);

			agent.commit();

			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createDeleteContext(agent, metadata, null, true);
			ctx.param("id", 1);
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).param("id", 1).first().orElse(null), is(nullValue()));
		}
	}

	@Test
	public void testAddAndRemovePropertyMapper() throws Exception {
		EntityHandler<?> handler = config.getEntityHandler();

		CustomMapper customMapper = new CustomMapper();
		handler.addPropertyMapper(customMapper);

		Class<?> clazz = handler.getClass();
		Field mapperField = clazz.getDeclaredField("propertyMapperManager");
		mapperField.setAccessible(true);
		Class<?> mapperClazz = mapperField.getType();
		Object mapperObj = mapperField.get(handler);
		Field mappersField = mapperClazz.getDeclaredField("mappers");
		mappersField.setAccessible(true);
		@SuppressWarnings("unchecked")
		List<PropertyMapper<?>> instance = (List<PropertyMapper<?>>) mappersField.get(mapperObj);

		assertThat(instance.contains(customMapper), is(true));

		handler.removePropertyMapper(customMapper);

		assertThat(instance.contains(customMapper), is(false));

	}

	public static class Name {
		private final String s;

		public Name(final String s) {
			this.s = s;
		}
	}

	private static class CustomMapper implements PropertyMapper<Name>, BindParameterMapper<Name> {
		@Override
		public Object toJdbc(final Name original, final Connection connection,
				final BindParameterMapperManager parameterMapperManager) {
			return "-" + original.s.toLowerCase() + "-";
		}

		@Override
		public Name getValue(final JavaType type, final ResultSet rs, final int columnIndex,
				final PropertyMapperManager mapperManager)
				throws SQLException {
			String s = rs.getString(columnIndex);
			return s != null ? new Name(s.toUpperCase().replaceAll("^-", "").replaceAll("-$", "")) : null;
		}
	}

}
