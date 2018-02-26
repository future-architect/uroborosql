package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.List;
import java.util.Optional;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.exception.OptimisticLockException;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapper;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

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
				stmt.execute("drop table if exists test");
				stmt.execute("create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500),lock_version NUMERIC(4), primary key(id))");
				stmt.execute("comment on table test is 'test'");
				stmt.execute("comment on column test.id is 'id'");
				stmt.execute("comment on column test.name is 'name'");
				stmt.execute("comment on column test.age is 'age'");
				stmt.execute("comment on column test.birthday is 'birthday'");
				stmt.execute("comment on column test.memo is 'memo'");

				stmt.execute("drop table if exists test_data_no_key");
				stmt.execute("create table if not exists test_data_no_key( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500))");

				stmt.execute("drop table if exists test_data_multi_key");
				stmt.execute("create table if not exists test_data_multi_key( id NUMERIC(4),key VARCHAR(10),name VARCHAR(10), primary key(id, key))");

				stmt.execute("drop table if exists test_data_lock_version");
				stmt.execute("create table if not exists test_data_lock_version( id NUMERIC(4), name VARCHAR(10), lock_version NUMERIC(10))");
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
			SqlContext ctx = handler.createSelectContext(agent, metadata, null);
			try (ResultSet rs = agent.query(ctx)) {
				assertThat(rs.next(), is(true));
			}
		}
	}

	@Test
	public void testCreateInsertContext() throws Exception {
		try (SqlAgent agent = config.agent()) {
			EntityHandler<?> handler = config.getEntityHandler();
			TableMetadata metadata = TableMetadata.createTableEntityMetadata(agent,
					MappingUtils.getTable(TestEntity.class));
			SqlContext ctx = handler.createInsertContext(agent, metadata, null);
			ctx.param("id", 1).param("name", "name1").param("age", 20)
					.param("birthday", LocalDate.of(1990, Month.APRIL, 1)).param("memo", Optional.of("memo1"));
			assertThat(agent.update(ctx), is(1));
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
			SqlContext ctx = handler.createUpdateContext(agent, metadata, null);
			ctx.param("id", 1).param("name", "updatename");
			assertThat(agent.update(ctx), is(1));
			assertThat(agent.query(TestEntity.class).param("id", 1).first().orElse(null).getName(), is("updatename"));
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
			SqlContext ctx = handler.createDeleteContext(agent, metadata, null);
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
