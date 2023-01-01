package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.util.Objects;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgentProviderImpl;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.utils.CaseFormat;

public class DefaultEntityHandlerIdentifierPascalCaseTest {

	private static SqlConfig config;

	@Table(name = "PascalTable")
	public static class TestEntity {
		private long id;
		private String lastName;
		private String firstName;

		public TestEntity() {
		}

		public TestEntity(final long id, final String lastName, final String firstName) {
			this.id = id;
			this.lastName = lastName;
			this.firstName = firstName;
		}

		public long getId() {
			return id;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public String getLastName() {
			return lastName;
		}

		public void setLastName(final String lastName) {
			this.lastName = lastName;
		}

		public String getFirstName() {
			return firstName;
		}

		public void setFirstName(final String firstName) {
			this.firstName = firstName;
		}

		@Override
		public int hashCode() {
			return Objects.hash(firstName, id, lastName);
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			var other = (TestEntity) obj;
			if (!Objects.equals(firstName, other.firstName) || (id != other.id) || !Objects.equals(lastName, other.lastName)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntity [id=" + id + ", lastName=" + lastName + ", firstName=" + firstName + "]";
		}

	}

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:" + DefaultEntityHandlerIdentifierPascalCaseTest.class.getSimpleName()
				+ ";DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
				stmt.execute("drop table if exists \"PascalTable\"");
				stmt.execute(
						"create table if not exists \"PascalTable\"( \"Id\" NUMERIC(4), \"LastName\" VARCHAR(10), \"FirstName\" VARCHAR(10), primary key(\"Id\"))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlAgentProvider(new SqlAgentProviderImpl().setDefaultMapKeyCaseFormat(CaseFormat.CAMEL_CASE))
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from \"PascalTable\"").count();
			agent.commit();
		}
	}

	//TODO SNAKE_CASE以外のカラム名に対するEntityマッピングについてはMappingルールの全体見直しと合わせて実施する
	//	@Test
	//	public void testInsert() throws Exception {
	//
	//		try (SqlAgent agent = config.agent()) {
	//			agent.required(() -> {
	//				TestEntity test1 = new TestEntity(1L, "lastName1", "firstName1");
	//				agent.insert(test1);
	//				TestEntity test2 = new TestEntity(2L, "lastName2", "firstName2");
	//				agent.insert(test2);
	//				TestEntity test3 = new TestEntity(3L, "lastName3", "firstName3");
	//				agent.insert(test3);
	//				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
	//				assertThat(data, is(test1));
	//				data = agent.find(TestEntity.class, 2).orElse(null);
	//				assertThat(data, is(test2));
	//				data = agent.find(TestEntity.class, 3).orElse(null);
	//				assertThat(data, is(test3));
	//
	//			});
	//		}
	//	}

	@Test
	void testSqlQuery() throws Exception {

		try (var agent = config.agent()) {
			agent.updateWith(
					"insert into \"PascalTable\" (\"Id\", \"LastName\", \"FirstName\") values (/*id*/1, /*lastName*/'', /*firstName*/'')")
					.param("id", 1)
					.param("lastName", "lastName1")
					.param("firstName", "firstName1")
					.count();

			var entity = agent.queryWith("select * from \"PascalTable\"").first(TestEntity.class);
			assertThat(entity.getId(), is(1L));
			assertThat(entity.getLastName(), is("lastName1"));
			assertThat(entity.getFirstName(), is("firstName1"));
		}
	}

	//TODO SNAKE_CASE以外のカラム名に対するEntityマッピングについてはMappingルールの全体見直しと合わせて実施する
	//	@Test
	//	public void testQuery1() throws Exception {
	//
	//		try (SqlAgent agent = config.agent()) {
	//			agent.required(() -> {
	//				TestEntity test1 = new TestEntity(1L, "lastName1", "firstName1");
	//				agent.insert(test1);
	//				TestEntity test2 = new TestEntity(2L, "lastName2", "firstName2");
	//				agent.insert(test2);
	//				TestEntity test3 = new TestEntity(3L, "lastName3", "firstName3");
	//				agent.insert(test3);
	//
	//				List<TestEntity> list = agent.query(TestEntity.class).collect();
	//				assertThat(list.get(0), is(test1));
	//				assertThat(list.get(1), is(test2));
	//				assertThat(list.get(2), is(test3));
	//
	//				list = agent.query(TestEntity.class)
	//						.equal("name", "name2")
	//						.collect();
	//				assertThat(list.get(0), is(test2));
	//			});
	//		}
	//	}
	//
	//	@Test
	//	public void testUpdate1() throws Exception {
	//
	//		try (SqlAgent agent = config.agent()) {
	//			agent.required(() -> {
	//				TestEntity test = new TestEntity(1L, "lastName1", "firstName1");
	//				agent.insert(test);
	//
	//				test.setLastName("updatename");
	//				agent.update(test);
	//
	//				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
	//				assertThat(data, is(test));
	//				assertThat(data.getLastName(), is("updatename"));
	//			});
	//		}
	//	}
	//
	//	@Test
	//	public void testDelete1() throws Exception {
	//
	//		try (SqlAgent agent = config.agent()) {
	//			agent.required(() -> {
	//				TestEntity test = new TestEntity(1L, "lastName1", "firstName1");
	//				agent.insert(test);
	//
	//				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
	//				assertThat(data, is(test));
	//
	//				agent.delete(test);
	//
	//				data = agent.find(TestEntity.class, 1).orElse(null);
	//				assertThat(data, is(nullValue()));
	//			});
	//		}
	//	}
	//
	//	@Test
	//	public void testBatchInsert() throws Exception {
	//
	//		try (SqlAgent agent = config.agent()) {
	//			agent.required(() -> {
	//				TestEntity test1 = new TestEntity(1L, "lastName1", "firstName1");
	//				TestEntity test2 = new TestEntity(2L, "lastName2", "firstName2");
	//				TestEntity test3 = new TestEntity(3L, "lastName3", "firstName3");
	//
	//				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
	//				assertThat(count, is(3));
	//
	//				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
	//				assertThat(data, is(test1));
	//				data = agent.find(TestEntity.class, 2).orElse(null);
	//				assertThat(data, is(test2));
	//				data = agent.find(TestEntity.class, 3).orElse(null);
	//				assertThat(data, is(test3));
	//
	//			});
	//		}
	//	}
	//
	//	@Test
	//	public void testBulkInsert() throws Exception {
	//
	//		try (SqlAgent agent = config.agent()) {
	//			agent.required(() -> {
	//				TestEntity test1 = new TestEntity(1L, "lastName1", "firstName1");
	//				TestEntity test2 = new TestEntity(2L, "lastName2", "firstName2");
	//				TestEntity test3 = new TestEntity(3L, "lastName3", "firstName3");
	//
	//				int count = agent.inserts(Stream.of(test1, test2, test3));
	//				assertThat(count, is(3));
	//
	//				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
	//				assertThat(data, is(test1));
	//				data = agent.find(TestEntity.class, 2).orElse(null);
	//				assertThat(data, is(test2));
	//				data = agent.find(TestEntity.class, 3).orElse(null);
	//				assertThat(data, is(test3));
	//
	//			});
	//		}
	//	}
	//
}
