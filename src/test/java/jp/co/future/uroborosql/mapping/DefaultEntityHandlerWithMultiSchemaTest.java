package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.List;
import java.util.stream.Stream;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class DefaultEntityHandlerWithMultiSchemaTest {

	private static SqlConfig config;

	@Table(name = "TEST")
	public static class TestEntity {
		private long id;
		private String name;

		public TestEntity() {
		}

		public TestEntity(final long id, final String name) {
			this.id = id;
			this.name = name;
		}

		public long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntity other = (TestEntity) obj;
			if (id != other.id) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntity [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST", schema = "SCHEMA1")
	public static class TestEntity1 {
		private long id;
		private String name;

		public TestEntity1() {
		}

		public TestEntity1(final long id, final String name) {
			this.id = id;
			this.name = name;
		}

		public long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntity1 other = (TestEntity1) obj;
			if (id != other.id) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntity1 [id=" + id + ", name=" + name + "]";
		}

	}

	@Table(name = "TEST", schema = "SCHEMA2")
	public static class TestEntity2 {
		private long id;
		private String name;

		public TestEntity2() {
		}

		public TestEntity2(final long id, final String name) {
			this.id = id;
			this.name = name;
		}

		public long getId() {
			return this.id;
		}

		public String getName() {
			return this.name;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (name == null ? 0 : name.hashCode());
			return result;
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (getClass() != obj.getClass()) {
				return false;
			}
			TestEntity2 other = (TestEntity2) obj;
			if (id != other.id) {
				return false;
			}
			if (name == null) {
				if (other.name != null) {
					return false;
				}
			} else if (!name.equals(other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntity2 [id=" + id + ", name=" + name + "]";
		}

	}

	private static Connection conn;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:" + DefaultEntityHandlerWithMultiSchemaTest.class.getSimpleName()
				+ ";DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		conn = DriverManager.getConnection(url, user, password);
		conn.setAutoCommit(false);
		// テーブル作成
		try (Statement stmt = conn.createStatement()) {
			stmt.execute("create schema SCHEMA1");
			stmt.execute("drop table if exists SCHEMA1.TEST");
			stmt.execute(
					"create table if not exists SCHEMA1.TEST( \"Id\" NUMERIC(4),\"Name\" VARCHAR(10), primary key(\"Id\"))");

			stmt.execute("create schema SCHEMA2");
			stmt.execute("drop table if exists SCHEMA2.TEST");
			stmt.execute(
					"create table if not exists SCHEMA2.TEST( \"Id\" NUMERIC(4),\"Name\" VARCHAR(10), primary key(\"Id\"))");
		}
		conn.setSchema("SCHEMA1");

		config = UroboroSQL.builder(conn)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from SCHEMA1.TEST").count();
			agent.updateWith("delete from SCHEMA2.TEST").count();
			agent.commit();
		}
	}

	@Test
	public void testInsertWithNoSchema() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1");
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2");
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3");
				agent.insert(test3);
				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

				TestEntity1 schema1Data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(schema1Data.getId(), is(1L));

				TestEntity2 schema2Data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(schema2Data, is(nullValue()));
			});
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test1 = new TestEntity1(1, "name1");
				agent.insert(test1);
				TestEntity1 test2 = new TestEntity1(2, "name2");
				agent.insert(test2);
				TestEntity1 test3 = new TestEntity1(3, "name3");
				agent.insert(test3);
				TestEntity1 data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity1.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity1.class, 3).orElse(null);
				assertThat(data, is(test3));

				TestEntity2 schema2Data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(schema2Data, is(nullValue()));
			});
		}
	}

	@Test
	public void testInsertSchema2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity2 test1 = new TestEntity2(1, "name1");
				agent.insert(test1);
				TestEntity2 test2 = new TestEntity2(2, "name2");
				agent.insert(test2);
				TestEntity2 test3 = new TestEntity2(3, "name3");
				agent.insert(test3);
				TestEntity2 data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity2.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity2.class, 3).orElse(null);
				assertThat(data, is(test3));

				TestEntity1 schema1Data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(schema1Data, is(nullValue()));
			});
		}
	}

	@Test
	public void testQuery1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test1 = new TestEntity1(1, "name1");
				agent.insert(test1);
				TestEntity1 test2 = new TestEntity1(2, "name2");
				agent.insert(test2);
				TestEntity1 test3 = new TestEntity1(3, "name3");
				agent.insert(test3);

				List<TestEntity1> list = agent.query(TestEntity1.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity1.class)
						.equal("name", "name2")
						.collect();
				assertThat(list.get(0), is(test2));
			});
		}
	}

	@Test
	public void testUpdate1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test = new TestEntity1(1, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntity1 data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	public void testUpdateSchema2() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test = new TestEntity1(1, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				TestEntity1 data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));

				TestEntity2 schema2Data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(schema2Data, is(nullValue()));
			});
		}
	}

	@Test
	public void testDelete1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test = new TestEntity1(1, "name1");
				agent.insert(test);

				TestEntity1 data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test1 = new TestEntity1(1, "name1");
				TestEntity1 test2 = new TestEntity1(2, "name2");
				TestEntity1 test3 = new TestEntity1(3, "name3");

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				TestEntity1 data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity1.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity1.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	public void testBulkInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity1 test1 = new TestEntity1(1, "name1");
				TestEntity1 test2 = new TestEntity1(2, "name2");
				TestEntity1 test3 = new TestEntity1(3, "name3");

				int count = agent.inserts(Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				TestEntity1 data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity1.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity1.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

}
