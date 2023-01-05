package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.exception.UroborosqlRuntimeException;
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
			return Objects.hash(id, name);
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
			if (id != other.id || !Objects.equals(name, other.name)) {
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
			return Objects.hash(id, name);
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			var other = (TestEntity1) obj;
			if (id != other.id || !Objects.equals(name, other.name)) {
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
			return Objects.hash(id, name);
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			var other = (TestEntity2) obj;
			if (id != other.id || !Objects.equals(name, other.name)) {
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

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:" + DefaultEntityHandlerWithMultiSchemaTest.class.getSimpleName()
				+ ";DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		conn = DriverManager.getConnection(url, user, password);
		conn.setAutoCommit(false);
		// テーブル作成
		try (var stmt = conn.createStatement()) {
			stmt.execute("create schema SCHEMA1");
			stmt.execute("drop table if exists SCHEMA1.TEST");
			stmt.execute(
					"create table if not exists SCHEMA1.TEST( \"Id\" NUMERIC(4),\"Name\" VARCHAR(10), primary key(\"Id\"))");
			stmt.execute("drop table if exists SCHEMA1.TEST_S1ONLY");
			stmt.execute(
					"create table if not exists SCHEMA1.TEST_S1ONLY( \"Id_S1\" NUMERIC(4),\"Name_S1\" VARCHAR(10), primary key(\"Id_S1\"))");

			stmt.execute("create schema SCHEMA2");
			stmt.execute("drop table if exists SCHEMA2.TEST");
			stmt.execute(
					"create table if not exists SCHEMA2.TEST( \"Id\" NUMERIC(4),\"Name\" VARCHAR(10), primary key(\"Id\"))");
			stmt.execute("drop table if exists SCHEMA2.TEST_S2ONLY");
			stmt.execute(
					"create table if not exists SCHEMA2.TEST_S2ONLY( \"Id_S2\" NUMERIC(4),\"Name_S2\" VARCHAR(10), primary key(\"Id_S2\"))");
		}
		conn.setSchema("SCHEMA1");

		config = UroboroSQL.builder(conn)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
		DefaultEntityHandler.clearCache();
		MappingUtils.clearCache();
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from SCHEMA1.TEST").count();
			agent.updateWith("delete from SCHEMA2.TEST").count();
			agent.commit();
		}
	}

	@Test
	void testInsertWithNoSchema() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1, "name1");
				agent.insert(test1);
				var test2 = new TestEntity(2, "name2");
				agent.insert(test2);
				var test3 = new TestEntity(3, "name3");
				agent.insert(test3);
				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

				var schema1Data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(schema1Data.getId(), is(1L));

				var schema2Data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(schema2Data, is(nullValue()));
			});
		}
	}

	@Test
	void testInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity1(1L, "name1");
				agent.insert(test1);
				var test2 = new TestEntity1(2L, "name2");
				agent.insert(test2);
				var test3 = new TestEntity1(3L, "name3");
				agent.insert(test3);
				var data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity1.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity1.class, 3).orElse(null);
				assertThat(data, is(test3));

				var schema2Data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(schema2Data, is(nullValue()));
			});
		}
	}

	@Test
	void testInsertSchema2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity2(1L, "name1");
				agent.insert(test1);
				var test2 = new TestEntity2(2L, "name2");
				agent.insert(test2);
				var test3 = new TestEntity2(3L, "name3");
				agent.insert(test3);
				var data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity2.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity2.class, 3).orElse(null);
				assertThat(data, is(test3));

				var schema1Data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(schema1Data, is(nullValue()));
			});
		}
	}

	@Test
	void testQuery1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity1(1L, "name1");
				agent.insert(test1);
				var test2 = new TestEntity1(2L, "name2");
				agent.insert(test2);
				var test3 = new TestEntity1(3L, "name3");
				agent.insert(test3);

				var list = agent.query(TestEntity1.class).collect();
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
	void testUpdate1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity1(1L, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	void testUpdateSchema2() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity1(1L, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));

				var schema2Data = agent.find(TestEntity2.class, 1).orElse(null);
				assertThat(schema2Data, is(nullValue()));
			});
		}
	}

	@Test
	void testDelete1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity1(1L, "name1");
				agent.insert(test);

				var data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testBatchInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity1(1L, "name1");
				var test2 = new TestEntity1(2L, "name2");
				var test3 = new TestEntity1(3L, "name3");

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity1.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity1.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBulkInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity1(1L, "name1");
				var test2 = new TestEntity1(2L, "name2");
				var test3 = new TestEntity1(3L, "name3");

				var count = agent.inserts(Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				var data = agent.find(TestEntity1.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity1.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity1.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testCreateTableEntityMetadata_existCurrentSchema() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					agent.getConnection().setSchema("SCHEMA1");
					var metadata = TableMetadata.createTableEntityMetadata(agent,
							new jp.co.future.uroborosql.mapping.Table() {

								@Override
								public String getSchema() {
									return null;
								}

								@Override
								public String getName() {
									return "TEST_S1ONLY";
								}
							});
					assertThat(metadata.getTableName(), is("TEST_S1ONLY"));
					assertThat(metadata.getSchema(), is("SCHEMA1"));
					assertThat(metadata.getKeyColumns().size(), is(1));
					assertThat(metadata.getKeyColumns().get(0).getColumnName(), is("Id_S1"));
				} catch (SQLException e) {
					throw new UroborosqlRuntimeException(e);
				}
			});
		}
	}

	@Test
	void testCreateTableEntityMetadata_notExistCurrentSchema() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					agent.getConnection().setSchema("SCHEMA2");
					var metadata = TableMetadata.createTableEntityMetadata(agent,
							new jp.co.future.uroborosql.mapping.Table() {

								@Override
								public String getSchema() {
									return null;
								}

								@Override
								public String getName() {
									return "TEST_S1ONLY";
								}
							});
					assertThat(metadata.getTableName(), is("TEST_S1ONLY"));
					assertThat(metadata.getSchema(), is("SCHEMA1"));
					assertThat(metadata.getKeyColumns().size(), is(1));
					assertThat(metadata.getKeyColumns().get(0).getColumnName(), is("Id_S1"));
				} catch (SQLException e) {
					throw new UroborosqlRuntimeException(e);
				}
			});
		}
	}

	@Test
	void testCreateTableEntityMetadata_withSchema() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					agent.getConnection().setSchema("SCHEMA2");
					var metadata = TableMetadata.createTableEntityMetadata(agent,
							new jp.co.future.uroborosql.mapping.Table() {

								@Override
								public String getSchema() {
									return "SCHEMA1";
								}

								@Override
								public String getName() {
									return "TEST_S1ONLY";
								}
							});
					assertThat(metadata.getTableName(), is("TEST_S1ONLY"));
					assertThat(metadata.getSchema(), is("SCHEMA1"));
					assertThat(metadata.getKeyColumns().size(), is(1));
					assertThat(metadata.getKeyColumns().get(0).getColumnName(), is("Id_S1"));
				} catch (SQLException e) {
					throw new UroborosqlRuntimeException(e);
				}
			});
		}
	}

	@Test
	void testCreateTableEntityMetadata_withWidlcard() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				try {
					agent.getConnection().setSchema("SCHEMA2");
					var metadata = TableMetadata.createTableEntityMetadata(agent,
							new jp.co.future.uroborosql.mapping.Table() {

								@Override
								public String getSchema() {
									return "SCHEMA%";
								}

								@Override
								public String getName() {
									return "TEST_S1ONLY";
								}
							});
					assertThat(metadata.getTableName(), is("TEST_S1ONLY"));
					assertThat(metadata.getSchema(), is("SCHEMA1"));
					assertThat(metadata.getKeyColumns().size(), is(1));
					assertThat(metadata.getKeyColumns().get(0).getColumnName(), is("Id_S1"));
				} catch (SQLException e) {
					throw new UroborosqlRuntimeException(e);
				}
			});
		}
	}
}
