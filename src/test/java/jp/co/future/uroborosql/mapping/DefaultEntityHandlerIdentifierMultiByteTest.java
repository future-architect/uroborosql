package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManagerImpl;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class DefaultEntityHandlerIdentifierMultiByteTest {

	private static SqlConfig config;

	@Table(name = "日本語table")
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

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:DefaultEntityHandlerBuildSqlTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
				stmt.execute("drop table if exists \"日本語table\"");
				stmt.execute(
						"create table if not exists \"日本語table\"( \"Id\" NUMERIC(4),\"Name\" VARCHAR(10), primary key(\"Id\"))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
		DefaultEntityHandler.clearCache();
		MappingUtils.clearCache();
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from \"日本語table\"").count();
			agent.commit();
		}
	}

	@Test
	void testInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1");
				agent.insert(test1);
				var test2 = new TestEntity(2L, "name2");
				agent.insert(test2);
				var test3 = new TestEntity(3L, "name3");
				agent.insert(test3);
				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testQuery1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1");
				agent.insert(test1);
				var test2 = new TestEntity(2L, "name2");
				agent.insert(test2);
				var test3 = new TestEntity(3L, "name3");
				agent.insert(test3);

				var list = agent.query(TestEntity.class).collect();
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));
				assertThat(list.get(2), is(test3));

				list = agent.query(TestEntity.class)
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
				var test = new TestEntity(1L, "name1");
				agent.insert(test);

				test.setName("updatename");
				agent.update(test);

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("updatename"));
			});
		}
	}

	@Test
	void testDelete1() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = new TestEntity(1L, "name1");
				agent.insert(test);

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));

				agent.delete(test);

				data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	void testBatchInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1");
				var test2 = new TestEntity(2L, "name2");
				var test3 = new TestEntity(3L, "name3");

				var count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

	@Test
	void testBulkInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new TestEntity(1L, "name1");
				var test2 = new TestEntity(2L, "name2");
				var test3 = new TestEntity(3L, "name3");

				var count = agent.inserts(Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

}
