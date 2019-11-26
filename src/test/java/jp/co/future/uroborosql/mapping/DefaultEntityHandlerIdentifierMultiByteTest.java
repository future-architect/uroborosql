package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.util.List;
import java.util.stream.Stream;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
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
			return HashCodeBuilder.reflectionHashCode(this, true);
		}

		@Override
		public boolean equals(final Object obj) {
			return EqualsBuilder.reflectionEquals(this, obj, true);
		}

		@Override
		public String toString() {
			return ToStringBuilder.reflectionToString(this);
		}
	}

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:DefaultEntityHandlerBuildSqlTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists \"日本語table\"");
				stmt.execute(
						"create table if not exists \"日本語table\"( \"Id\" NUMERIC(4),\"Name\" VARCHAR(10), primary key(\"Id\"))");
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setSqlFilterManager(new SqlFilterManagerImpl().addSqlFilter(new AuditLogSqlFilter()))
				.build();
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from \"日本語table\"").count();
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {

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

			});
		}
	}

	@Test
	public void testQuery1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1");
				agent.insert(test1);
				TestEntity test2 = new TestEntity(2, "name2");
				agent.insert(test2);
				TestEntity test3 = new TestEntity(3, "name3");
				agent.insert(test3);

				List<TestEntity> list = agent.query(TestEntity.class).collect();
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
	public void testUpdate1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(1, "name1");
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
	public void testDelete1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(1, "name1");
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
	public void testBatchInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1");
				TestEntity test2 = new TestEntity(2, "name2");
				TestEntity test3 = new TestEntity(3, "name3");

				int count = agent.inserts(Stream.of(test1, test2, test3), InsertsType.BATCH);
				assertThat(count, is(3));

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
	public void testBulkInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(1, "name1");
				TestEntity test2 = new TestEntity(2, "name2");
				TestEntity test3 = new TestEntity(3, "name3");

				int count = agent.inserts(Stream.of(test1, test2, test3));
				assertThat(count, is(3));

				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(TestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(TestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

			});
		}
	}

}
