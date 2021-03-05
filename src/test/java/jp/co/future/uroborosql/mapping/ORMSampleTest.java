package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.annotations.Domain;
import jp.co.future.uroborosql.mapping.annotations.Table;

/**
 * Mapperのサンプル実装
 *
 * @author ota
 */
public class ORMSampleTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:ORMSampleTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500), primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password).build();

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("delete from test").count();

				// 準備
				for (int i = 0; i < 24; i++) {
					TestEntity test = new TestEntity();
					test.setId(i + 1);
					test.setName("name" + (i + 1));
					test.setAge(20 + i);
					test.setBirthday(LocalDate.of(1990, Month.of(i % 12 + 1), 1));
					test.setMemo(Optional.of("memo text"));
					agent.insert(test);
				}
			});
			agent.commit();
		}
	}

	public static class TestEntity {
		private long id;
		private String name;
		private int age;
		private LocalDate birthday;
		private Optional<String> memo;

		public TestEntity() {
		}

		public String getName() {
			return name;
		}

		/* 他のgetterは省略 */

		public void setId(final long id) {
			this.id = id;
		}

		public void setName(final String name) {
			this.name = name;
		}

		public void setAge(final int age) {
			this.age = age;
		}

		public void setBirthday(final LocalDate birthday) {
			this.birthday = birthday;
		}

		public void setMemo(final Optional<String> memo) {
			this.memo = memo;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + age;
			result = prime * result + (birthday == null ? 0 : birthday.hashCode());
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (memo == null ? 0 : memo.hashCode());
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
			if (age != other.age) {
				return false;
			}
			if (birthday == null) {
				if (other.birthday != null) {
					return false;
				}
			} else if (!birthday.equals(other.birthday)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (memo == null) {
				if (other.memo != null) {
					return false;
				}
			} else if (!memo.equals(other.memo)) {
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
			return "TestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", memo="
					+ memo + "]";
		}

	}

	@Test
	public void testFind() throws Exception {

		try (SqlAgent agent = config.agent()) {

			// KEYを指定して取得
			TestEntity data = agent.find(TestEntity.class, /* KEY */2).orElse(null);
			assertThat(data.getName(), is("name2"));

		}
	}

	@Test
	public void testQuery() throws Exception {

		try (SqlAgent agent = config.agent()) {

			// 条件なし
			List<TestEntity> list = agent.query(TestEntity.class).collect();
			assertThat(list.size(), is(24));

			// 条件あり
			list = agent.query(TestEntity.class)
					.equal("birthday", LocalDate.of(1990, Month.MAY, 1))
					.collect();
			assertThat(list.size(), is(2));
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				// INSERT
				TestEntity test = new TestEntity();
				test.setId(100);
				test.setName("name100");
				test.setAge(20);
				test.setBirthday(LocalDate.of(1990, Month.APRIL, 1));
				test.setMemo(Optional.of("memo text"));
				agent.insert(test);

				// check
				TestEntity data = agent.find(TestEntity.class, 100).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testUpdate() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = agent.find(TestEntity.class, 1).orElse(null);

				// UPDATE
				test.setName("update!!");
				agent.update(test);

				// check
				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("update!!"));
			});
		}
	}

	@Test
	public void testDelete() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(test, is(not(nullValue())));

				// DELETE
				agent.delete(test);

				// check
				TestEntity data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	// Doaminを定義
	@Domain(valueType = String.class, toJdbcMethod = "getName")
	public static class NameDomain {
		private final String name;

		public NameDomain(final String name) {
			this.name = name;
		}

		public String getName() {
			return name;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
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
			NameDomain other = (NameDomain) obj;
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
			return "NameDomain [name=" + name + "]";
		}

	}

	@Table(name = "TEST")
	public static class DomainTestEntity {
		private long id;
		// 定義したDoaminを利用
		private NameDomain name;

		public DomainTestEntity() {
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
			DomainTestEntity other = (DomainTestEntity) obj;
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
			return "DomainTestEntity [id=" + id + ", name=" + name + "]";
		}

	}

	@Test
	public void testDomain() throws Exception {
		// Domainを定義したクラスを扱う
		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				// INSERT
				DomainTestEntity test = new DomainTestEntity();
				test.id = 100;
				test.name = new NameDomain("name1");
				agent.insert(test);

				// SELECT
				DomainTestEntity data = agent.find(DomainTestEntity.class, 100).orElse(null);
				assertThat(data, is(test));
			});
		}
	}
}
