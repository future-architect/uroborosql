package jp.co.future.uroborosql.mapping;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.DriverManager;
import java.time.LocalDate;
import java.time.Month;
import java.util.Objects;
import java.util.Optional;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.AuditLogEventSubscriber;
import jp.co.future.uroborosql.mapping.annotations.Domain;
import jp.co.future.uroborosql.mapping.annotations.Table;

/**
 * Mapperのサンプル実装
 *
 * @author ota
 */
public class ORMSampleTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:ORMSampleTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( id NUMERIC(4),name VARCHAR(10),age NUMERIC(5),birthday DATE,memo VARCHAR(500), primary key(id))");
			}
		}

		config = UroboroSQL.builder(url, user, password).build();

		var sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogEventSubscriber());
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.required(() -> {
				agent.updateWith("delete from test").count();

				// 準備
				for (var i = 0; i < 24; i++) {
					var test = new TestEntity();
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
			return Objects.hash(age, birthday, id, memo, name);
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
			if (age != other.age || !Objects.equals(birthday, other.birthday) || id != other.id
					|| !Objects.equals(memo, other.memo)) {
				return false;
			}
			if (!Objects.equals(name, other.name)) {
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
	void testFind() throws Exception {

		try (var agent = config.agent()) {

			// KEYを指定して取得
			var data = agent.find(TestEntity.class, /* KEY */2).orElse(null);
			assertThat(data.getName(), is("name2"));

		}
	}

	@Test
	void testQuery() throws Exception {

		try (var agent = config.agent()) {

			// 条件なし
			var list = agent.query(TestEntity.class).collect();
			assertThat(list.size(), is(24));

			// 条件あり
			list = agent.query(TestEntity.class)
					.equal("birthday", LocalDate.of(1990, Month.MAY, 1))
					.collect();
			assertThat(list.size(), is(2));
		}
	}

	@Test
	void testInsert() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				// INSERT
				var test = new TestEntity();
				test.setId(100);
				test.setName("name100");
				test.setAge(20);
				test.setBirthday(LocalDate.of(1990, Month.APRIL, 1));
				test.setMemo(Optional.of("memo text"));
				agent.insert(test);

				// check
				var data = agent.find(TestEntity.class, 100).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	void testUpdate() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = agent.find(TestEntity.class, 1).orElse(null);

				// UPDATE
				test.setName("update!!");
				agent.update(test);

				// check
				var data = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(data, is(test));
				assertThat(data.getName(), is("update!!"));
			});
		}
	}

	@Test
	void testDelete() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test = agent.find(TestEntity.class, 1).orElse(null);
				assertThat(test, is(not(nullValue())));

				// DELETE
				agent.delete(test);

				// check
				var data = agent.find(TestEntity.class, 1).orElse(null);
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
			return Objects.hash(name);
		}

		@Override
		public boolean equals(final Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null || getClass() != obj.getClass()) {
				return false;
			}
			var other = (NameDomain) obj;
			if (!Objects.equals(name, other.name)) {
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
			var other = (DomainTestEntity) obj;
			if (id != other.id || !Objects.equals(name, other.name)) {
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
	void testDomain() throws Exception {
		// Domainを定義したクラスを扱う
		try (var agent = config.agent()) {
			agent.required(() -> {
				// INSERT
				var test = new DomainTestEntity();
				test.id = 100;
				test.name = new NameDomain("name1");
				agent.insert(test);

				// SELECT
				var data = agent.find(DomainTestEntity.class, 100).orElse(null);
				assertThat(data, is(test));
			});
		}
	}
}
