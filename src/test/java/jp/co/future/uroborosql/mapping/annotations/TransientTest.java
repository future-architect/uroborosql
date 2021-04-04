package jp.co.future.uroborosql.mapping.annotations;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.sql.DriverManager;
import java.time.LocalDate;
import java.time.Month;
import java.util.Objects;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;

public class TransientTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:TransientTest;DB_CLOSE_DELAY=-1";
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
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (var agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Table(name = "TEST")
	public static class TestEntity {
		private long id;
		private String name;
		private int age;
		private LocalDate birthday;
		private static String ignore;
		private final int finalInt = 10;

		public TestEntity() {
		}

		public TestEntity(final long id, final String name, final int age, final LocalDate birthday) {
			this.id = id;
			this.name = name;
			this.age = age;
			this.birthday = birthday;
			TestEntity.ignore = "ignore";
		}

		public long getId() {
			return id;
		}

		public void setId(final long id) {
			this.id = id;
		}

		public String getName() {
			return name;
		}

		public void setName(final String name) {
			this.name = name;
		}

		public int getAge() {
			return age;
		}

		public void setAge(final int age) {
			this.age = age;
		}

		public LocalDate getBirthday() {
			return birthday;
		}

		public void setBirthday(final LocalDate birthday) {
			this.birthday = birthday;
		}

		public static String getIgnore() {
			return ignore;
		}

		public static void setIgnore(final String ignore) {
			TestEntity.ignore = ignore;
		}

		public int getFinalInt() {
			return finalInt;
		}

		@Override
		public int hashCode() {
			return Objects.hash(age, birthday, finalInt, id, name);
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
			var other = (TestEntity) obj;
			if (age != other.age) {
				return false;
			}
			if (!Objects.equals(birthday, other.birthday)) {
				return false;
			}
			if (finalInt != other.finalInt) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(name, other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", finalInt="
					+ finalInt + "]";
		}
	}

	@SuppressWarnings("unused")
	@Table(name = "TEST")
	public static class TransientAnnoTestEntity {
		private long id;
		@Transient
		private String name;
		private int age;
		@Transient(insert = true, update = true)
		private LocalDate birthday;
		private static String ignore;
		private final int finalInt = 10;

		public TransientAnnoTestEntity() {
		}

		public TransientAnnoTestEntity(final long id, final String name, final int age, final LocalDate birthday) {
			this.id = id;
			this.name = name;
			this.age = age;
			this.birthday = birthday;
			TransientAnnoTestEntity.ignore = "ignore";
		}

		@Override
		public int hashCode() {
			return Objects.hash(age, birthday, finalInt, id, name);
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
			var other = (TransientAnnoTestEntity) obj;
			if (age != other.age) {
				return false;
			}
			if (!Objects.equals(birthday, other.birthday)) {
				return false;
			}
			if (finalInt != other.finalInt) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(name, other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TransientAnnoTestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday
					+ ", finalInt=" + finalInt + "]";
		}
	}

	@Table(name = "TEST")
	public static class TransientAnnoInsTestEntity {
		private long id;
		@Transient
		private String name;
		private int age;
		@Transient(insert = false, update = true)
		private LocalDate birthday;

		public TransientAnnoInsTestEntity() {
		}

		public TransientAnnoInsTestEntity(final long id, final String name, final int age, final LocalDate birthday) {
			this.id = id;
			this.name = name;
			this.age = age;
			this.birthday = birthday;
		}

		@Override
		public int hashCode() {
			return Objects.hash(age, birthday, id, name);
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
			var other = (TransientAnnoInsTestEntity) obj;
			if (age != other.age) {
				return false;
			}
			if (!Objects.equals(birthday, other.birthday)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(name, other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TransientAnnoInsTestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday
					+ "]";
		}
	}

	@Table(name = "TEST")
	public static class TransientAnnoUpdTestEntity {
		private long id;
		@Transient
		private String name;
		private int age;
		@Transient(insert = true, update = false)
		private LocalDate birthday;

		public TransientAnnoUpdTestEntity() {
		}

		public TransientAnnoUpdTestEntity(final long id, final String name, final int age, final LocalDate birthday) {
			this.id = id;
			this.name = name;
			this.age = age;
			this.birthday = birthday;
		}

		@Override
		public int hashCode() {
			return Objects.hash(age, birthday, id, name);
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
			var other = (TransientAnnoUpdTestEntity) obj;
			if (age != other.age) {
				return false;
			}
			if (!Objects.equals(birthday, other.birthday)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(name, other.name)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TransientAnnoUpdTestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday
					+ "]";
		}
	}

	@Test
	public void testAll() throws Exception {

		try (var agent = config.agent()) {
			var test1 = new TransientAnnoTestEntity(1, "name1", 20, LocalDate.of(1990,
					Month.APRIL, 1));
			agent.insert(test1);
			var data = agent.find(TransientAnnoTestEntity.class, 1).orElse(null);
			assertThat(data.name, is(nullValue()));
			assertThat(data.birthday, is(nullValue()));
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (var agent = config.agent()) {
			var date = LocalDate.of(1990, Month.APRIL, 1);
			var test1 = new TransientAnnoInsTestEntity(1, "name1", 20, date);
			agent.insert(test1);
			var data = agent.find(TransientAnnoInsTestEntity.class, 1).orElse(null);
			assertThat(data.name, is(nullValue()));
			// insertでは値が設定される
			assertThat(data.birthday, is(date));

			data.birthday = date.plusMonths(1);
			agent.update(data);

			data = agent.find(TransientAnnoInsTestEntity.class, 1).orElse(null);
			assertThat(data.name, is(nullValue()));
			// update では値が設定されない
			assertThat(data.birthday, is(date));
		}
	}

	@Test
	public void testUpdate() throws Exception {

		try (var agent = config.agent()) {
			var date = LocalDate.of(1990, Month.APRIL, 1);
			var test1 = new TransientAnnoUpdTestEntity(1, "name1", 20, date);
			agent.insert(test1);
			var data = agent.find(TransientAnnoUpdTestEntity.class, 1).orElse(null);
			assertThat(data.name, is(nullValue()));
			// insertでは値が設定されない
			assertThat(data.birthday, is(nullValue()));

			data.birthday = date;
			agent.update(data);

			data = agent.find(TransientAnnoUpdTestEntity.class, 1).orElse(null);
			assertThat(data.name, is(nullValue()));
			// update では値が設定される
			assertThat(data.birthday, is(date));
		}
	}

	@Test
	public void testQuery() throws Exception {

		try (var agent = config.agent()) {
			var date = LocalDate.of(1990, Month.APRIL, 1);
			agent.insert(new TestEntity(1, "name1", 20, date));

			var data1 = agent.find(TransientAnnoTestEntity.class, 1).orElse(null);
			assertThat(data1, notNullValue());
			assertThat(data1.name, is("name1"));
			assertThat(data1.birthday, is(date));

			var data2 = agent.find(TransientAnnoInsTestEntity.class, 1).orElse(null);
			assertThat(data2, notNullValue());
			assertThat(data2.name, is("name1"));
			assertThat(data2.birthday, is(date));

			var data3 = agent.find(TransientAnnoUpdTestEntity.class, 1).orElse(null);
			assertThat(data3, notNullValue());
			assertThat(data3.name, is("name1"));
			assertThat(data3.birthday, is(date));
		}
	}
}
