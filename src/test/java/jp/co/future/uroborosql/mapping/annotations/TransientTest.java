package jp.co.future.uroborosql.mapping.annotations;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class TransientTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:TransientTest;DB_CLOSE_DELAY=-1";
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
			agent.updateWith("delete from test").count();
			agent.commit();
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

	@SuppressWarnings("unused")
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

	@SuppressWarnings("unused")
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

	@Test
	public void testAll() throws Exception {

		try (SqlAgent agent = config.agent()) {
			TransientAnnoTestEntity test1 = new TransientAnnoTestEntity(1, "name1", 20, LocalDate.of(1990,
					Month.APRIL, 1));
			agent.insert(test1);
			TransientAnnoTestEntity data = agent.find(TransientAnnoTestEntity.class, 1).orElse(null);
			assertThat(data.name, is(nullValue()));
			assertThat(data.birthday, is(nullValue()));
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			LocalDate date = LocalDate.of(1990, Month.APRIL, 1);
			TransientAnnoInsTestEntity test1 = new TransientAnnoInsTestEntity(1, "name1", 20, date);
			agent.insert(test1);
			TransientAnnoInsTestEntity data = agent.find(TransientAnnoInsTestEntity.class, 1).orElse(null);
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

		try (SqlAgent agent = config.agent()) {
			LocalDate date = LocalDate.of(1990, Month.APRIL, 1);
			TransientAnnoUpdTestEntity test1 = new TransientAnnoUpdTestEntity(1, "name1", 20, date);
			agent.insert(test1);
			TransientAnnoUpdTestEntity data = agent.find(TransientAnnoUpdTestEntity.class, 1).orElse(null);
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
}
