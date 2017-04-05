package jp.co.future.uroborosql.mapping.annotations;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.Month;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
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

		config = DefaultSqlConfig.getConfig(url, user, password);

		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		sqlFilterManager.addSqlFilter(new AuditLogSqlFilter());
	}

	@Before
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.createAgent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Table(name = "TEST")
	public static class TransientAnnoTestEntity {
		private long id;
		@Transient
		private String name;
		private int age;
		@Transient
		private LocalDate birthday;

		public TransientAnnoTestEntity() {
		}

		public TransientAnnoTestEntity(final long id, final String name, final int age, final LocalDate birthday) {
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
	public void test() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				TransientAnnoTestEntity test1 = new TransientAnnoTestEntity(1, "name1", 20, LocalDate.of(1990, Month.APRIL, 1));
				agent.insert(test1);
				TransientAnnoTestEntity data = agent.find(TransientAnnoTestEntity.class, 1).orElse(null);
				assertThat(data.name, is(nullValue()));
				assertThat(data.birthday, is(nullValue()));

			});
		}
	}

}
