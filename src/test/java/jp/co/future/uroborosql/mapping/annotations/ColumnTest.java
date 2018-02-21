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

public class ColumnTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:ColumnTest;DB_CLOSE_DELAY=-1";
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

	@Table(name = "TEST")
	public static class ColumnAnnoTestEntity {
		@Column(name = "ID")
		private long idAaaaAaaa;
		@Column(name = "NAME")
		private String nameAaaaAaaa;
		@Column(name = "AGE")
		private int ageAaaaAaaa;
		@Column(name = "BIRTHDAY")
		private LocalDate birthdayAaaaAaaa;
		@Column(name = "MEMO")
		@Transient
		private String memoAaaaAaaa;

		public ColumnAnnoTestEntity() {
		}

		public ColumnAnnoTestEntity(final long id, final String name, final int age, final LocalDate birthday,
				final String memo) {
			this.idAaaaAaaa = id;
			this.nameAaaaAaaa = name;
			this.ageAaaaAaaa = age;
			this.birthdayAaaaAaaa = birthday;
			this.memoAaaaAaaa = memo;
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

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				// insert
				ColumnAnnoTestEntity test1 = new ColumnAnnoTestEntity(1, "name1", 20, LocalDate
						.of(1990, Month.APRIL, 1), "memo1");
				agent.insert(test1);
				test1.memoAaaaAaaa = null;

				ColumnAnnoTestEntity test2 = new ColumnAnnoTestEntity(2, "name2", 21, LocalDate
						.of(1990, Month.APRIL, 2), null);
				agent.insert(test2);
				test2.memoAaaaAaaa = null;

				ColumnAnnoTestEntity test3 = new ColumnAnnoTestEntity(3, "name3", 22, LocalDate
						.of(1990, Month.APRIL, 3), "memo3");
				agent.insert(test3);
				test3.memoAaaaAaaa = null;

				ColumnAnnoTestEntity data = agent.find(ColumnAnnoTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));
				data = agent.find(ColumnAnnoTestEntity.class, 2).orElse(null);
				assertThat(data, is(test2));
				data = agent.find(ColumnAnnoTestEntity.class, 3).orElse(null);
				assertThat(data, is(test3));

				// update

				test1.nameAaaaAaaa = "name11";
				test1.memoAaaaAaaa = "memo11";
				agent.update(test1);

				test1.memoAaaaAaaa = null;

				data = agent.find(ColumnAnnoTestEntity.class, 1).orElse(null);
				assertThat(data, is(test1));

			});
		}
	}

}
