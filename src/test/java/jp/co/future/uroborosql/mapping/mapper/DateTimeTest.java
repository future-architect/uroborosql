package jp.co.future.uroborosql.mapping.mapper;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Date;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class DateTimeTest {

	private static SqlConfig config;

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:DateTimeTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute(
						"drop table if exists test");
				stmt.execute(
						"create table if not exists test( "
								+ "id NUMERIC(4)"
								+ ",date_value DATE"
								+ ",datetime_value DATETIME"
								+ ",time_value TIME"
								+ ", primary key(id))");
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
	public static class LocalTestEntity {
		private long id;
		private LocalDate dateValue;
		private LocalDateTime datetimeValue;
		private LocalTime timeValue;

		public LocalTestEntity(final long id) {
			this.id = id;
			this.dateValue = LocalDate.now();
			this.datetimeValue = LocalDateTime.now();
			this.timeValue = LocalTime.now();
		}

		public LocalTestEntity() {
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (dateValue == null ? 0 : dateValue.hashCode());
			result = prime * result + (datetimeValue == null ? 0 : datetimeValue.hashCode());
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (timeValue == null ? 0 : timeValue.hashCode());
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
			LocalTestEntity other = (LocalTestEntity) obj;
			if (dateValue == null) {
				if (other.dateValue != null) {
					return false;
				}
			} else if (!dateValue.equals(other.dateValue)) {
				return false;
			}
			if (datetimeValue == null) {
				if (other.datetimeValue != null) {
					return false;
				}
			} else if (!datetimeValue.equals(other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (timeValue == null) {
				if (other.timeValue != null) {
					return false;
				}
			} else if (!timeValue.equals(other.timeValue)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "LocalTestEntity [id=" + id + ", dateValue=" + dateValue + ", datetimeValue=" + datetimeValue
					+ ", timeValue=" + timeValue + "]";
		}
	}

	@Table(name = "TEST")
	public static class DateTestEntity {
		private long id;
		private java.sql.Date dateValue;
		private java.util.Date datetimeValue;
		private java.sql.Time timeValue;

		public DateTestEntity(final long id) {
			this.id = id;
			this.dateValue = java.sql.Date.valueOf(LocalDate.now());
			this.datetimeValue = new java.util.Date();
			this.timeValue = java.sql.Time.valueOf(LocalTime.now());
		}

		public DateTestEntity() {
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (dateValue == null ? 0 : dateValue.hashCode());
			result = prime * result + (datetimeValue == null ? 0 : datetimeValue.hashCode());
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (timeValue == null ? 0 : timeValue.hashCode());
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
			DateTestEntity other = (DateTestEntity) obj;
			if (dateValue == null) {
				if (other.dateValue != null) {
					return false;
				}
			} else if (!dateValue.equals(other.dateValue)) {
				return false;
			}
			if (datetimeValue == null) {
				if (other.datetimeValue != null) {
					return false;
				}
			} else if (!datetimeValue.equals(other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (timeValue == null) {
				if (other.timeValue != null) {
					return false;
				}
			} else if (!timeValue.equals(other.timeValue)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "DateTestEntity [id=" + id + ", dateValue=" + dateValue + ", datetimeValue=" + datetimeValue
					+ ", timeValue=" + timeValue + "]";
		}
	}

	@Table(name = "TEST")
	public static class OffsetTestEntity {
		private long id;
		private OffsetDateTime datetimeValue;
		private OffsetTime timeValue;

		public OffsetTestEntity(final long id) {
			this.id = id;
			this.datetimeValue = OffsetDateTime.now();
			this.timeValue = OffsetTime.now();
		}

		public OffsetTestEntity() {
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (datetimeValue == null ? 0 : datetimeValue.hashCode());
			result = prime * result + (int) (id ^ id >>> 32);
			result = prime * result + (timeValue == null ? 0 : timeValue.hashCode());
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
			OffsetTestEntity other = (OffsetTestEntity) obj;
			if (datetimeValue == null) {
				if (other.datetimeValue != null) {
					return false;
				}
			} else if (!datetimeValue.equals(other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (timeValue == null) {
				if (other.timeValue != null) {
					return false;
				}
			} else if (!timeValue.equals(other.timeValue)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "OffsetTestEntity [id=" + id + ", datetimeValue=" + datetimeValue + ", timeValue=" + timeValue + "]";
		}
	}

	@Table(name = "TEST")
	public static class ZonedTestEntity {
		private long id;
		private ZonedDateTime datetimeValue;

		public ZonedTestEntity(final long id) {
			this.id = id;
			this.datetimeValue = ZonedDateTime.now();
		}

		public ZonedTestEntity() {
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (datetimeValue == null ? 0 : datetimeValue.hashCode());
			result = prime * result + (int) (id ^ id >>> 32);
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
			ZonedTestEntity other = (ZonedTestEntity) obj;
			if (datetimeValue == null) {
				if (other.datetimeValue != null) {
					return false;
				}
			} else if (!datetimeValue.equals(other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "ZonedTestEntity [id=" + id + ", datetimeValue=" + datetimeValue + "]";
		}
	}

	@Test
	public void testZoned() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				ZonedTestEntity test1 = new ZonedTestEntity(1);
				agent.insert(test1);
				ZonedTestEntity zoned = agent.find(ZonedTestEntity.class, 1).orElse(null);
				LocalTestEntity local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(zoned.datetimeValue, is(test1.datetimeValue.truncatedTo(ChronoUnit.MILLIS)));
				assertThat(zoned.datetimeValue.toLocalDateTime(), is(local.datetimeValue));
			});
		}
	}

	@Test
	public void testOffset() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				OffsetTestEntity test1 = new OffsetTestEntity(1);
				agent.insert(test1);
				test1.datetimeValue = test1.datetimeValue.truncatedTo(ChronoUnit.MILLIS);
				OffsetTestEntity offset = agent.find(OffsetTestEntity.class, 1).orElse(null);
				LocalTestEntity local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(offset.datetimeValue, is(test1.datetimeValue));
				assertThat(offset.datetimeValue.toLocalDateTime(), is(local.datetimeValue));
				assertThat(offset.timeValue.toLocalTime(), is(local.timeValue));
			});
		}
	}

	@Test
	public void testDate() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				DateTestEntity test1 = new DateTestEntity(1);
				agent.insert(test1);
				DateTestEntity date = agent.find(DateTestEntity.class, 1).orElse(null);
				LocalTestEntity local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(date.datetimeValue.getTime(), is(test1.datetimeValue.getTime()));
				assertThat(date.dateValue.getTime(), is(test1.dateValue.getTime()));
				System.out.println(date.timeValue);

				final Calendar c = Calendar.getInstance();
				c.setLenient(false);
				c.setTime(date.timeValue);
				c.set(Calendar.YEAR, 1970);
				c.set(Calendar.MONTH, 0);
				c.set(Calendar.DAY_OF_MONTH, 1);
				Date time = c.getTime();
				assertThat(date.timeValue.getTime(), is(time.getTime()));
				assertThat(new Date(date.datetimeValue.getTime()).toInstant().atZone(ZoneId.systemDefault())
						.toLocalDateTime(),
						is(local.datetimeValue));
				assertThat(new Date(date.dateValue.getTime()).toInstant().atZone(ZoneId.systemDefault()).toLocalDate(),
						is(local.dateValue));
				assertThat(new Date(date.timeValue.getTime()).toInstant().atZone(ZoneId.systemDefault()).toLocalTime(),
						is(local.timeValue));
			});
		}
	}

}
