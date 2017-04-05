package jp.co.future.uroborosql.mapping.mapper;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

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
import java.util.Calendar;
import java.util.Date;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.filter.SqlFilterManager;
import jp.co.future.uroborosql.mapping.annotations.Table;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.time.DateUtils;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

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
	public void testZoned() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				ZonedTestEntity test1 = new ZonedTestEntity(1);
				agent.insert(test1);
				ZonedTestEntity zoned = agent.find(ZonedTestEntity.class, 1).orElse(null);
				LocalTestEntity local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(zoned.datetimeValue, is(test1.datetimeValue));
				assertThat(zoned.datetimeValue.toLocalDateTime(), is(local.datetimeValue));
			});
		}
	}

	@Test
	public void testOffset() throws Exception {

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				OffsetTestEntity test1 = new OffsetTestEntity(1);
				agent.insert(test1);
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

		try (SqlAgent agent = config.createAgent()) {
			agent.required(() -> {
				DateTestEntity test1 = new DateTestEntity(1);
				agent.insert(test1);
				DateTestEntity date = agent.find(DateTestEntity.class, 1).orElse(null);
				LocalTestEntity local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(date.datetimeValue.getTime(), is(test1.datetimeValue.getTime()));
				assertThat(date.dateValue.getTime(), is(DateUtils.truncate(test1.dateValue, Calendar.DAY_OF_MONTH).getTime()));
				System.out.println(date.timeValue);
				Date time = DateUtils.setYears(test1.timeValue, 1970);
				time = DateUtils.setMonths(time, 0);
				time = DateUtils.setDays(time, 1);
				assertThat(date.timeValue.getTime(), is(time.getTime()));
				assertThat(new Date(date.datetimeValue.getTime()).toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime(),
						is(local.datetimeValue));
				assertThat(new Date(date.dateValue.getTime()).toInstant().atZone(ZoneId.systemDefault()).toLocalDate(), is(local.dateValue));
				assertThat(new Date(date.timeValue.getTime()).toInstant().atZone(ZoneId.systemDefault()).toLocalTime(), is(local.timeValue));
			});
		}
	}

}
