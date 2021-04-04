package jp.co.future.uroborosql.mapping.mapper;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.sql.DriverManager;
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
import java.util.Objects;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.filter.AuditLogSqlFilter;
import jp.co.future.uroborosql.mapping.annotations.Table;

public class DateTimeTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		var url = "jdbc:h2:mem:DateTimeTest;DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (var conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (var stmt = conn.createStatement()) {
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
			return Objects.hash(dateValue, datetimeValue, id, timeValue);
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
			var other = (LocalTestEntity) obj;
			if (!Objects.equals(dateValue, other.dateValue)) {
				return false;
			}
			if (!Objects.equals(datetimeValue, other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(timeValue, other.timeValue)) {
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
			return Objects.hash(dateValue, datetimeValue, id, timeValue);
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
			var other = (DateTestEntity) obj;
			if (!Objects.equals(dateValue, other.dateValue)) {
				return false;
			}
			if (!Objects.equals(datetimeValue, other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(timeValue, other.timeValue)) {
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
			return Objects.hash(datetimeValue, id, timeValue);
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
			var other = (OffsetTestEntity) obj;
			if (!Objects.equals(datetimeValue, other.datetimeValue)) {
				return false;
			}
			if (id != other.id) {
				return false;
			}
			if (!Objects.equals(timeValue, other.timeValue)) {
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
			return Objects.hash(datetimeValue, id);
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
			var other = (ZonedTestEntity) obj;
			if (!Objects.equals(datetimeValue, other.datetimeValue)) {
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

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new ZonedTestEntity(1);
				agent.insert(test1);
				var zoned = agent.find(ZonedTestEntity.class, 1).orElse(null);
				var local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(zoned.datetimeValue, is(test1.datetimeValue.truncatedTo(ChronoUnit.MILLIS)));
				assertThat(zoned.datetimeValue.toLocalDateTime(), is(local.datetimeValue));
			});
		}
	}

	@Test
	public void testOffset() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new OffsetTestEntity(1);
				agent.insert(test1);
				test1.datetimeValue = test1.datetimeValue.truncatedTo(ChronoUnit.MILLIS);
				var offset = agent.find(OffsetTestEntity.class, 1).orElse(null);
				var local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(offset.datetimeValue, is(test1.datetimeValue));
				assertThat(offset.datetimeValue.toLocalDateTime(), is(local.datetimeValue));
				assertThat(offset.timeValue.toLocalTime(), is(local.timeValue));
			});
		}
	}

	@Test
	public void testDate() throws Exception {

		try (var agent = config.agent()) {
			agent.required(() -> {
				var test1 = new DateTestEntity(1);
				agent.insert(test1);
				var date = agent.find(DateTestEntity.class, 1).orElse(null);
				var local = agent.find(LocalTestEntity.class, 1).orElse(null);
				assertThat(date.datetimeValue.getTime(), is(test1.datetimeValue.getTime()));
				assertThat(date.dateValue.getTime(), is(test1.dateValue.getTime()));
				System.out.println(date.timeValue);

				final var c = Calendar.getInstance();
				c.setLenient(false);
				c.setTime(date.timeValue);
				c.set(Calendar.YEAR, 1970);
				c.set(Calendar.MONTH, 0);
				c.set(Calendar.DAY_OF_MONTH, 1);
				var time = c.getTime();
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
