package jp.co.future.uroborosql.parameter.mapper.legacy;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.Statement;
import java.sql.Time;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.MonthDay;
import java.time.Year;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContextFactoryImpl;
import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.enums.InsertsType;
import jp.co.future.uroborosql.expr.ognl.OgnlExpressionParserFactory;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.Table;

/**
 * LegacyパッケージのToStringParameterMapperを設定した上でDAOインタフェースの動作を確認するためのテストケース. <br>
 *
 * OGNLを使用した場合
 *
 * @author H.Sugimoto
 */
public class LegacyToStringParameterMapperWithOgnlTest {

	private static SqlConfig config;

	@BeforeAll
	public static void setUpBeforeClass() throws Exception {
		String url = "jdbc:h2:mem:" + LegacyToStringParameterMapperWithOgnlTest.class.getSimpleName()
				+ ";DB_CLOSE_DELAY=-1";
		String user = null;
		String password = null;

		try (Connection conn = DriverManager.getConnection(url, user, password)) {
			conn.setAutoCommit(false);
			// テーブル作成
			try (Statement stmt = conn.createStatement()) {
				stmt.execute("drop table if exists test");

				StringBuilder builder = new StringBuilder();
				builder.append("create table if not exists test( ")
						.append("id integer auto_increment, ")
						.append("date_time varchar(17), ")
						.append("date varchar(8), ")
						.append("day_of_week varchar(1), ")
						.append("local_date varchar(8), ")
						.append("month_day varchar(4), ")
						.append("month varchar(2), ")
						.append("sql_time varchar(6), ")
						.append("time varchar(6), ")
						.append("year_month varchar(6), ")
						.append("year varchar(4), ")
						.append("primary key(id) )");

				stmt.execute(builder.toString());
			}
		}

		config = UroboroSQL.builder(url, user, password)
				.setExpressionParser(new OgnlExpressionParserFactory().create())
				.setSqlContextFactory(new SqlContextFactoryImpl()
						.addBindParamMapper(new DateTimeToStringParameterMapper())
						.addBindParamMapper(new DateToStringParameterMapper())
						.addBindParamMapper(new DayOfWeekToStringParameterMapper())
						.addBindParamMapper(new LocalDateToStringParameterMapper())
						.addBindParamMapper(new MonthToStringParameterMapper())
						.addBindParamMapper(new SqlTimeToStringParameterMapper())
						.addBindParamMapper(new TimeToStringParameterMapper())
						.addBindParamMapper(new YearMonthToStringParameterMapper())
						.addBindParamMapper(new YearToStringParameterMapper()))
				.build();
	}

	@BeforeEach
	public void setUpBefore() throws Exception {
		try (SqlAgent agent = config.agent()) {
			agent.updateWith("delete from test").count();
			agent.commit();
		}
	}

	@Test
	public void testInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test = new TestEntity(
						LocalDateTime.of(2020, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.SUNDAY,
						LocalDate.of(2020, 2, 3),
						MonthDay.of(4, 3),
						Month.APRIL,
						Time.valueOf(LocalTime.parse("11:22:33")),
						LocalTime.of(10, 20, 30),
						YearMonth.of(2020, 5),
						Year.of(2020));
				TestEntity entity = agent.insertAndReturn(test);
				TestEntity data = agent.find(TestEntity.class, entity.getId()).orElse(null);
				assertThat(data, is(test));
			});
		}
	}

	@Test
	public void testQuery1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(
						LocalDateTime.of(2020, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.SUNDAY,
						LocalDate.of(2020, 2, 3),
						MonthDay.of(4, 3),
						Month.APRIL,
						Time.valueOf(LocalTime.parse("11:22:33")),
						LocalTime.of(10, 20, 30),
						YearMonth.of(2020, 5),
						Year.of(2020));
				agent.insertAndReturn(test1);
				TestEntity test2 = new TestEntity(
						LocalDateTime.of(2021, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2001-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.MONDAY,
						LocalDate.of(2021, 2, 3),
						MonthDay.of(9, 4),
						Month.DECEMBER,
						Time.valueOf(LocalTime.parse("21:32:43")),
						LocalTime.of(20, 30, 40),
						YearMonth.of(2021, 12),
						Year.of(2021));
				agent.insertAndReturn(test2);

				List<TestEntity> list = agent.query(TestEntity.class).collect();
				assertThat(list.size(), is(2));
				assertThat(list.get(0), is(test1));
				assertThat(list.get(1), is(test2));

				// dateTime
				list = agent.query(TestEntity.class)
						.equal("dateTime", LocalDateTime.of(2020, 1, 2, 10, 20, 30))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// dateTime
				list = agent.query(TestEntity.class)
						.equal("date",
								Date.from(
										LocalDate.parse("2001-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				// dayOfWeek
				list = agent.query(TestEntity.class)
						.equal("dayOfWeek", DayOfWeek.SUNDAY)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// localDate
				list = agent.query(TestEntity.class)
						.equal("localDate", LocalDate.of(2021, 2, 3))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				// monthDay
				list = agent.query(TestEntity.class)
						.equal("monthDay", MonthDay.of(4, 3))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// month
				list = agent.query(TestEntity.class)
						.equal("month", Month.DECEMBER)
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				// sqlTime
				list = agent.query(TestEntity.class)
						.equal("sqlTime", Time.valueOf(LocalTime.parse("11:22:33")))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// time
				list = agent.query(TestEntity.class)
						.equal("time", LocalTime.of(20, 30, 40))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));

				// yearMonth
				list = agent.query(TestEntity.class)
						.equal("yearMonth", YearMonth.of(2020, 5))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test1));

				// year
				list = agent.query(TestEntity.class)
						.equal("year", Year.of(2021))
						.collect();
				assertThat(list.size(), is(1));
				assertThat(list.get(0), is(test2));
			});
		}
	}

	@Test
	public void testUpdate1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(
						LocalDateTime.of(2020, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.SUNDAY,
						LocalDate.of(2020, 2, 3),
						MonthDay.of(4, 3),
						Month.APRIL,
						Time.valueOf(LocalTime.parse("11:22:33")),
						LocalTime.of(10, 20, 30),
						YearMonth.of(2020, 5),
						Year.of(2020));
				TestEntity entity1 = agent.insertAndReturn(test1);
				TestEntity test2 = new TestEntity(
						LocalDateTime.of(2021, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2001-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.MONDAY,
						LocalDate.of(2021, 2, 3),
						MonthDay.of(9, 3),
						Month.DECEMBER,
						Time.valueOf(LocalTime.parse("21:32:43")),
						LocalTime.of(20, 30, 40),
						YearMonth.of(2021, 12),
						Year.of(2021));
				agent.insertAndReturn(test2);

				LocalTime updateTime = LocalTime.of(11, 22, 33);
				test1.setTime(updateTime);
				TestEntity updateEntity = agent.updateAndReturn(test1);

				TestEntity data = agent.find(TestEntity.class, entity1.getId()).orElse(null);
				assertThat(data, is(updateEntity));
				assertThat(data.getTime(), is(updateTime));
			});
		}
	}

	@Test
	public void testDelete1() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(
						LocalDateTime.of(2020, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.SUNDAY,
						LocalDate.of(2020, 2, 3),
						MonthDay.of(4, 3),
						Month.APRIL,
						Time.valueOf(LocalTime.parse("11:22:33")),
						LocalTime.of(10, 20, 30),
						YearMonth.of(2020, 5),
						Year.of(2020));
				TestEntity entity1 = agent.insertAndReturn(test1);
				TestEntity test2 = new TestEntity(
						LocalDateTime.of(2021, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2001-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.MONDAY,
						LocalDate.of(2021, 2, 3),
						MonthDay.of(9, 3),
						Month.DECEMBER,
						Time.valueOf(LocalTime.parse("21:32:43")),
						LocalTime.of(20, 30, 40),
						YearMonth.of(2021, 12),
						Year.of(2021));
				agent.insertAndReturn(test2);

				agent.delete(test1);

				TestEntity data = agent.find(TestEntity.class, entity1.getId()).orElse(null);
				assertThat(data, is(nullValue()));
			});
		}
	}

	@Test
	public void testBatchInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(
						LocalDateTime.of(2020, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.SUNDAY,
						LocalDate.of(2020, 2, 3),
						MonthDay.of(4, 3),
						Month.APRIL,
						Time.valueOf(LocalTime.parse("11:22:33")),
						LocalTime.of(10, 20, 30),
						YearMonth.of(2020, 5),
						Year.of(2020));
				TestEntity test2 = new TestEntity(
						LocalDateTime.of(2021, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2001-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.MONDAY,
						LocalDate.of(2021, 2, 3),
						MonthDay.of(9, 3),
						Month.DECEMBER,
						Time.valueOf(LocalTime.parse("21:32:43")),
						LocalTime.of(20, 30, 40),
						YearMonth.of(2021, 12),
						Year.of(2021));

				int count = agent.inserts(Stream.of(test1, test2), InsertsType.BATCH);
				assertThat(count, is(2));
			});
		}
	}

	@Test
	public void testBulkInsert() throws Exception {

		try (SqlAgent agent = config.agent()) {
			agent.required(() -> {
				TestEntity test1 = new TestEntity(
						LocalDateTime.of(2020, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2000-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.SUNDAY,
						LocalDate.of(2020, 2, 3),
						MonthDay.of(4, 3),
						Month.APRIL,
						Time.valueOf(LocalTime.parse("11:22:33")),
						LocalTime.of(10, 20, 30),
						YearMonth.of(2020, 5),
						Year.of(2020));
				TestEntity test2 = new TestEntity(
						LocalDateTime.of(2021, 1, 2, 10, 20, 30),
						Date.from(LocalDate.parse("2001-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant()),
						DayOfWeek.MONDAY,
						LocalDate.of(2021, 2, 3),
						MonthDay.of(9, 3),
						Month.DECEMBER,
						Time.valueOf(LocalTime.parse("21:32:43")),
						LocalTime.of(20, 30, 40),
						YearMonth.of(2021, 12),
						Year.of(2021));

				int count = agent.inserts(Stream.of(test1, test2), InsertsType.BULK);
				assertThat(count, is(2));
			});
		}
	}

	@Table(name = "TEST")
	public static class TestEntity {
		@Id
		@GeneratedValue(strategy = GenerationType.IDENTITY)
		private Integer id;
		private LocalDateTime dateTime;
		private Date date;
		private DayOfWeek dayOfWeek;
		private LocalDate localDate;
		private MonthDay monthDay;
		private Month month;
		private Time sqlTime;
		private LocalTime time;
		private YearMonth yearMonth;
		private Year year;

		public TestEntity() {
		}

		public TestEntity(final LocalDateTime dateTime,
				final Date date,
				final DayOfWeek dayOfWeek,
				final LocalDate localDate,
				final MonthDay monthDay,
				final Month month,
				final Time sqlTime,
				final LocalTime time,
				final YearMonth yearMonth,
				final Year year) {
			super();
			this.dateTime = dateTime;
			this.date = date;
			this.dayOfWeek = dayOfWeek;
			this.localDate = localDate;
			this.monthDay = monthDay;
			this.month = month;
			this.sqlTime = sqlTime;
			this.time = time;
			this.yearMonth = yearMonth;
			this.year = year;
		}

		public Integer getId() {
			return id;
		}

		public void setId(final Integer id) {
			this.id = id;
		}

		public LocalDateTime getDateTime() {
			return dateTime;
		}

		public void setDateTime(final LocalDateTime dateTime) {
			this.dateTime = dateTime;
		}

		public Date getDate() {
			return date;
		}

		public void setDate(final Date date) {
			this.date = date;
		}

		public DayOfWeek getDayOfWeek() {
			return dayOfWeek;
		}

		public void setDayOfWeek(final DayOfWeek dayOfWeek) {
			this.dayOfWeek = dayOfWeek;
		}

		public LocalDate getLocalDate() {
			return localDate;
		}

		public void setLocalDate(final LocalDate localDate) {
			this.localDate = localDate;
		}

		public MonthDay getMonthDay() {
			return monthDay;
		}

		public void setMonthDay(final MonthDay monthDay) {
			this.monthDay = monthDay;
		}

		public Month getMonth() {
			return month;
		}

		public void setMonth(final Month month) {
			this.month = month;
		}

		public Time getSqlTime() {
			return sqlTime;
		}

		public void setSqlTime(final Time sqlTime) {
			this.sqlTime = sqlTime;
		}

		public LocalTime getTime() {
			return time;
		}

		public void setTime(final LocalTime time) {
			this.time = time;
		}

		public YearMonth getYearMonth() {
			return yearMonth;
		}

		public void setYearMonth(final YearMonth yearMonth) {
			this.yearMonth = yearMonth;
		}

		public Year getYear() {
			return year;
		}

		public void setYear(final Year year) {
			this.year = year;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + (date == null ? 0 : date.hashCode());
			result = prime * result + (dateTime == null ? 0 : dateTime.hashCode());
			result = prime * result + (dayOfWeek == null ? 0 : dayOfWeek.hashCode());
			result = prime * result + (id == null ? 0 : id.hashCode());
			result = prime * result + (localDate == null ? 0 : localDate.hashCode());
			result = prime * result + (month == null ? 0 : month.hashCode());
			result = prime * result + (monthDay == null ? 0 : monthDay.hashCode());
			result = prime * result + (sqlTime == null ? 0 : sqlTime.hashCode());
			result = prime * result + (time == null ? 0 : time.hashCode());
			result = prime * result + (year == null ? 0 : year.hashCode());
			result = prime * result + (yearMonth == null ? 0 : yearMonth.hashCode());
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
			if (date == null) {
				if (other.date != null) {
					return false;
				}
			} else if (date.compareTo(other.date) != 0) {
				return false;
			}
			if (dateTime == null) {
				if (other.dateTime != null) {
					return false;
				}
			} else if (!dateTime.equals(other.dateTime)) {
				return false;
			}
			if (dayOfWeek != other.dayOfWeek) {
				return false;
			}
			if (id == null) {
				if (other.id != null) {
					return false;
				}
			} else if (!id.equals(other.id)) {
				return false;
			}
			if (localDate == null) {
				if (other.localDate != null) {
					return false;
				}
			} else if (!localDate.equals(other.localDate)) {
				return false;
			}
			if (month != other.month) {
				return false;
			}
			if (monthDay == null) {
				if (other.monthDay != null) {
					return false;
				}
			} else if (!monthDay.equals(other.monthDay)) {
				return false;
			}
			if (sqlTime == null) {
				if (other.sqlTime != null) {
					return false;
				}
			} else if (!sqlTime.equals(other.sqlTime)) {
				return false;
			}
			if (time == null) {
				if (other.time != null) {
					return false;
				}
			} else if (!time.equals(other.time)) {
				return false;
			}
			if (year == null) {
				if (other.year != null) {
					return false;
				}
			} else if (!year.equals(other.year)) {
				return false;
			}
			if (yearMonth == null) {
				if (other.yearMonth != null) {
					return false;
				}
			} else if (!yearMonth.equals(other.yearMonth)) {
				return false;
			}
			return true;
		}

		@Override
		public String toString() {
			return "TestEntity [id=" + id + ", dateTime=" + dateTime + ", date=" + date + ", dayOfWeek=" + dayOfWeek
					+ ", localDate=" + localDate + ", monthDay=" + monthDay + ", month=" + month + ", sqlTime="
					+ sqlTime + ", time=" + time + ", yearMonth=" + yearMonth + ", year=" + year + "]";
		}

	}

}
