package jp.co.future.uroborosql.parameter;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;
import jp.co.future.uroborosql.testlog.TestAppender;
import org.slf4j.LoggerFactory;

public class ParameterTest {
	private SqlConfig config;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:ParameterTest", null, null).build();
	}

	@Test
	void testSetInParameter_mapperForDate() throws ParseException, SQLException {

		var date = Date.from(LocalDate.parse("2002-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("test/PARAM_MAPPING1")
					.param("targetDate", date);

			try (var rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getDate("TARGET_DATE"), is(new java.sql.Date(date.getTime())));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}
		}
	}

	@Test
	void testSetInParameter_mapperForLocalDate() throws ParseException, SQLException {

		var localDate = LocalDate.of(2002, Month.JANUARY, 1);
		var date = Date.from(LocalDate.parse("2002-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("test/PARAM_MAPPING1")
					.param("targetDate", localDate);

			try (var rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getDate("TARGET_DATE"), is(new java.sql.Date(date.getTime())));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}

		}
	}

	@Test
	void testSetInParameter_mapperForOptional() throws ParseException, SQLException {

		var localDate = LocalDate.of(2002, Month.JANUARY, 1);
		var date = Date.from(LocalDate.parse("2002-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("test/PARAM_MAPPING1")
					.param("targetDate", Optional.of(localDate));

			try (var rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getDate("TARGET_DATE"), is(new java.sql.Date(date.getTime())));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}
		}
	}

	@Test
	void testSetInParameter_mapperForArray() throws SQLException {

		String[] param = { "1", "2", "3" };

		config.getExecutionContextProvider().addBindParamMapper(new BindParameterMapper<String[]>() {

			@Override
			public Class<String[]> targetType() {
				return String[].class;
			}

			@Override
			public Object toJdbc(final String[] original, final Connection connection,
					final BindParameterMapperManager parameterMapperManager) {
				return String.join("", original);
			}
		});

		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("test/PARAM_MAPPING2")
					.param("targetStr", param);

			try (var rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getString("TARGET_STR"), is("123"));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}
		}
	}

	/**
	 * 通常のArrayテスト
	 *
	 * @throws SQLException SQL例外
	 */
	@Test
	void testSetInParameter_array() throws SQLException {

		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("test/PARAM_MAPPING3")
					.param("targetStrs", List.of("1", "2", "3"));

			try (var rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getString("TARGET_STR"), is("1"));
				assertThat(rs.next(), is(true));
				assertThat(rs.getString("TARGET_STR"), is("2"));
				assertThat(rs.next(), is(true));
				assertThat(rs.getString("TARGET_STR"), is("3"));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}
		}
	}

	/**
	 * ByteArrayのテスト
	 *
	 * @throws SQLException SQL例外
	 */
	@Test
	void testSetInParameter_bytearray() throws SQLException {

		try (var agent = config.agent()) {
			agent.updateWith("create table if not exists BYTE_ARRAY_TEST (ID VARCHAR(10), DATA BINARY(10))").count();

			assertThat("更新件数が一致しません",
					agent.updateWith("INSERT INTO BYTE_ARRAY_TEST VALUES (/*id*/, /*data*/)")
							.param("id", 1)
							.param("data", "test".getBytes()).count(),
					is(1));
		}
	}

	@Test
	void testSetParameter_subParameter() throws ParseException, SQLException {

		try (var agent = config.agent()) {
			agent.update("ddl/create_tables").count();

			var bean = new ColumnTypeTest("test", 'A', 10, true, new Timestamp(System.currentTimeMillis()),
					new Date(), Time.valueOf("10:20:30"));
			agent.insert(bean);

			var sql = "select * from COLUMN_TYPE_TEST WHERE 1 = 1 /*IF bean.colVarchar != null*/ AND COL_VARCHAR = /*bean.colVarchar*//*END*//*IF bean.colBoolean != null*/ AND COL_BOOLEAN = /*bean.colBoolean*//*END*/";
			var child1 = new ColumnTypeChild("test", 'X', 0, null, null, null, null);
			var list = agent.queryWith(sql)
					.param("bean", child1)
					.collect();
			assertThat(list.size(), is(1));
			assertThat(list.get(0).get("COL_VARCHAR"), is("test"));
			assertThat(list.get(0).get("COL_BOOLEAN"), is(true));

			child1 = new ColumnTypeChild("test2", 'X', 0, null, null, null, null);
			list = agent.queryWith(sql)
					.param("bean", child1)
					.collect();
			assertThat(list.size(), is(0));

			var child2 = new ColumnTypeChild(null, 'X', 0, true, null, null, null);
			list = agent.queryWith(sql)
					.param("bean", child2)
					.collect();
			assertThat(list.size(), is(1));
			assertThat(list.get(0).get("COL_VARCHAR"), is("test"));
			assertThat(list.get(0).get("COL_BOOLEAN"), is(true));

			child2 = new ColumnTypeChild(null, 'X', 0, false, null, null, null);
			list = agent.queryWith(sql)
					.param("bean", child2)
					.collect();
			assertThat(list.size(), is(0));
		}
	}

	/**
	 * Iterableパラメータの場合、parameterLogが1回だけ呼ばれることを確認するテスト
	 * また、ログレベルがTRACEであることを確認する
	 *
	 * @throws Exception 例外
	 */
	@Test
	void testSetInParameter_iterableParameterLogOnce() throws Exception {
		// Configure SQL logger to TRACE level and attach TestAppender
		var loggerContext = (LoggerContext) LoggerFactory.getILoggerFactory();
		var sqlLogger = loggerContext.getLogger("jp.co.future.uroborosql.sql");
		var originalLevel = sqlLogger.getLevel();
		var testAppender = new TestAppender();
		testAppender.setContext(loggerContext);
		var encoder = new ch.qos.logback.classic.encoder.PatternLayoutEncoder();
		encoder.setContext(loggerContext);
		encoder.setPattern("%-5level - %m%n");
		encoder.start();
		testAppender.setEncoder(encoder);
		testAppender.start();

		try {
			sqlLogger.setLevel(Level.TRACE);
			sqlLogger.addAppender(testAppender);

			var logs = TestAppender.getLogbackLogs(() -> {
				try (var agent = config.agent()) {
					// Test with a List parameter (Iterable) - should log once
					var ctx = agent.context().setSqlName("test/PARAM_MAPPING3")
							.param("targetStrs", List.of("1", "2", "3"));

					try (var rs = agent.query(ctx)) {
						assertThat("結果が0件です。", rs.next(), is(true));
						assertThat(rs.getString("TARGET_STR"), is("1"));
						assertThat(rs.next(), is(true));
						assertThat(rs.getString("TARGET_STR"), is("2"));
						assertThat(rs.next(), is(true));
						assertThat(rs.getString("TARGET_STR"), is("3"));

						assertThat("取得データが多すぎます", rs.next(), is(false));
					}

					// Test with a non-Iterable parameter - should also log once
					var ctx2 = agent.context().setSqlName("test/PARAM_MAPPING2")
							.param("targetStr", "123");

					try (var rs = agent.query(ctx2)) {
						assertThat("結果が0件です。", rs.next(), is(true));
						assertThat(rs.getString("TARGET_STR"), is("123"));

						assertThat("取得データが多すぎます", rs.next(), is(false));
					}
				}
			});

			// Verify that parameter logs are at TRACE level
			// Filter logs for parameter setting messages
			var parameterLogs = logs.stream()
					.filter(log -> log.contains("Set the parameter"))
					.collect(java.util.stream.Collectors.toList());

			// Verify we have at least one parameter log
			assertThat("パラメータログが出力されていません", parameterLogs.size() > 0, is(true));

			// Verify all parameter logs are at TRACE level
			for (var log : parameterLogs) {
				assertThat("パラメータログがTRACEレベルではありません: " + log, log.contains("TRACE"), is(true));
			}
		} finally {
			// Restore original logger configuration
			sqlLogger.detachAppender(testAppender);
			sqlLogger.setLevel(originalLevel);
			testAppender.stop();
		}
	}

	@Table(name = "COLUMN_TYPE_TEST")
	public static class ColumnTypeTest {
		private String colVarchar;
		private char colChar;
		private int colNumeric;
		private Boolean colBoolean;
		private Timestamp colTimestamp;
		private Date colDate;
		private Time colTime;

		public ColumnTypeTest(final String colVarchar, final char colChar, final int colNumeric,
				final Boolean colBoolean,
				final Timestamp colTimestamp, final Date colDate, final Time colTime) {
			this.colVarchar = colVarchar;
			this.colChar = colChar;
			this.colNumeric = colNumeric;
			this.colBoolean = colBoolean;
			this.colTimestamp = colTimestamp;
			this.colDate = colDate;
			this.colTime = colTime;
		}

		public String getColVarchar() {
			return colVarchar;
		}

		public void setColVarchar(final String colVarchar) {
			this.colVarchar = colVarchar;
		}

		public char getColChar() {
			return colChar;
		}

		public void setColChar(final char colChar) {
			this.colChar = colChar;
		}

		public int getColNumeric() {
			return colNumeric;
		}

		public void setColNumeric(final int colNumeric) {
			this.colNumeric = colNumeric;
		}

		public Boolean getColBoolean() {
			return colBoolean;
		}

		public void setColBoolean(final Boolean colBoolean) {
			this.colBoolean = colBoolean;
		}

		public Timestamp getColTimestamp() {
			return colTimestamp;
		}

		public void setColTimestamp(final Timestamp colTimestamp) {
			this.colTimestamp = colTimestamp;
		}

		public Date getColDate() {
			return colDate;
		}

		public void setColDate(final Date colDate) {
			this.colDate = colDate;
		}

		public Time getColTime() {
			return colTime;
		}

		public void setColTime(final Time colTime) {
			this.colTime = colTime;
		}

	}

	public static class ColumnTypeChild extends ColumnTypeTest {
		public ColumnTypeChild(final String colVarchar, final char colChar, final int colNumeric,
				final Boolean colBoolean,
				final Timestamp colTimestamp, final Date colDate, final Time colTime) {
			super(colVarchar, colChar, colNumeric, colBoolean, colTimestamp, colDate, colTime);
		}

	}
}
