package jp.co.future.uroborosql.parameter;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.Month;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.ExecutionContext;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

public class ParameterTest {
	private SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:ParameterTest", null, null).build();
	}

	@Test
	public void testSetInParameter_mapperForDate() throws ParseException, SQLException {

		Date date = Date.from(LocalDate.parse("2002-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		try (SqlAgent agent = config.agent()) {
			ExecutionContext ctx = agent.contextFrom("test/PARAM_MAPPING1").param("targetDate", date);

			try (ResultSet rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getDate("TARGET_DATE"), is(new java.sql.Date(date.getTime())));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}
		}
	}

	@Test
	public void testSetInParameter_mapperForLocalDate() throws ParseException, SQLException {

		LocalDate localDate = LocalDate.of(2002, Month.JANUARY, 1);
		Date date = Date.from(LocalDate.parse("2002-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		try (SqlAgent agent = config.agent()) {
			ExecutionContext ctx = agent.contextFrom("test/PARAM_MAPPING1").param("targetDate", localDate);

			try (ResultSet rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getDate("TARGET_DATE"), is(new java.sql.Date(date.getTime())));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}

		}
	}

	@Test
	public void testSetInParameter_mapperForOptional() throws ParseException, SQLException {

		LocalDate localDate = LocalDate.of(2002, Month.JANUARY, 1);
		Date date = Date.from(LocalDate.parse("2002-01-01").atStartOfDay(ZoneId.systemDefault()).toInstant());
		try (SqlAgent agent = config.agent()) {
			ExecutionContext ctx = agent.contextFrom("test/PARAM_MAPPING1").param("targetDate", Optional.of(localDate));

			try (ResultSet rs = agent.query(ctx)) {
				assertThat("結果が0件です。", rs.next(), is(true));
				assertThat(rs.getDate("TARGET_DATE"), is(new java.sql.Date(date.getTime())));

				assertThat("取得データが多すぎます", rs.next(), is(false));
			}
		}
	}

	@Test
	public void testSetInParameter_mapperForArray() throws SQLException {

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

		try (SqlAgent agent = config.agent()) {
			ExecutionContext ctx = agent.contextFrom("test/PARAM_MAPPING2").param("targetStr", param);

			try (ResultSet rs = agent.query(ctx)) {
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
	public void testSetInParameter_array() throws SQLException {

		try (SqlAgent agent = config.agent()) {
			ExecutionContext ctx = agent.contextFrom("test/PARAM_MAPPING3").param("targetStrs", List.of("1", "2", "3"));

			try (ResultSet rs = agent.query(ctx)) {
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
	public void testSetInParameter_bytearray() throws SQLException {

		try (SqlAgent agent = config.agent()) {
			agent.updateWith("create table if not exists BYTE_ARRAY_TEST (ID VARCHAR(10), DATA BINARY(10))").count();

			assertThat("更新件数が一致しません",
					agent.updateWith("INSERT INTO BYTE_ARRAY_TEST VALUES (/*id*/, /*data*/)").param("id", 1)
							.param("data", "test".getBytes()).count(),
					is(1));
		}
	}

	@Test
	public void testSetParameter_subParameter() throws ParseException, SQLException {

		try (SqlAgent agent = config.agent()) {
			agent.update("ddl/create_tables").count();

			ColumnTypeTest bean = new ColumnTypeTest("test", 'A', 10, true, new Timestamp(System.currentTimeMillis()),
					new Date(), Time.valueOf("10:20:30"));
			agent.insert(bean);

			String sql = "select * from COLUMN_TYPE_TEST WHERE 1 = 1 /*IF bean.colVarchar != null*/ AND COL_VARCHAR = /*bean.colVarchar*//*END*//*IF bean.colBoolean != null*/ AND COL_BOOLEAN = /*bean.colBoolean*//*END*/";
			List<Map<String, Object>> list = null;

			ColumnTypeChild child1 = new ColumnTypeChild("test", 'X', 0, null, null, null, null);
			list = agent.queryWith(sql).param("bean", child1).collect();
			assertThat(list.size(), is(1));
			assertThat(list.get(0).get("COL_VARCHAR"), is("test"));
			assertThat(list.get(0).get("COL_BOOLEAN"), is(true));

			child1 = new ColumnTypeChild("test2", 'X', 0, null, null, null, null);
			list = agent.queryWith(sql).param("bean", child1).collect();
			assertThat(list.size(), is(0));

			ColumnTypeChild child2 = new ColumnTypeChild(null, 'X', 0, true, null, null, null);
			list = agent.queryWith(sql).param("bean", child2).collect();
			assertThat(list.size(), is(1));
			assertThat(list.get(0).get("COL_VARCHAR"), is("test"));
			assertThat(list.get(0).get("COL_BOOLEAN"), is(true));

			child2 = new ColumnTypeChild(null, 'X', 0, false, null, null, null);
			list = agent.queryWith(sql).param("bean", child2).collect();
			assertThat(list.size(), is(0));
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
			super();
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
