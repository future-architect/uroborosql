package jp.co.future.uroborosql.parameter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.ParseException;
import java.time.LocalDate;
import java.time.Month;
import java.util.Date;
import java.util.Optional;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.context.SqlContext;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapper;
import jp.co.future.uroborosql.parameter.mapper.BindParameterMapperManager;

import org.apache.commons.lang3.time.DateUtils;
import org.junit.Before;
import org.junit.Test;

public class ParameterTest {
	private SqlConfig config;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder("jdbc:h2:mem:ParameterTest", null, null).build();
	}

	@Test
	public void testSetInParameter_mapperForDate() throws ParseException, SQLException {

		Date date = DateUtils.parseDate("2002-01-01", new String[] { "yyyy-MM-dd" });
		try (SqlAgent agent = config.agent()) {
			SqlContext ctx = agent.contextFrom("test/PARAM_MAPPING1").param("targetDate", date);

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
		Date date = DateUtils.parseDate("2002-01-01", new String[] { "yyyy-MM-dd" });
		try (SqlAgent agent = config.agent()) {
			SqlContext ctx = agent.contextFrom("test/PARAM_MAPPING1").param("targetDate", localDate);

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
		Date date = DateUtils.parseDate("2002-01-01", new String[] { "yyyy-MM-dd" });
		try (SqlAgent agent = config.agent()) {
			SqlContext ctx = agent.contextFrom("test/PARAM_MAPPING1").param("targetDate", Optional.of(localDate));

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

		config.getSqlContextFactory().addBindParamMapper(new BindParameterMapper<String[]>() {

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
			SqlContext ctx = agent.contextFrom("test/PARAM_MAPPING2").param("targetStr", param);

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
			SqlContext ctx = agent.contextFrom("test/PARAM_MAPPING3").paramList("targetStrs", "1", "2", "3");

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
							.param("data", "test".getBytes()).count(), is(1));
		}
	}
}
