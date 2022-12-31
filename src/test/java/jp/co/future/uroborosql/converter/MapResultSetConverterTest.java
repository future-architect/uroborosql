package jp.co.future.uroborosql.converter;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.io.ByteArrayInputStream;
import java.io.StringReader;
import java.math.BigDecimal;
import java.sql.DriverManager;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.Month;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.dialect.Oracle12Dialect;
import jp.co.future.uroborosql.dialect.PostgresqlDialect;
import jp.co.future.uroborosql.mapping.mapper.PropertyMapperManager;
import jp.co.future.uroborosql.utils.CaseFormat;
import jp.co.future.uroborosql.utils.StringUtils;

public class MapResultSetConverterTest {
	private String url;
	private static final String INSERT_SQL = "insert into COLUMN_TYPE_TEST2 (" +
			"	COL_INT," +
			"	COL_BOOLEAN," +
			"	COL_TINYINT," +
			"	COL_SMALLINT," +
			"	COL_BIGINT," +
			"	COL_DECIMAL," +
			"	COL_DOUBLE," +
			"	COL_REAL," +
			"	COL_TIME," +
			"	COL_DATE," +
			"	COL_TIMESTAMP," +
			//				"	COL_TIMESTAMPWTZ," +
			"	COL_BINARY," +
			"	COL_CHAR," +
			"	COL_NCHAR," +
			"	COL_VARCHAR," +
			"	COL_NVARCHAR," +
			"	COL_LONGVARCHAR," +
			"	COL_CLOB," +
			"	COL_NCLOB," +
			"	COL_BLOB," +
			"	COL_ARRAY" +
			") VALUES (" +
			"	/*int*/, " +
			"	/*boolean*/, " +
			"	/*tinyint*/, " +
			"	/*smallint*/, " +
			"	/*bigint*/, " +
			"	/*decimal*/, " +
			"	/*double*/, " +
			"	/*real*/, " +
			"	/*time*/, " +
			"	/*date*/, " +
			"	/*timestamp*/, " +
			//				"	/*timestampwtz*/, " +
			"	/*binary*/, " +
			"	/*char*/, " +
			"	/*nchar*/, " +
			"	/*varchar*/, " +
			"	/*nvarchar*/, " +
			"	/*longvarchar*/, " +
			"	/*clob*/, " +
			"	/*nclob*/, " +
			"	/*blob*/, " +
			"	/*arr*/" +
			")";

	@Before
	public void setUp() {
		url = "jdbc:h2:mem:" + this.getClass().getSimpleName() + System.currentTimeMillis();
	}

	@Test
	public void testCreateRecord() throws Exception {
		var config = UroboroSQL.builder(DriverManager.getConnection(url))
				.build();
		try (var agent = config.agent()) {

			createTable(agent);

			var clob = StringUtils.repeat('1', 10000);
			var nclob = StringUtils.repeat('あ', 10000);
			var binary = StringUtils.repeat('y', 20000).getBytes();
			var blob = StringUtils.repeat('x', 20000).getBytes();
			int[] arr = { 1, 2 };
			var time = LocalTime.of(10, 0, 0);
			var date = ZonedDateTime.of(LocalDateTime.of(2019, Month.MAY, 1, 0, 0, 0),
					config.getClock().getZone());
			var timestamp = Timestamp.valueOf(LocalDateTime.of(2019, Month.MAY, 1, 10, 30, 0));

			agent.updateWith(INSERT_SQL)
					.param("int", 1)
					.param("boolean", true)
					.param("tinyint", 2)
					.param("smallint", 3)
					.param("bigint", 4L)
					.param("decimal", new BigDecimal(5))
					.param("double", 6d)
					.param("real", 7f)
					.param("time", time)
					.param("date", date)
					.param("timestamp", timestamp)
					// h2db timestamp with time zone is not support rs.getTimestamp()
					//				.param("timestampwtz",
					//						ZonedDateTime.of(LocalDateTime.of(2019, Month.MAY, 1, 10, 45), config.getClock().getZone()))
					.param("binary", binary)
					.param("char", 'a')
					.param("nchar", 'あ')
					.param("varchar", "abc")
					.param("nvarchar", "文字列")
					.param("longvarchar", "abcabc")
					.clobParam("clob", new StringReader(clob))
					.clobParam("nclob", new StringReader(nclob))
					.blobParam("blob", new ByteArrayInputStream(blob))
					.param("arr", arr)
					.count();

			var optional = agent.queryWith("select * from COLUMN_TYPE_TEST2").findFirst();
			assertThat(optional.isPresent(), is(true));
			var row = optional.get();

			assertThat(row.get("COL_INT"), is(1));
			assertThat(row.get("COL_BOOLEAN"), is(true));
			assertThat(row.get("COL_TINYINT"), is((byte) 2));
			assertThat(row.get("COL_SMALLINT"), is((short) 3));
			assertThat(row.get("COL_BIGINT"), is(4L));
			assertThat(row.get("COL_DECIMAL"), is(new BigDecimal(5)));
			assertThat(row.get("COL_DOUBLE"), is(6d));
			assertThat(row.get("COL_REAL"), is(7f));
			assertThat(row.get("COL_TIME"), is(time));
			assertThat(row.get("COL_DATE"), is(date));
			assertThat(row.get("COL_TIMESTAMP"), is(timestamp));
			assertThat(row.get("COL_BINARY"), is(binary));
			assertThat(row.get("COL_CHAR"), is("a"));
			assertThat(row.get("COL_NCHAR"), is("あ"));
			assertThat(row.get("COL_VARCHAR"), is("abc"));
			assertThat(row.get("COL_NVARCHAR"), is("文字列"));
			assertThat(row.get("COL_LONGVARCHAR"), is("abcabc"));
			assertThat(row.get("COL_CLOB"), is(clob));
			assertThat(row.get("COL_NCLOB"), is(nclob));
			assertThat(row.get("COL_BLOB"), is(blob));
			assertThat(row.get("COL_ARRAY"), is(arr));
		}
	}

	@Test
	public void testCreateRecordWithDialect() throws Exception {
		var config = UroboroSQL.builder(DriverManager.getConnection(url))
				.setDialect(new Oracle12Dialect())
				.build();
		try (var agent = config.agent()) {

			createTable(agent);

			var clob = StringUtils.repeat('1', 10000);
			var nclob = StringUtils.repeat('あ', 10000);
			var binary = StringUtils.repeat('y', 20000).getBytes();
			var blob = StringUtils.repeat('x', 20000).getBytes();
			int[] arr = { 1, 2 };
			var time = LocalTime.of(10, 0, 0);
			var date = ZonedDateTime.of(LocalDateTime.of(2019, Month.MAY, 1, 0, 0, 0),
					config.getClock().getZone());
			var timestamp = Timestamp.valueOf(LocalDateTime.of(2019, Month.MAY, 1, 10, 30, 0));

			agent.updateWith(INSERT_SQL)
					.param("int", 1)
					.param("boolean", true)
					.param("tinyint", 2)
					.param("smallint", 3)
					.param("bigint", 4L)
					.param("decimal", new BigDecimal(5))
					.param("double", 6d)
					.param("real", 7f)
					.param("time", time)
					.param("date", date)
					.param("timestamp", timestamp)
					// h2db timestamp with time zone is not support rs.getTimestamp()
					//				.param("timestampwtz",
					//						ZonedDateTime.of(LocalDateTime.of(2019, Month.MAY, 1, 10, 45), config.getClock().getZone()))
					.param("binary", binary)
					.param("char", 'a')
					.param("nchar", 'あ')
					.param("varchar", "abc")
					.param("nvarchar", "文字列")
					.param("longvarchar", "abcabc")
					.clobParam("clob", new StringReader(clob))
					.clobParam("nclob", new StringReader(nclob))
					.blobParam("blob", new ByteArrayInputStream(blob))
					.param("arr", arr)
					.count();

			// constractor MapResultSetConverter(final Dialect dialect)
			var context = config.contextWith("select * from COLUMN_TYPE_TEST2");
			List<Map<String, Object>> result = agent.query(context, new MapResultSetConverter(config))
					.collect(Collectors.toList());
			assertThat(result.size(), is(1));
			var row = result.get(0);

			assertThat(row.get("COL_INT"), is(1));
			assertThat(row.get("COL_BOOLEAN"), is(true));
			assertThat(row.get("COL_TINYINT"), is((byte) 2));
			assertThat(row.get("COL_SMALLINT"), is((short) 3));
			assertThat(row.get("COL_BIGINT"), is(4L));
			assertThat(row.get("COL_DECIMAL"), is(new BigDecimal(5)));
			assertThat(row.get("COL_DOUBLE"), is(6d));
			assertThat(row.get("COL_REAL"), is(7f));
			assertThat(row.get("COL_TIME"), is(time));
			assertThat(row.get("COL_DATE"), is(date));
			assertThat(row.get("COL_TIMESTAMP"), is(timestamp));
			assertThat(row.get("COL_BINARY"), is(binary));
			assertThat(row.get("COL_CHAR"), is("a"));
			assertThat(row.get("COL_NCHAR"), is("あ"));
			assertThat(row.get("COL_VARCHAR"), is("abc"));
			assertThat(row.get("COL_NVARCHAR"), is("文字列"));
			assertThat(row.get("COL_LONGVARCHAR"), is("abcabc"));
			assertThat(row.get("COL_CLOB"), is(clob));
			assertThat(row.get("COL_NCLOB"), is(nclob));
			assertThat(row.get("COL_BLOB"), is(blob));
			assertThat(row.get("COL_ARRAY"), is(arr));
		}
	}

	@Test
	public void testCreateRecordWithSnakeCase() throws Exception {
		var config = UroboroSQL.builder(DriverManager.getConnection(url))
				.setDialect(new PostgresqlDialect())
				.build();
		try (var agent = config.agent()) {

			createTable(agent);

			var clob = StringUtils.repeat('1', 10000);
			var nclob = StringUtils.repeat('あ', 10000);
			var binary = StringUtils.repeat('y', 20000).getBytes();
			var blob = StringUtils.repeat('x', 20000).getBytes();
			int[] arr = { 1, 2 };
			var time = LocalTime.of(10, 0, 0);
			var date = ZonedDateTime.of(LocalDateTime.of(2019, Month.MAY, 1, 0, 0, 0),
					config.getClock().getZone());
			var timestamp = Timestamp.valueOf(LocalDateTime.of(2019, Month.MAY, 1, 10, 30, 0));

			agent.updateWith(INSERT_SQL)
					.param("int", 1)
					.param("boolean", true)
					.param("tinyint", 2)
					.param("smallint", 3)
					.param("bigint", 4L)
					.param("decimal", new BigDecimal(5))
					.param("double", 6d)
					.param("real", 7f)
					.param("time", time)
					.param("date", date)
					.param("timestamp", timestamp)
					// h2db timestamp with time zone is not support rs.getTimestamp()
					//				.param("timestampwtz",
					//						ZonedDateTime.of(LocalDateTime.of(2019, Month.MAY, 1, 10, 45), config.getClock().getZone()))
					.param("binary", binary)
					.param("char", 'a')
					.param("nchar", 'あ')
					.param("varchar", "abc")
					.param("nvarchar", "文字列")
					.param("longvarchar", "abcabc")
					.clobParam("clob", new StringReader(clob))
					.clobParam("nclob", new StringReader(nclob))
					.blobParam("blob", new ByteArrayInputStream(blob))
					.param("arr", arr)
					.count();

			// constractor MapResultSetConverter(final Dialect dialect)
			var context = config.contextWith("select * from COLUMN_TYPE_TEST2");
			List<Map<String, Object>> result = agent
					.query(context,
							new MapResultSetConverter(config, CaseFormat.UPPER_SNAKE_CASE,
									new PropertyMapperManager(config.getClock())))
					.collect(Collectors.toList());
			assertThat(result.size(), is(1));
			var row = result.get(0);

			assertThat(row.get("COL_INT"), is(1));
			assertThat(row.get("COL_BOOLEAN"), is(true));
			assertThat(row.get("COL_TINYINT"), is((byte) 2));
			assertThat(row.get("COL_SMALLINT"), is((short) 3));
			assertThat(row.get("COL_BIGINT"), is(4L));
			assertThat(row.get("COL_DECIMAL"), is(new BigDecimal(5)));
			assertThat(row.get("COL_DOUBLE"), is(6d));
			assertThat(row.get("COL_REAL"), is(7f));
			assertThat(row.get("COL_TIME"), is(time));
			assertThat(row.get("COL_DATE"), is(date));
			assertThat(row.get("COL_TIMESTAMP"), is(timestamp));
			assertThat(row.get("COL_BINARY"), is(binary));
			assertThat(row.get("COL_CHAR"), is("a"));
			assertThat(row.get("COL_NCHAR"), is("あ"));
			assertThat(row.get("COL_VARCHAR"), is("abc"));
			assertThat(row.get("COL_NVARCHAR"), is("文字列"));
			assertThat(row.get("COL_LONGVARCHAR"), is("abcabc"));
			assertThat(row.get("COL_CLOB"), is(clob));
			assertThat(row.get("COL_NCLOB"), is(nclob));
			assertThat(row.get("COL_BLOB"), is(blob));
			assertThat(row.get("COL_ARRAY"), is(arr));
		}
	}

	protected void createTable(final SqlAgent agent) {
		var createTableSql = "create table if not exists COLUMN_TYPE_TEST2 (" +
				"	COL_INT				INT," +
				"	COL_BOOLEAN			BOOLEAN," +
				"	COL_TINYINT			TINYINT," +
				"	COL_SMALLINT		SMALLINT," +
				"	COL_BIGINT			BIGINT," +
				"	COL_DECIMAL			DECIMAL," +
				"	COL_DOUBLE			DOUBLE," +
				"	COL_REAL			REAL," +
				"	COL_TIME			TIME," +
				"	COL_DATE			DATE," +
				"	COL_TIMESTAMP		TIMESTAMP(9)," +
				"	COL_TIMESTAMPWTZ	TIMESTAMP(9) WITH TIME ZONE," +
				"	COL_BINARY			BINARY," +
				"	COL_CHAR			CHAR," +
				"	COL_NCHAR			NCHAR," +
				"	COL_VARCHAR			VARCHAR," +
				"	COL_NVARCHAR		NVARCHAR," +
				"	COL_LONGVARCHAR		LONGVARCHAR," +
				"	COL_CLOB			CLOB," +
				"	COL_NCLOB			NCLOB," +
				"	COL_BLOB			BLOB," +
				"	COL_ARRAY			ARRAY" +
				");";
		agent.updateWith(createTableSql).count();
	}

}
