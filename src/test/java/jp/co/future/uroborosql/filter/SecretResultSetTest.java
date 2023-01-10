package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.sql.Date;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Calendar;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.event.subscriber.AbstractSecretColumnEventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SecretColumnEventSubscriber;
import jp.co.future.uroborosql.event.subscriber.SqlFilterManager;
import jp.co.future.uroborosql.event.subscriber.SqlFilterManagerImpl;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.utils.StringUtils;

public class SecretResultSetTest {

	private static SqlConfig config;
	private static SqlFilterManager sqlFilterManager;
	private static AbstractSecretColumnEventSubscriber filter;

	@BeforeAll
	public static void setUpClass() throws Exception {
		sqlFilterManager = new SqlFilterManagerImpl();
		filter = new SecretColumnEventSubscriber();

		filter.setCryptColumnNames(Arrays.asList("PRODUCT_KANA_NAME"));
		// 下記コマンドでkeystoreファイル生成
		// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS
		// -alias testexample
		// -storepass password -keypass password -keyalg AES -keysize 128
		filter.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
		filter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		filter.setAlias("testexample");
		filter.setCharset("UTF-8");
		filter.setTransformationType("AES/ECB/PKCS5Padding");

		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest"))
				.setSqlFilterManager(sqlFilterManager.addSqlFilter(filter)).build();

		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (var sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		} catch (UroborosqlSQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@BeforeEach
	public void setUp() throws Exception {
		try (var agent = config.agent()) {
			var dt = LocalDateTime.of(2017, 1, 2, 12, 23, 30);

			agent.updateWith("truncate table PRODUCT").count();
			agent.update("example/insert_product")
					.param("product_id", 1)
					.param("product_name", "name")
					.param("product_kana_name", "kana")
					.param("jan_code", "1234567890123")
					.param("product_description", "description")
					.param("ins_datetime", dt)
					.param("upd_datetime", dt)
					.param("version_no", 1)
					.count();
		}
	}

	@AfterEach
	public void tearDown() throws Exception {
	}

	@SuppressWarnings("deprecation")
	@Test
	void testQuery() throws Exception {
		try (var agent = config.agent()) {
			try (var rs = agent.query("example/select_product").param("product_id", 1).resultSet()) {
				assertThat(rs.next(), is(true));
				assertThat(rs.getString("PRODUCT_ID"), is("1"));
				assertThat(rs.getString(1), is("1"));
				assertThat(rs.getBoolean("PRODUCT_ID"), is(true));
				assertThat(rs.getBoolean(1), is(true));
				assertThat(rs.getByte("PRODUCT_ID"), is((byte) 1));
				assertThat(rs.getByte(1), is((byte) 1));
				assertThat(rs.getShort("PRODUCT_ID"), is((short) 1));
				assertThat(rs.getShort(1), is((short) 1));
				assertThat(rs.getInt("PRODUCT_ID"), is(1));
				assertThat(rs.getInt(1), is(1));
				assertThat(rs.getLong("PRODUCT_ID"), is(1L));
				assertThat(rs.getLong(1), is(1L));
				assertThat(rs.getFloat("PRODUCT_ID"), is(1.0f));
				assertThat(rs.getFloat(1), is(1.0f));
				assertThat(rs.getDouble("PRODUCT_ID"), is(1.0d));
				assertThat(rs.getDouble(1), is(1.0d));
				assertThat(rs.getBigDecimal("PRODUCT_ID"), is(BigDecimal.ONE));
				assertThat(rs.getBigDecimal(1), is(BigDecimal.ONE));
				assertThat(rs.getBigDecimal("PRODUCT_ID", 0), is(BigDecimal.ONE));
				assertThat(rs.getBigDecimal(1, 0), is(BigDecimal.ONE));
				assertThat(rs.getDate("INS_DATETIME"), is(Date.valueOf("2017-01-02")));
				assertThat(rs.getDate(6), is(Date.valueOf("2017-01-02")));
				assertThat(rs.getDate("INS_DATETIME", Calendar.getInstance()), is(Date.valueOf("2017-01-02")));
				assertThat(rs.getDate(6, Calendar.getInstance()), is(Date.valueOf("2017-01-02")));
				assertThat(rs.getTime("INS_DATETIME"), is(Time.valueOf("12:23:30")));
				assertThat(rs.getTime(6), is(Time.valueOf("12:23:30")));
				assertThat(rs.getTime("INS_DATETIME", Calendar.getInstance()), is(Time.valueOf("12:23:30")));
				assertThat(rs.getTime(6, Calendar.getInstance()), is(Time.valueOf("12:23:30")));
				assertThat(rs.getTimestamp("INS_DATETIME"), is(Timestamp.valueOf("2017-01-02 12:23:30")));
				assertThat(rs.getTimestamp(6), is(Timestamp.valueOf("2017-01-02 12:23:30")));
				assertThat(rs.getTimestamp("INS_DATETIME", Calendar.getInstance()),
						is(Timestamp.valueOf("2017-01-02 12:23:30")));
				assertThat(rs.getTimestamp(6, Calendar.getInstance()),
						is(Timestamp.valueOf("2017-01-02 12:23:30")));
				assertThat(rs.getNString("PRODUCT_ID"), is("1"));
				assertThat(rs.getNString(1), is("1"));
				assertThat(rs.getBlob(1).toString().split(": ")[1], is("1"));
				assertThat(rs.getBlob("PRODUCT_ID").toString().split(": ")[1], is("1"));
				assertThat(rs.getArray(1).toString().split(": ")[1], is("[1]"));
				assertThat(rs.getArray("PRODUCT_ID").toString().split(": ")[1], is("[1]"));
				assertThat(rs.getClob(1).toString().split(": ")[1], is("1"));
				assertThat(rs.getClob("PRODUCT_ID").toString().split(": ")[1], is("1"));
				assertThat(rs.getStatement().getMaxRows(), is(0));
				assertThat(rs.getObject("PRODUCT_ID").getClass().getName(), is("java.math.BigDecimal"));
				assertThat(rs.getObject(1).getClass().getName(), is("java.math.BigDecimal"));
				assertThat(rs.getObject("PRODUCT_ID", String.class).getClass().getName(), is("java.lang.String"));
				assertThat(rs.getObject(1, String.class).getClass().getName(), is("java.lang.String"));
				assertThat(rs.findColumn("PRODUCT_ID"), is(1));
				assertThat(rs.wasNull(), is(false));
				assertThat(rs.isFirst(), is(true));
				assertThat(rs.isLast(), is(true));
				assertThat(rs.isBeforeFirst(), is(false));
				assertThat(rs.isAfterLast(), is(false));
				assertThat(rs.getRow(), is(1));
				assertThat(rs.getHoldability(), is(1));
				assertThat(rs.rowUpdated(), is(false));
				assertThat(rs.rowInserted(), is(false));
				assertThat(rs.rowDeleted(), is(false));
				assertThat(rs.absolute(1), is(true));
				assertThat(rs.relative(1), is(false));
				assertThat(rs.isClosed(), is(false));
				rs.setFetchDirection(1000);
				assertThat(rs.getFetchDirection(), is(1000));
				rs.setFetchSize(0);
				assertThat(rs.getFetchSize(), is(0));
				assertThat(rs.getType(), is(1003));
				assertThat(rs.getConcurrency(), is(1007));
				assertThat(rs.getMetaData().getColumnCount(), is(8));
				rs.clearWarnings();
				assertNull(rs.getWarnings());
			}
		}
	}

	@Test
	void testUpdate() throws Exception {
		try (var agent = config.agent()) {
			var query = agent.query("example/select_product").param("product_id", 1);
			query.context().setResultSetConcurrency(ResultSet.CONCUR_UPDATABLE);
			try (var rs = query.resultSet()) {
				assertThat(rs.next(), is(true));
				rs.updateNull("PRODUCT_KANA_NAME");
				rs.updateNull(2);
				rs.updateRow();
				assertNull(rs.getString("PRODUCT_KANA_NAME"));
				assertNull(rs.getString(2));

				rs.updateBoolean("PRODUCT_KANA_NAME", true);
				rs.updateBoolean(2, false);
				rs.updateRow();
				assertThat(rs.getBoolean("PRODUCT_KANA_NAME"), is(true));
				assertThat(rs.getBoolean(2), is(false));

				rs.updateByte("PRODUCT_KANA_NAME", (byte) 1);
				rs.updateByte(2, (byte) 2);
				rs.updateRow();
				assertThat(rs.getByte("PRODUCT_KANA_NAME"), is((byte) 1));
				assertThat(rs.getByte(2), is((byte) 2));

				rs.updateShort("PRODUCT_KANA_NAME", (short) 3);
				rs.updateShort(2, (short) 4);
				rs.updateRow();
				assertThat(rs.getShort("PRODUCT_KANA_NAME"), is((short) 3));
				assertThat(rs.getShort(2), is((short) 4));

				rs.updateInt("PRODUCT_KANA_NAME", 5);
				rs.updateInt(2, 6);
				rs.updateRow();
				assertThat(rs.getInt("PRODUCT_KANA_NAME"), is(5));
				assertThat(rs.getInt(2), is(6));

				rs.updateLong("PRODUCT_KANA_NAME", 7L);
				rs.updateLong(2, 8L);
				rs.updateRow();
				assertThat(rs.getLong("PRODUCT_KANA_NAME"), is(7L));
				assertThat(rs.getLong(2), is(8L));

				rs.updateFloat("PRODUCT_KANA_NAME", 8.0f);
				rs.updateFloat(2, 8.1f);
				rs.updateRow();
				assertThat(rs.getFloat("PRODUCT_KANA_NAME"), is(8.0f));
				assertThat(rs.getFloat(2), is(8.1f));

				rs.updateDouble("PRODUCT_KANA_NAME", 9.0D);
				rs.updateDouble(2, 9.1D);
				rs.updateRow();
				assertThat(rs.getDouble("PRODUCT_KANA_NAME"), is(9.0D));
				assertThat(rs.getDouble(2), is(9.1D));

				rs.updateBigDecimal("PRODUCT_KANA_NAME", BigDecimal.ZERO);
				rs.updateBigDecimal(2, BigDecimal.ONE);
				rs.updateRow();
				assertThat(rs.getBigDecimal("PRODUCT_KANA_NAME"), is(BigDecimal.ZERO));
				assertThat(rs.getBigDecimal(2), is(BigDecimal.ONE));

				rs.updateString("PRODUCT_KANA_NAME", "string1");
				rs.updateString(2, "string2");
				rs.updateRow();
				assertThat(rs.getString("PRODUCT_KANA_NAME"), is("string1"));
				assertThat(rs.getString(2), is("string2"));

				rs.updateDate("PRODUCT_KANA_NAME", Date.valueOf("2017-06-01"));
				rs.updateDate(2, Date.valueOf("2017-06-02"));
				rs.updateRow();
				assertThat(rs.getDate("PRODUCT_KANA_NAME"), is(Date.valueOf("2017-06-01")));
				assertThat(rs.getDate(2), is(Date.valueOf("2017-06-02")));

				rs.updateTime("PRODUCT_KANA_NAME", Time.valueOf("10:10:10"));
				rs.updateTime(2, Time.valueOf("10:10:11"));
				rs.updateRow();
				assertThat(rs.getTime("PRODUCT_KANA_NAME"), is(Time.valueOf("10:10:10")));
				assertThat(rs.getTime(2), is(Time.valueOf("10:10:11")));

				rs.updateTimestamp("PRODUCT_KANA_NAME", Timestamp.valueOf("2005-12-12 10:10:10"));
				rs.updateTimestamp(2, Timestamp.valueOf("2005-12-12 10:10:11"));
				rs.updateRow();
				assertThat(rs.getTimestamp("PRODUCT_KANA_NAME"), is(Timestamp.valueOf("2005-12-12 10:10:10")));
				assertThat(rs.getTimestamp(2), is(Timestamp.valueOf("2005-12-12 10:10:11")));

				rs.updateNString("PRODUCT_KANA_NAME", "string1");
				rs.updateNString(2, "string2");
				rs.updateRow();
				assertThat(rs.getNString("PRODUCT_KANA_NAME"), is("string1"));
				assertThat(rs.getNString(2), is("string2"));
			}
		}
	}

	@Test
	void testIsClose() throws Exception {
		try (var agent = config.agent()) {
			ResultSet rs = null;
			try {
				rs = agent.query("example/select_product").param("product_id", 1).resultSet();
				assertThat(rs.isClosed(), is(false));
			} finally {
				if (rs != null) {
					rs.close();

					assertThat(rs.isClosed(), is(true));
				}
			}
		}
	}
}
