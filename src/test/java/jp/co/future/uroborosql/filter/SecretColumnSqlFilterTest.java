package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Date;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.SqlAgent;
import jp.co.future.uroborosql.config.DefaultSqlConfig;
import jp.co.future.uroborosql.config.SqlConfig;

public class SecretColumnSqlFilterTest {

	private SqlConfig config;
	private SqlFilterManager sqlFilterManager;
	private SecretColumnSqlFilter filter;

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest"));
		sqlFilterManager = config.getSqlFilterManager();
		filter = new SecretColumnSqlFilter();
		sqlFilterManager.addSqlFilter(filter);

		filter.setCryptColumnNames(Arrays.asList("PRODUCT_NAME"));
		// 下記コマンドでkeystoreファイル生成
		// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS
		// -alias testexample
		// -storepass password -keypass password -keyalg AES -keysize 128
		filter.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnSqlFilter/keystore.jceks");
		filter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		filter.setAlias("testexample");
		filter.setCharset("UTF-8");
		filter.setTransformationType("AES/ECB/PKCS5Padding");
		sqlFilterManager.initialize();

		try (SqlAgent agent = config.createAgent()) {

			String[] sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		} catch (SQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				String[] parts = line.split("\t");
				for (String part : parts) {
					String[] keyValue = part.split(":", 2);
					row.put(keyValue[0].toLowerCase(), StringUtils.isBlank(keyValue[1]) ? null : keyValue[1]);
				}
				ans.add(row);
			});
		} catch (IOException e) {
			e.printStackTrace();
		}
		return ans;
	}

	private void truncateTable(final Object... tables) {
		try (SqlAgent agent = config.createAgent()) {
			Arrays.asList(tables).stream().forEach(tbl -> {
				try {
					agent.updateWith("truncate table " + tbl.toString()).count();
				} catch (SQLException ex) {
					ex.printStackTrace();
					fail("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage());
				}
			});
		} catch (SQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	private void cleanInsert(final Path path) {
		List<Map<String, Object>> dataList = getDataFromFile(path);

		try (SqlAgent agent = config.createAgent()) {
			dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
					.forEach(tbl -> truncateTable(tbl));

			dataList.stream().forEach(map -> {
				try {

					agent.update(map.get("sql").toString()).paramMap(map).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					fail("TABLE:" + map.get("table") + " insert is miss. ex:" + ex.getMessage());
				}
			});

		} catch (SQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@Test
	public void testFilterSettings() {
		assertThat(filter.getCharset(), is(StandardCharsets.UTF_8));
		assertThat(filter.getTransformationType(), is("AES/ECB/PKCS5Padding"));
		assertThat(filter.isSkipFilter(), is(false));
	}

	@Test
	public void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// skipFilter = falseの別のフィルター設定
		SqlConfig skipConfig = DefaultSqlConfig
				.getConfig(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest"));
		SqlFilterManager skipSqlFilterManager = skipConfig.getSqlFilterManager();
		SecretColumnSqlFilter skipFilter = new SecretColumnSqlFilter();
		skipSqlFilterManager.addSqlFilter(skipFilter);

		skipFilter.setCryptColumnNames(Arrays.asList("PRODUCT_NAME"));
		skipFilter.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnSqlFilter/keystore.jceks");
		skipFilter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		skipFilter.setAlias("testexample");
		skipFilter.setSkipFilter(true);

		// 復号化しないで取得した場合 (skipFilter = true)
		try (SqlAgent agent = skipConfig.createAgent()) {
			ResultSet result = agent.query("example/select_product").param("product_id", new BigDecimal(0)).resultSet();

			while (result.next()) {
				assertEquals(result.getString("PRODUCT_NAME"), "3EniRr6_Jb2c-kVG0I0CgA");
			}

		}

		// 復号化して取得した場合 (skipFilter = false)
		try (SqlAgent agent = config.createAgent()) {
			ResultSet result = agent.query("example/select_product").param("product_id", new BigDecimal(0)).resultSet();

			while (result.next()) {
				assertThat(result.getBigDecimal("PRODUCT_ID"), is(BigDecimal.ZERO));
				assertThat(result.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getString("JAN_CODE"), is("1234567890123"));
				assertThat(result.getString("PRODUCT_DESCRIPTION"), is("0番目の商品"));
				assertThat(result.getTimestamp("INS_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10.0")));
				assertThat(result.getTimestamp("UPD_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10.0")));
				assertThat(result.getBigDecimal("VERSION_NO"), is(BigDecimal.ZERO));
			}
		}
	};

	@Test
	public void testSecretResultSet() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// 復号化して取得した場合 (skipFilter = false)
		try (SqlAgent agent = config.createAgent()) {
			ResultSet result = agent.query("example/select_product").param("product_id", new BigDecimal(0)).resultSet();

			while (result.next()) {
				assertThat(result.getString("PRODUCT_ID"), is("0"));
				assertThat(result.getString(1), is("0"));
				assertThat(result.getBoolean("PRODUCT_ID"), is(false));
				assertThat(result.getBoolean(1), is(false));
				assertThat(result.getByte("PRODUCT_ID"), is((byte) 0));
				assertThat(result.getByte(1), is((byte) 0));
				assertThat(result.getShort("PRODUCT_ID"), is((short) 0));
				assertThat(result.getShort(1), is((short) 0));
				assertThat(result.getInt("PRODUCT_ID"), is((int) 0));
				assertThat(result.getInt(1), is((int) 0));
				assertThat(result.getLong("PRODUCT_ID"), is(0L));
				assertThat(result.getLong(1), is(0L));
				assertThat(result.getFloat("PRODUCT_ID"), is(0.0f));
				assertThat(result.getFloat(1), is(0.0f));
				assertThat(result.getDouble("PRODUCT_ID"), is(0.0d));
				assertThat(result.getDouble(1), is(0.0d));
				assertThat(result.getBigDecimal("PRODUCT_ID"), is(BigDecimal.ZERO));
				assertThat(result.getBigDecimal(1), is(BigDecimal.ZERO));
				assertThat(result.getBigDecimal("PRODUCT_ID", 0), is(BigDecimal.ZERO));
				assertThat(result.getBigDecimal(1, 0), is(BigDecimal.ZERO));
				assertThat(result.getDate("INS_DATETIME"), is(Date.valueOf("2005-12-12")));
				assertThat(result.getDate(6), is(Date.valueOf("2005-12-12")));
				assertThat(result.getDate("INS_DATETIME", Calendar.getInstance()), is(Date.valueOf("2005-12-12")));
				assertThat(result.getDate(6, Calendar.getInstance()), is(Date.valueOf("2005-12-12")));
				assertThat(result.getTime("INS_DATETIME"), is(Time.valueOf("10:10:10")));
				assertThat(result.getTime(6), is(Time.valueOf("10:10:10")));
				assertThat(result.getTime("INS_DATETIME", Calendar.getInstance()), is(Time.valueOf("10:10:10")));
				assertThat(result.getTime(6, Calendar.getInstance()), is(Time.valueOf("10:10:10")));
				assertThat(result.getTimestamp("INS_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10")));
				assertThat(result.getTimestamp(6), is(Timestamp.valueOf("2005-12-12 10:10:10")));
				assertThat(result.getTimestamp("INS_DATETIME", Calendar.getInstance()), is(Timestamp.valueOf("2005-12-12 10:10:10")));
				assertThat(result.getTimestamp(6, Calendar.getInstance()), is(Timestamp.valueOf("2005-12-12 10:10:10")));
				assertThat(result.getNString("PRODUCT_ID"), is("0"));
				assertThat(result.getNString(1), is("0"));
				assertThat(result.findColumn("PRODUCT_ID"), is(1));
				assertThat(result.wasNull(), is(false));
				assertThat(result.isFirst(), is(true));
				assertThat(result.isLast(), is(true));
				assertThat(result.isBeforeFirst(), is(false));
				assertThat(result.isAfterLast(), is(false));
				assertThat(result.getRow(), is(1));
				assertThat(result.getHoldability(), is(1));
				assertThat(result.absolute(1), is(true));
				assertThat(result.relative(1), is(false));
				assertThat(result.isClosed(), is(false));
				result.setFetchDirection(1000);
				assertThat(result.getFetchDirection(), is(1000));
				result.setFetchSize(0);
				assertThat(result.getFetchSize(), is(0));
				assertThat(result.getType(), is(1003));
				assertThat(result.getConcurrency(), is(1007));
				result.clearWarnings();
				assertNull(result.getWarnings());
			}
			result.close();
		}
	};
}
