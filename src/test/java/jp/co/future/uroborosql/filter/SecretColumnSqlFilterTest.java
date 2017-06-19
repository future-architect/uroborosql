package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
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
			Map<String, Object> result = agent.query("example/select_product").param("product_id", new BigDecimal(0))
					.first();

			assertEquals(result.get("PRODUCT_NAME"), "3EniRr6_Jb2c-kVG0I0CgA");

		}

		// 復号化して取得した場合 (skipFilter = false)
		try (SqlAgent agent = config.createAgent()) {
			Map<String, Object> result = agent.query("example/select_product").param("product_id", new BigDecimal(0))
					.first();

			assertThat(result.get("PRODUCT_ID"), is(new BigDecimal("0")));
			assertThat(result.get("PRODUCT_NAME"), is("商品名0"));
			assertThat(result.get("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
			assertThat(result.get("JAN_CODE"), is("1234567890123"));
			assertThat(result.get("PRODUCT_DESCRIPTION"), is("0番目の商品"));
			assertThat(result.get("INS_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10.0")));
			assertThat(result.get("UPD_DATETIME"), is(Timestamp.valueOf("2005-12-12 10:10:10.0")));
			assertThat(result.get("VERSION_NO"), is(new BigDecimal("0")));
		}
	};
}
