package jp.co.future.uroborosql.filter;

import static org.junit.Assert.*;

import java.io.IOException;
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

	@Before
	public void setUp() throws Exception {
		config = DefaultSqlConfig.getConfig(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest"));
		SqlFilterManager sqlFilterManager = config.getSqlFilterManager();
		SecretColumnSqlFilter filter = new SecretColumnSqlFilter();
		sqlFilterManager.addSqlFilter(filter);

		filter.setCryptColumnNames(Arrays.asList("JAN_CODE", "PRODUCT_NAME"));
		// 下記コマンドでkeystoreファイル生成
		// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS
		// -alias testexample
		// -storepass password -keypass password -keyalg AES -keysize 128
		filter.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnSqlFilter/keystore.jceks");
		filter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		filter.setAlias("testexample");
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
				} catch (SQLException ex) {
					ex.printStackTrace();
					fail("TABLE:" + map.get("TABLE") + " insert is miss. ex:" + ex.getMessage());
				}
			});
		} catch (SQLException ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@Test
	public void testExecuteQueryFilter() throws Exception {
		// cleanInsert(Paths.get("src/test/resources/data/setup",
		// "testExecuteSecretQuery.ltsv"));

		try (SqlAgent agent = config.createAgent()) {
			Timestamp currentDatetime = Timestamp.valueOf("2005-12-12 10:10:10.000000000");

			// Map<String, Object> result =
			// agent.query("example/select_product").param("product_id", new
			// BigDecimal(0))
			// .first();
			//
			// System.out.println(result);

			// SqlContext ctx =
			// agent.contextFrom("example/insert_product").setSqlId("333")
			// .param("product_id", new BigDecimal(1)).param("product_name",
			// "商品名1")
			// .param("product_kana_name", "ショウヒンメイイチ").param("jan_code",
			// "1234567890123")
			// .param("product_description", "1番目の商品").param("ins_datetime",
			// currentDatetime)
			// .param("upd_datetime", currentDatetime).param("version_no", new
			// BigDecimal(0)).addBatch()
			// .param("product_id", new BigDecimal(2)).param("product_name",
			// "商品名2")
			// .param("product_kana_name", "ショウヒンメイニ").param("jan_code",
			// "1234567890124")
			// .param("product_description", "2番目の商品").param("ins_datetime",
			// currentDatetime)
			// .param("upd_datetime", currentDatetime).param("version_no", new
			// BigDecimal(0))
			// .param("_userName", "testUserName").param("_funcId",
			// "testFunction").addBatch();
			//
			// agent.batch(ctx);
			//
			// Map<String, Object> result =
			// agent.query("example/select_product").param("product_id", new
			// BigDecimal(1))
			// .first();
			// System.out.println(result);
		}
	};

	@Test
	public void testExecuteUpdateFilter() throws Exception {
		// 更新時の暗号化テスト
	}

	@Test
	public void testExecuteInsertFilter() throws Exception {
		// 登録時の暗号化テスト
	}
}
