package jp.co.future.uroborosql.filter;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.utils.StringUtils;

/**
 * Test case of SecretColumnSqlFilter when using CBC mode
 *
 * @author hoshi
 *
 */
public class SecretColumnSqlFilterUseCbcTest {

	private SqlConfig config;

	private SqlFilterManager sqlFilterManager;

	private AbstractSecretColumnSqlFilter filter;

	@BeforeEach
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest")).build();
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
		filter.setTransformationType("AES/CBC/PKCS5Padding");
		sqlFilterManager.initialize();

		try (var agent = config.agent()) {
			var sqls = new String(Files.readAllBytes(Paths.get("src/test/resources/sql/ddl/create_tables.sql")),
					StandardCharsets.UTF_8).split(";");
			for (String sql : sqls) {
				if (StringUtils.isNotBlank(sql)) {
					agent.updateWith(sql.trim()).count();
				}
			}
			agent.commit();
		} catch (UroborosqlSQLException ex) {
			ex.printStackTrace();
			assertThat(ex.getMessage(), false);
		}
	}

	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				var parts = line.split("\t");
				for (String part : parts) {
					var keyValue = part.split(":", 2);
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
		try {
			Arrays.asList(tables).stream().forEach(tbl -> {
				try (var agent = config.agent()) {
					agent.updateWith("truncate table " + tbl.toString()).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					assertThat("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage(), false);
				}
			});
		} catch (Exception ex) {
			ex.printStackTrace();
			assertThat(ex.getMessage(), false);
		}
	}

	private void cleanInsert(final Path path) {
		var dataList = getDataFromFile(path);

		try {
			dataList.stream().map(map -> map.get("table")).collect(Collectors.toSet())
					.forEach(this::truncateTable);

			dataList.stream().forEach(map -> {
				try (var agent = config.agent()) {
					agent.update(map.get("sql").toString()).paramMap(map).count();
				} catch (Exception ex) {
					ex.printStackTrace();
					assertThat("TABLE:" + map.get("table") + " insert is miss. ex:" + ex.getMessage(), false);
				}
			});

		} catch (Exception ex) {
			ex.printStackTrace();
			assertThat(ex.getMessage(), false);
		}
	}

	@Test
	public void testFilterSettings() {
		assertThat(filter.getCharset(), is(StandardCharsets.UTF_8));
		assertThat(filter.getTransformationType(), is("AES/CBC/PKCS5Padding"));
		assertThat(filter.isSkipFilter(), is(false));
		assertThat(filter.isUseIV(), is(true));
		assertThat(filter.getSecretKey().getAlgorithm(), is("AES"));
	}

	@Test
	public void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// skipFilter = falseの別のフィルター設定
		var skipConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest"))
				.build();
		var skipSqlFilterManager = skipConfig.getSqlFilterManager();
		AbstractSecretColumnSqlFilter skipFilter = new SecretColumnSqlFilter();
		skipSqlFilterManager.addSqlFilter(skipFilter);

		skipFilter.setCryptColumnNames(Arrays.asList("PRODUCT_NAME"));
		skipFilter.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnSqlFilter/keystore.jceks");
		skipFilter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		skipFilter.setAlias("testexample");
		skipFilter.setSkipFilter(true);

		// 復号化しないで取得した場合 (skipFilter = true)
		try (var skipAgent = skipConfig.agent()) {
			var result = skipAgent.query("example/select_product").param("product_id", new BigDecimal(0))
					.resultSet();

			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is(not("商品名0")));
			}
			result.close();
		}

		// 復号化して取得した場合 (skipFilter = false)
		try (var agent = config.agent()) {
			var result = agent.query("example/select_product").param("product_id", new BigDecimal(0)).resultSet();

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
			result.close();
		}
	}

	@Test
	public void testSecretResultSet01() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			var result = agent.query("example/select_product")
					.param("product_id", new BigDecimal(0)).resultSet();

			while (result.next()) {
				assertThat(result.getString("PRODUCT_ID"), is("0"));
				assertThat(result.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_KANA_NAME", String.class), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_ID"), is(BigDecimal.ZERO));
				assertThat(result.getObject("PRODUCT_ID", Integer.class), is(0));
			}
			result.close();
		}
	}

	@Test
	public void testSecretResultSet02() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			var ctx = agent.contextFrom("example/select_product").param("product_id", new BigDecimal(0));

			var result = agent.query(ctx);
			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME", String.class), is("商品名0"));
			}
			result.close();
		}
	}

	@Test
	public void testSecretResultSet03() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			var ctx = agent.contextFrom("example/select_product").param("product_id", new BigDecimal(0));
			ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);

			var result = agent.query(ctx);
			while (result.next()) {
				result.first();
				assertThat(result.isFirst(), is(true));
				result.previous();
				assertThat(result.isBeforeFirst(), is(true));
				result.next();
				assertThat(result.isBeforeFirst(), is(false));
				result.last();
				assertThat(result.isLast(), is(true));
				result.next();
				assertThat(result.isAfterLast(), is(true));
				result.previous();
				assertThat(result.isAfterLast(), is(false));
				result.beforeFirst();
				assertThat(result.isBeforeFirst(), is(true));
				result.afterLast();
				assertThat(result.isAfterLast(), is(true));
				result.next();

				assertThat(result.isWrapperFor(SecretResultSet.class), is(true));
				assertThat(result.unwrap(SecretResultSet.class).getCharset(), is(Charset.forName("UTF-8")));
				assertThat(result.unwrap(SecretResultSet.class).getCryptColumnNames(),
						is(Arrays.asList("PRODUCT_NAME")));
			}
			result.close();
		}
	}

	@Test
	@Disabled
	public void testSecretResultSetPerformance01() throws Exception {
		for (var i = 0; i < 30; i++) {
			truncateTable("PRODUCT");
			try (var agent = config.agent()) {
				var startTime = System.currentTimeMillis();
				agent.batch("example/insert_product")
						.paramStream(IntStream.range(1, 100000).mapToObj(count -> new HashMap<String, Object>() {
							{
								put("product_id", count);
								put("product_name", "商品名" + count);
								put("product_kana_name", "ショウヒンメイ" + count);
								put("jan_code", "1234567890123");
								put("product_description", count + "番目の商品");
								put("ins_datetime", "2005-12-12 10:10:10");
								put("upd_datetime", "2005-12-13 10:10:10");
								put("version_no", count);
							}
						})).count();

				var lapTime = System.currentTimeMillis();

				agent.query("example/select_product").stream()
						.forEach(m -> assertThat(m.get("PRODUCT_NAME").toString(), containsString("商品名")));

				var endTime = System.currentTimeMillis();

				System.out.printf("update\t%d\tquery\t%d\ttotal\t%d\r\n", lapTime - startTime, endTime - lapTime,
						endTime - startTime);
			}
		}
	}

}
