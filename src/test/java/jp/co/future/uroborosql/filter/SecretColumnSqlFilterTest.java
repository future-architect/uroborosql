package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

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
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.exception.UroborosqlSQLException;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;
import jp.co.future.uroborosql.utils.StringUtils;

public class SecretColumnSqlFilterTest {

	private SqlConfig config;

	private SqlFilterManager sqlFilterManager;

	private SecretColumnSqlFilter filter;

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
		filter.setTransformationType("AES/ECB/PKCS5Padding");
		sqlFilterManager.initialize();

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

	private List<Map<String, Object>> getDataFromFile(final Path path) {
		List<Map<String, Object>> ans = new ArrayList<>();
		try {
			Files.readAllLines(path, StandardCharsets.UTF_8).forEach(line -> {
				Map<String, Object> row = new LinkedHashMap<>();
				var parts = line.split("\t");
				for (var part : parts) {
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
					fail("TABLE:" + tbl + " truncate is miss. ex:" + ex.getMessage());
				}
			});
		} catch (Exception ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
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
					fail("TABLE:" + map.get("table") + " insert is miss. ex:" + ex.getMessage());
				}
			});

		} catch (Exception ex) {
			ex.printStackTrace();
			fail(ex.getMessage());
		}
	}

	@Test
	void testFilterSettings() {
		assertThat(filter.getCharset(), is(StandardCharsets.UTF_8));
		assertThat(filter.getTransformationType(), is("AES/ECB/PKCS5Padding"));
		assertThat(filter.isSkipFilter(), is(false));
	}

	@Test
	void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// skipFilter = falseの別のフィルター設定
		var skipConfig = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterTest"))
				.build();
		var skipSqlFilterManager = skipConfig.getSqlFilterManager();
		var skipFilter = new SecretColumnSqlFilter();
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
				assertEquals(result.getString("PRODUCT_NAME"), "3EniRr6_Jb2c-kVG0I0CgA");
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
	void testSecretResultSet01() throws Exception {
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
	void testSecretResultSet02() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("example/select_product").param("product_id", new BigDecimal(0));

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
	void testSecretResultSet03() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			var ctx = agent.context().setSqlName("example/select_product").param("product_id", new BigDecimal(0));
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
	void testWithModel() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			var product = new Product();
			product.setProductId(10);
			product.setProductName(Optional.of("商品名１０"));
			product.setVersionNo(1);

			agent.insert(product);

			var result = agent.query(Product.class)
					.equal("productId", new BigDecimal(10))
					.first().orElseThrow(Exception::new);
			assertThat(result.getProductName().isPresent(), is(true));
			assertThat(result.getProductName().orElse(null), is("商品名１０"));
		}
	}

	@Test
	void testSqlInsertOptional() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			agent.update("example/insert_product_for_optional")
					.param("product_id", 10)
					.param("product_name", Optional.of("商品名１０"))
					.param("product_kana_name", Optional.of("ショウヒンメイ１０"))
					.param("jan_code", "1234567890123")
					.param("product_description", "１０番目の商品")
					.param("ins_datetime", new Date())
					.param("upd_datetime", new Date())
					.param("version_no", 1)
					.count();

			var result = agent.query(Product.class)
					.equal("productId", new BigDecimal(10))
					.first().orElseThrow(Exception::new);
			assertThat(result.getProductName().isPresent(), is(true));
			assertThat(result.getProductName().orElse(null), is("商品名１０"));
		}
	}

	@Test
	void testSqlInsertOptionalEmpty() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var agent = config.agent()) {
			agent.update("example/insert_product_for_optional")
					.param("product_id", 10)
					.param("product_name", Optional.empty())
					.param("product_kana_name", Optional.empty())
					.param("jan_code", "1234567890123")
					.param("product_description", "１０番目の商品")
					.param("ins_datetime", new Date())
					.param("upd_datetime", new Date())
					.param("version_no", 1)
					.count();

			var result = agent.query(Product.class)
					.equal("productId", new BigDecimal(10))
					.first().orElseThrow(Exception::new);
			assertThat(result.getProductName().isPresent(), is(false));
			assertThat(result.getProductKanaName().isPresent(), is(false));
		}
	}

	@Table(name = "PRODUCT")
	public static class Product {
		private int productId;
		private Optional<String> productName;
		private Optional<String> productKanaName;
		private Optional<String> janCode;
		private Optional<String> productDescription;
		private Date insDatetime;
		private Date updDatetime;
		@Version
		private int versionNo;

		public Product() {
		}

		public Product(final int productId,
				final Optional<String> productName,
				final Optional<String> productKanaName,
				final Optional<String> janCode,
				final Optional<String> productDescription,
				final Date insDatetime,
				final Date updDatetime,
				final int versionNo) {
			this.productId = productId;
			this.productName = productName;
			this.productKanaName = productKanaName;
			this.janCode = janCode;
			this.productDescription = productDescription;
			this.insDatetime = insDatetime;
			this.updDatetime = updDatetime;
			this.versionNo = versionNo;
		}

		public int getProductId() {
			return productId;
		}

		public void setProductId(final int productId) {
			this.productId = productId;
		}

		public Optional<String> getProductName() {
			return productName;
		}

		public void setProductName(final Optional<String> productName) {
			this.productName = productName;
		}

		public Optional<String> getProductKanaName() {
			return productKanaName;
		}

		public void setProductKanaName(final Optional<String> productKanaName) {
			this.productKanaName = productKanaName;
		}

		public Optional<String> getJanCode() {
			return janCode;
		}

		public void setJanCode(final Optional<String> janCode) {
			this.janCode = janCode;
		}

		public Optional<String> getProductDescription() {
			return productDescription;
		}

		public void setProductDescription(final Optional<String> productDescription) {
			this.productDescription = productDescription;
		}

		public Date getInsDatetime() {
			return insDatetime;
		}

		public void setInsDatetime(final Date insDatetime) {
			this.insDatetime = insDatetime;
		}

		public Date getUpdDatetime() {
			return updDatetime;
		}

		public void setUpdDatetime(final Date updDatetime) {
			this.updDatetime = updDatetime;
		}

		public int getVersionNo() {
			return versionNo;
		}

		public void setVersionNo(final int versionNo) {
			this.versionNo = versionNo;
		}
	}

}
