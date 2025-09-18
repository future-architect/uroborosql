package jp.co.future.uroborosql.event.subscriber;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.sql.ResultSet;
import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.AbstractDbTest;
import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

public class SecretColumnEventSubscriberTest extends AbstractDbTest {
	private SecretColumnEventSubscriber eventSubscriber;

	@BeforeEach
	public void setUpLocal() throws Exception {
		eventSubscriber = new SecretColumnEventSubscriber();

		eventSubscriber.setCryptColumnNames(List.of("PRODUCT_NAME"));
		// 下記コマンドでkeystoreファイル生成
		// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS
		// -alias testexample
		// -storepass password -keypass password -keyalg AES -keysize 128
		eventSubscriber
				.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
		eventSubscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		eventSubscriber.setAlias("testexample");
		eventSubscriber.setCharset("UTF-8");
		eventSubscriber.setTransformationType("AES/ECB/PKCS5Padding");
		config.getEventListenerHolder().addEventSubscriber(eventSubscriber);
	}

	@AfterEach
	public void tearDownLocal() throws Exception {
		config.getEventListenerHolder().removeEventSubscriber(eventSubscriber);
	}

	@Test
	void testFilterSettings() {
		assertThat(eventSubscriber.getCharset(), is(StandardCharsets.UTF_8));
		assertThat(eventSubscriber.getTransformationType(), is("AES/ECB/PKCS5Padding"));
		assertThat(eventSubscriber.isSkip(), is(false));
	}

	@Test
	void testExecuteQueryFilter() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		// skipFilter = falseの別のイベントサブスクライバ設定
		var skipConfig = UroboroSQL
				.builder("jdbc:h2:mem:" + this.getClass().getSimpleName(), "sa", "")
				.build();
		var skipEventSubscriber = new SecretColumnEventSubscriber();

		skipEventSubscriber.setCryptColumnNames(List.of("PRODUCT_NAME"));
		skipEventSubscriber
				.setKeyStoreFilePath("src/test/resources/data/expected/SecretColumnEventSubscriber/keystore.jceks");
		skipEventSubscriber.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
		skipEventSubscriber.setAlias("testexample");
		skipEventSubscriber.setSkip(true);
		skipConfig.getEventListenerHolder().addEventSubscriber(skipEventSubscriber);

		// 復号化しないで取得した場合 (skipFilter = true)
		try (var skipAgent = skipConfig.agent()) {
			var result = skipAgent.query("example/select_product")
					.param("product_id", new BigDecimal(0))
					.resultSet();

			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is("3EniRr6_Jb2c-kVG0I0CgA"));
			}
			result.close();
		}

		// 復号化して取得した場合 (skipFilter = false)
		try (var agent = config.agent()) {
			var result = agent.query("example/select_product")
					.param("product_id", new BigDecimal(0))
					.resultSet();

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

		try (var result = agent.query("example/select_product")
				.param("product_id", new BigDecimal(0))
				.resultSet()) {

			while (result.next()) {
				assertThat(result.getString("PRODUCT_ID"), is("0"));
				assertThat(result.getString("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_KANA_NAME"), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_KANA_NAME", String.class), is("ショウヒンメイゼロ"));
				assertThat(result.getObject("PRODUCT_ID"), is(BigDecimal.ZERO));
				assertThat(result.getObject("PRODUCT_ID", Integer.class), is(0));
			}
		}
	}

	@Test
	void testSecretResultSet02() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		try (var result = agent.query(agent.context().setSqlName("example/select_product")
				.param("product_id", new BigDecimal(0)))) {
			while (result.next()) {
				assertThat(result.getString("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME"), is("商品名0"));
				assertThat(result.getObject("PRODUCT_NAME", String.class), is("商品名0"));
			}
		}
	}

	@Test
	void testSecretResultSet03() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

		var ctx = agent.context().setSqlName("example/select_product")
				.param("product_id", new BigDecimal(0));
		ctx.setResultSetType(ResultSet.TYPE_SCROLL_INSENSITIVE);
		try (var result = agent.query(ctx)) {
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
						is(List.of("PRODUCT_NAME")));
			}
		}
	}

	@Test
	void testWithModel() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

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

	@Test
	void testSqlInsertOptional() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

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

	@Test
	void testSqlInsertOptionalEmpty() throws Exception {
		cleanInsert(Paths.get("src/test/resources/data/setup", "testExecuteQuery.ltsv"));

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
