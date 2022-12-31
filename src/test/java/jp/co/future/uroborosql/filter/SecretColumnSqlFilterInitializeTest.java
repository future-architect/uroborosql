package jp.co.future.uroborosql.filter;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import java.sql.DriverManager;
import java.util.Arrays;

import org.junit.Before;
import org.junit.Test;

import jp.co.future.uroborosql.UroboroSQL;
import jp.co.future.uroborosql.config.SqlConfig;
import jp.co.future.uroborosql.testlog.TestAppender;

public class SecretColumnSqlFilterInitializeTest {
	private SqlConfig config;
	private SqlFilterManager sqlFilterManager;
	private SecretColumnSqlFilter filter;

	@Before
	public void setUp() throws Exception {
		config = UroboroSQL.builder(DriverManager.getConnection("jdbc:h2:mem:SecretColumnSqlFilterInitializeTest"))
				.build();
		sqlFilterManager = config.getSqlFilterManager();
		filter = new SecretColumnSqlFilter();
		sqlFilterManager.addSqlFilter(filter);
	}

	@Test
	public void testInitialize01() throws Exception {
		filter.setCryptColumnNames(null);
		sqlFilterManager.initialize();
		assertThat(filter.isSkipFilter(), is(true));

		filter.setCryptColumnNames(Arrays.asList());
		sqlFilterManager.initialize();
		assertThat(filter.isSkipFilter(), is(true));

		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		filter.setCryptParamKeys(Arrays.asList("product_id", "product_name"));
		sqlFilterManager.initialize();
		assertThat(filter.getCryptParamKeys(), is(Arrays.asList("productId", "productName")));
		assertThat(filter.getCryptColumnNames(), is(Arrays.asList("PRODUCT_ID", "PRODUCT_NAME")));
	}

	@Test
	public void testInitialize02() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList("Invalid KeyStore file path. Path:null")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize03() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilter/fake.jks");
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Not found KeyStore file path. Path:src/test/resources/data/expected/SecretColumnSqlFilter/fake.jks")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize04() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilter");
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid KeyStore file path. Path:src/test/resources/data/expected/SecretColumnSqlFilter")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize05() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilter/keystore.jceks");
			filter.setStorePassword(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList(
				"Invalid password for access KeyStore.")));
		assertThat(filter.isSkipFilter(), is(true));
	}

	@Test
	public void testInitialize06() throws Exception {
		filter.setCryptColumnNames(Arrays.asList("product_id", "product_name"));
		var log = TestAppender.getLogbackLogs(() -> {
			// 下記コマンドでkeystoreファイル生成
			// keytool -genseckey -keystore C:\keystore.jceks -storetype JCEKS -alias testexample
			// -storepass password -keypass password -keyalg AES -keysize 128
			filter.setKeyStoreFilePath(
					"src/test/resources/data/expected/SecretColumnSqlFilter/keystore.jceks");
			filter.setStorePassword("cGFzc3dvcmQ="); // 文字列「password」をBase64で暗号化
			filter.setAlias(null);
			sqlFilterManager.initialize();
		});
		assertThat(log, is(Arrays.asList("No alias for access KeyStore.")));
		assertThat(filter.isSkipFilter(), is(true));
	}
}
